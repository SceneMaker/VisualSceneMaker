package de.dfki.vsm.xtension.mithos;

import com.google.gson.Gson;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.mithos.compint.command.ScenarioScriptCommand;
import de.mithos.compint.command.ScenarioScriptFeedback;
import de.mithos.compint.interaction.ActKind;
import de.mithos.compint.interaction.AppraisalTag;
import de.mithos.compint.interaction.InteractionAct;
import de.mithos.compint.log.VSMPilotLog;
import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.clients.producer.ProducerRecord;
import org.apache.kafka.clients.producer.RecordMetadata;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

/**
 * This plugin uses a kafka server to control an agent-environment and to receive processed userdata.
 * kafka producer implementation based on mithos semvox producer demo
 *
 * @author Manuel Anglet
 */

public class MithosExecutor extends ActivityExecutor {

    private final String server;
    private final String read_topics;

    private final String write_topic;
    private final String interaction_log_topic, message_log_topic;
    private KafkaProducer<String, String> producer;
    MithosHandler handler;
    private final LOGConsoleLogger logger = LOGConsoleLogger.getInstance();
    Gson gson = new Gson();
    private Integer actID = 0;

    private final Map<String, ActivityWorker> activityWorkerMap = new HashMap<>();

    public MithosExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
        server = mConfig.getProperty("server");
        read_topics = mConfig.getProperty("read_topic");
        write_topic = mConfig.getProperty("write_topic");
        if (mConfig.containsKey("interaction_timeline_log_topic")) {
            interaction_log_topic = mConfig.getProperty("interaction_timeline_log_topic");
        } else {
            interaction_log_topic = "InteractionVSMLog";
        }
        if (mConfig.containsKey("interaction_timeline_log_topic")) {
            message_log_topic = mConfig.getProperty("message_log_topic");
        } else {
            message_log_topic = "MessageVSMLog";
        }
    }

    @Override
    public String marker(long id) {
        // Bracket style bookmarks
        return "$(" + id + ")";
    }


    synchronized private int nextId() {
        int next_id = actID + 1;
        actID = next_id;
        return next_id;
    }

    private void sendRecord(ProducerRecord<String, String> record) {
        try {
            RecordMetadata metaData = producer.send(record).get();
            logger.message("sending : " + record.toString());
            logger.message(metaData.toString());
        } catch (Exception e) {
            logger.failure(e.toString());
            logger.failure(e.getCause().toString());
        }
        producer.flush();
    }

    private void sendRecordAndWait(ProducerRecord<String, String> record, Integer id) {
        sendRecord(record);
        synchronized (activityWorkerMap) {
            ActivityWorker aw = (ActivityWorker) Thread.currentThread();
            activityWorkerMap.put(id.toString(), aw);
            logger.message("start waiting on tricat with " + id);
            while (activityWorkerMap.containsValue(aw)) {
                try {
                    activityWorkerMap.wait(1000);
                    logger.message("still waiting on tricat with " + id);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            }
            logger.message("done waiting on tricat with " + id);
        }
    }


    private void executeSpeechActivity(SpeechActivity speechActivity) {
        String actor = speechActivity.getActor();
        final List<String> time_marks = speechActivity.getTimeMarks("$(");
        String aid = mProject.getAgentConfig(actor).getProperty("aid");

        String demarkedText = speechActivity.getText();
        String activityText;
        StringBuffer sb;

        for (String mark : time_marks) {
            activityText = mPlayer.getActivityScheduler().getActivity(mark).getText();
            sb = new StringBuffer(activityText);
            sb.insert(1, actor + " ");
            demarkedText = demarkedText.replace(mark, sb.toString());
            mProject.getRunTimePlayer().getActivityScheduler().handle(mark);
        }
        String key = "speech_command";
        String command = actor + " " + demarkedText;
        Integer id = nextId();
        ScenarioScriptCommand ssc = new ScenarioScriptCommand(id, command);
        String sscGsonString = gson.toJson(ssc);
        ProducerRecord<String, String> record = new ProducerRecord<>(write_topic, 0, "Command", sscGsonString);
        sendRecordAndWait(record, id);
    }

    private void sendLogEntry() {
        String name = (String) mProject.getValueOf("name").getValue();
        Integer interaction_count = (int) mProject.getValueOf("interaction_count").getValue();
        String phase = (String) mProject.getValueOf("phase").getValue();
        Integer relationship_lvl = (int) mProject.getValueOf("relationship_lvl").getValue();
        String affection_interpretation = (String) mProject.getValueOf("affection_interpretation").getValue();
        String freedom_interpretation = (String) mProject.getValueOf("freedom_interpretation").getValue();
        Integer task_lvl = (int) mProject.getValueOf("task_lvl").getValue();
        VSMPilotLog logEntry = new VSMPilotLog(name,interaction_count,phase,relationship_lvl,affection_interpretation,freedom_interpretation,task_lvl);
        String logEntryGsonString = gson.toJson(logEntry);
        ProducerRecord<String, String> record = new ProducerRecord<>(interaction_log_topic, 0, "Log", logEntryGsonString);
        sendRecord(record);
    }

    private void executeActionActivity(ActionActivity actionActivity) {
        if (actionActivity.getContext().equals(ActionActivity.Context.NESTED)) {
            return;
        }
        if (actionActivity.getName().equals("LogInteraction")) {
            actionActivity.setType(AbstractActivity.Type.parallel);
            sendLogEntry();
            return;
        }
        if (actionActivity.getName().equals("LogMsg")) {
            actionActivity.setType(AbstractActivity.Type.parallel);
            String message = actionActivity.get("msg");
            sendLogMsg(message);
            return;
        }

        String text = actionActivity.getText();
        String command = text.substring(1, text.length() - 1);
        command = command.replace('\'', '"');
        Integer id = nextId();
        ScenarioScriptCommand ssc = new ScenarioScriptCommand(id, command);
        String sscGsonString = gson.toJson(ssc);
        ProducerRecord<String, String> record = new ProducerRecord<>(write_topic, 0, "Command", sscGsonString);
        if (actionActivity.getName().equals("SpeakAndAct") || actionActivity.getName().equals("StartSpeaking")) {
            actionActivity.setType(AbstractActivity.Type.blocking);
            sendRecordAndWait(record, id);
        } else {
            actionActivity.setType(AbstractActivity.Type.parallel);
            sendRecord(record);
        }
    }

    private void sendLogMsg(String message) {
        ProducerRecord<String, String> record = new ProducerRecord<>(message_log_topic, 0, "LogMessage", message);
        sendRecord(record);
    }


    @Override
    public void execute(AbstractActivity activity) {
        if (activity instanceof SpeechActivity) {
            executeSpeechActivity((SpeechActivity) activity);
        } else if (activity instanceof ActionActivity) {
            executeActionActivity((ActionActivity) activity);
        }
    }



    @Override
    public void launch() {
        Properties props = new Properties();
        props.put("bootstrap.servers", server);
        props.put("acks", "all");
        props.put("retries", 0);
        props.put("linger.ms", 1);
        props.put("key.serializer", "org.apache.kafka.common.serialization.StringSerializer");
        props.put("value.serializer", "org.apache.kafka.common.serialization.StringSerializer");
        producer = new KafkaProducer<>(props);
        logger.message("Mithos Kafka producer set up");
        handler = new MithosHandler(server, read_topics, this);
        handler.start();
    }

    @Override
    public void unload() {
        //mActivityWorkerMap.notifyAll();
        producer.close();
        handler.abort();
    }


    public void process(ScenarioScriptFeedback ssf) {
        synchronized (activityWorkerMap) {
            activityWorkerMap.remove(Integer.toString(ssf.getId()));
            if (ssf.getFeedback().equals("Failed")) {
                logger.failure("Action " + ssf.getId() + " failed");
            } else if (ssf.getFeedback().equals("Aborted")) {
                logger.failure("Action " + ssf.getId() + " aborted");
            } else {
                logger.message("Action " + ssf.getId() + " succeeded");
            }
            activityWorkerMap.notifyAll();
        }

    }

    public void process(InteractionAct intAct) {
        ActKind intent = intAct.intent;
        if (intent != null) {
            logger.message("Interaction kind: " + intent);
            mProject.setVariable("intent", intent.name());
        }
        AppraisalTag appraisalTag = intAct.appraisalTag;
        if (appraisalTag != null) {
            logger.message("Appraisal Tag : " + appraisalTag);
            mProject.setVariable("appraisalTag", appraisalTag.name());
        }
    }


}