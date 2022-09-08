package de.dfki.vsm.xtension.mithos;

import com.google.gson.Gson;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.mithos.compint.command.Feedback;
import de.mithos.compint.command.ScenarioScriptCommand;
import de.mithos.compint.command.ScenarioScriptFeedback;
import de.mithos.compint.interaction.ActKind;
import de.mithos.compint.interaction.InteractionAct;
import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.clients.producer.ProducerRecord;

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
    KafkaProducer<String, String> producer;
    MithosHandler handler;
    private final LOGConsoleLogger logger = LOGConsoleLogger.getInstance();
    Gson gson = new Gson();
    private long actID = 0;

    private final Map<String, ActivityWorker> activityWorkerMap = new HashMap<>();

    public MithosExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
        System.out.println("Mithos Kafka starting");
        server = mConfig.getProperty("server");
        read_topics = mConfig.getProperty("read_topic");
        write_topic = mConfig.getProperty("write_topic");
    }

    @Override
    public String marker(long id) {
        // Bracket style bookmarks
        return "$(" + id + ")";
    }

    @Override
    public void execute(AbstractActivity activity) {
        try {
            System.out.println("Mithos Kafka action to be executed");
            String actor = activity.getActor();
            String command, key;

            if (activity instanceof SpeechActivity) {
                final SpeechActivity speech_activity = (SpeechActivity) activity;
                final List<String> time_marks = speech_activity.getTimeMarks("$(");
                String aid = mProject.getAgentConfig(actor).getProperty("aid");

                String demarkedText = speech_activity.getText();
                String activityText;
                StringBuffer sb;

                for (String mark : time_marks) {
                    activityText = mPlayer.getActivityScheduler().getActivity(mark).getText();
                    sb = new StringBuffer(activityText);
                    sb.insert(1, actor + " ");
                    demarkedText = demarkedText.replace(mark, sb.toString());
                    mProject.getRunTimePlayer().getActivityScheduler().handle(mark);
                }
                key = "speech_command";
                command = actor + " " + demarkedText;
            } else {
                key = "command";
                String text = activity.getText();
                command = text.substring(1, text.length() - 1);
            }
            String actionIDString = Long.toString(actID++);
            ScenarioScriptCommand ssc = new ScenarioScriptCommand("SSC", "VSM", actor, command, actionIDString);
            String sscGsonString = gson.toJson(ssc);
            ProducerRecord<String, String> record = new ProducerRecord<>(write_topic, key, sscGsonString);
            producer.send(record);
            synchronized (activityWorkerMap) {
                System.out.println("inside the syncblock");
                // organize wait for feedback if (activity instanceof SpeechActivity) {
                ActivityWorker aw = (ActivityWorker) Thread.currentThread();
                activityWorkerMap.put(actionIDString, aw);

                if (activity.getType() == AbstractActivity.Type.blocking) {
                    while (activityWorkerMap.containsValue(aw)) {
                        activityWorkerMap.wait();
                    }
                }
            }
        } catch (InterruptedException exc) {
            System.out.println(exc.toString());
            logger.failure(exc.toString());
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
        System.out.println("Mithos Kafka producer set up");

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
            if (ssf.getFeedback().equals(Feedback.SUCCESS)) {
                activityWorkerMap.remove(ssf.getuID());
            } else if (ssf.getFeedback().equals(Feedback.FAILIURE)) {
                activityWorkerMap.remove(ssf.getuID());
                logger.failure("Action " + ssf.getuID() + " failed");
            }
            activityWorkerMap.notifyAll();
        }
    }

    public void process(InteractionAct intAct) {
        ActKind kindDa = intAct.kind_da;
        if (kindDa != null) {
            logger.message("Interaction kind: " + kindDa);
            mProject.setVariable("kind_da", kindDa.toString());
        }
    }
}