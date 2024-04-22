package de.dfki.vsm.xtension.mithos;

import com.google.gson.Gson;
import de.dfki.vsm.event.EventDispatcher;
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
import de.mithos.compint.interaction.*;
import de.mithos.compint.log.VSMPilotLog;
import org.apache.kafka.clients.producer.KafkaProducer;
import org.apache.kafka.clients.producer.ProducerRecord;
import org.apache.kafka.clients.producer.RecordMetadata;
import org.apache.kafka.common.protocol.types.Field;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

import static de.mithos.compint.interaction.AppraisalTag.*;

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
    private final String interaction_log_topic, student_dialogue_act_log_topic, message_log_topic;
    private KafkaProducer<String, String> producer;
    MithosHandler handler;
    private final LOGConsoleLogger logger = LOGConsoleLogger.getInstance();
    Gson gson = new Gson();
    private Integer actID = 0;

    private final Map<String, ActivityWorker> activityWorkerMap = new HashMap<>();
    private long speakingTimeBegin;
    private long speakingTimeEnd;

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

        student_dialogue_act_log_topic = "StudentDialogueActVSMLog";
    }

    private void sendTempIntAct() {
        ActKind iaIntent = ActKind.ConventionalOpening;
        String actor = "Teacher";
        List<String> addressees = new ArrayList<>();
        Emotion emotion = new Emotion(0,0,0,0,10);
        InteractionActBuilder iAB = new InteractionActBuilder(iaIntent, actor, addressees, emotion, 0, 10);
        iAB.addAppraisalTag(GoodActSelf);
        InteractionAct iA = iAB.getInteractionAct();
        String iAGsonString = gson.toJson(iA);
        ProducerRecord<String, String> record = new ProducerRecord<>("InteractionActs", 0, "iA", iAGsonString);
        sendRecord(record);
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

    private void sendInteractionLogEntry() {
        String name = (String) mProject.getValueOf("name").getValue();
        Integer interaction_count = (int) mProject.getValueOf("interaction_count").getValue();
        String phase = (String) mProject.getValueOf("phase").getValue();
        Integer relationship_lvl = (int) mProject.getValueOf("relationship_lvl").getValue();
        String affection_interpretation = (String) mProject.getValueOf("affection_interpretation").getValue();
        String freedom_interpretation = (String) mProject.getValueOf("freedom_interpretation").getValue();
        Integer task_lvl = (int) mProject.getValueOf("task_lvl").getValue();
        VSMPilotLog logEntry = new VSMPilotLog(name, interaction_count, phase, relationship_lvl, affection_interpretation, freedom_interpretation, task_lvl);
        String logEntryGsonString = gson.toJson(logEntry);
        ProducerRecord<String, String> record = new ProducerRecord<>(interaction_log_topic, 0, "Log", logEntryGsonString);
        sendRecord(record);
    }

    private void sendLogMsg(String message) {
        ProducerRecord<String, String> record = new ProducerRecord<>(message_log_topic, 0, "LogMessage", message);
        sendRecord(record);
    }

    private void sendDialogueLogEntry(String DialogueAct) {
        //sendTempIntAct();
        String actor = "Student_Alex";

        try {
            ActKind intent = ActKind.valueOf(DialogueAct);
            List<String> addressees = new ArrayList<>();
            addressees.add("Teacher");
            Emotion nullEmotion = new Emotion(0, 0, 0, speakingTimeBegin, speakingTimeEnd);
            InteractionAct studentIntAct = new InteractionAct(intent, actor, addressees, nullEmotion, speakingTimeBegin,
                    speakingTimeEnd);
            String studentIntActString = gson.toJson(studentIntAct);
            ProducerRecord<String, String> record = new ProducerRecord<>(student_dialogue_act_log_topic, 0,
                    "LogStudentDialogueAct", studentIntActString);
            sendRecord(record);
        } catch (IllegalArgumentException e) {
            // Handle the case where the provided color argument is not valid
            mLogger.failure("Invalid Dialogue Act: " + DialogueAct);
        }
    }

    private void executeActionActivity(ActionActivity actionActivity) {
        if (actionActivity.getContext().equals(ActionActivity.Context.NESTED)) {
            return;
        }
        if (actionActivity.getName().equals("LogInteraction")) {
            actionActivity.setType(AbstractActivity.Type.parallel);
            sendInteractionLogEntry();
            return;
        }
        if (actionActivity.getName().equals("LogMsg")) {
            actionActivity.setType(AbstractActivity.Type.parallel);
            String message = actionActivity.get("msg");
            sendLogMsg(message);
            return;
        }
        if (actionActivity.getName().equals("LogStudentDialogueAct")) {
            actionActivity.setType(AbstractActivity.Type.parallel);
            String DialogueAct = actionActivity.get("DA");
            sendDialogueLogEntry(DialogueAct);
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
            speakingTimeBegin = System.nanoTime();
            sendRecordAndWait(record, id);
            speakingTimeEnd = System.nanoTime();
        } else {
            actionActivity.setType(AbstractActivity.Type.parallel);
            sendRecord(record);
        }
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


    public void process(Emotion emotion) {
        //finding the closes emotion with cosin similarity
        String cosestEmotion = "UNDEFINED";

        List<List<String>> padToEm = readCSV("plugins/mithos/data/PAD_to_emotion.csv", ",");

        double[] padValues = {emotion.getValence(), emotion.getArousal(), emotion.getDominance()};

        //TODO DELETE for testing purpose
        //padValues = new double[]{0.6, 0.45, 0.45};// results in pride not gratification
        //padValues = new double[]{-0.51,0.59,0.25};// results in pride not gratification
        double max_similarity = -1.0;

        for (List<String> line : padToEm) {

            double[] tableValues = {new Double(line.get(1)), new Double(line.get(2)), new Double(line.get(3))};
            double cosineSimilarity = cosineSimilarity(tableValues, padValues);
            //mLogger.message("calculate CosSim: in line: " + line.get(1) + " " + line.get(2) + " " + line.get(3) + " inCAll" + padValues[0] + " " + padValues[1] + " " + padValues[2] + " out: " + cosineSimilarity);

            if(cosineSimilarity >= max_similarity){
                max_similarity = cosineSimilarity;
                cosestEmotion = line.get(0);
            }
        }


        //fiding the Appraisal tag
        List<List<String>> emToApp = readCSV("plugins/mithos/data/Emotion_to_appraisaltag.csv", ",");
        String appraisalTags = "UNDEFINED";
        for (List<String> line : emToApp) {
            if(line.get(1).equals(cosestEmotion)){
                appraisalTags = line.get(0);
                break;
            }
        }

        if (appraisalTags == "UNDEFINED") {
            mLogger.warning("WARNING no matching AppraisalTag found for Emotion " + cosestEmotion);
            return;
        }
        mLogger.message("For PAD: " + padValues.toString() + " closes emotion is: " + cosestEmotion + " resulting in APPRAISAL TAG: " + appraisalTags);

        String[] appraisalTagList = appraisalTags.replace(" " ,  "").split("\\+");

        String affectList = (String) mProject.getValueOf("affectList").getValue();
        for (String tag : appraisalTagList) {
            //mProject.setVariable("")
            if (!(tag.equals(""))) {
                affectList = affectList  + tag + ";";
            }
        }

        mProject.setVariable("affectList", affectList);
    }


    //function used to read the csv files containing the tables for the PAD to eomotion and Appraisal tag to emotion mapping
    private List<List<String>> readCSV(String csvFile, String csvSplitBy){
        String line = "";
        List<List<String>> csvData = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {
            while ((line = br.readLine()) != null) {
                // Split the line by comma
                List<String> data = new ArrayList<String>();
                String[] array = line.split(csvSplitBy);

                for (String s:array) {
                    data.add(s);
                }

                csvData.add(data);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }


        //enable logging of reading
        boolean print = false;
        if(print){
            String toPrint = "|";
            for (List<String> csvLine : csvData) {


                for (String s : csvLine) {
                    toPrint = toPrint + s + " | ";

                }
                toPrint = toPrint + "\n";
            }
            mLogger.message(toPrint);
        }


        return csvData;
    }

    // Function to calculate cosine similarity
    private double cosineSimilarity(double[] vectorA, double[] vectorB) {
        double dotProduct = 0;
        double magnitudeA = 0;
        double magnitudeB = 0;

        for (int i = 0; i < vectorA.length; i++) {
            dotProduct += vectorA[i] * vectorB[i];            magnitudeA += Math.pow(vectorA[i], 2);
            magnitudeB += Math.pow(vectorB[i], 2);
        }

        magnitudeA = Math.sqrt(magnitudeA);
        magnitudeB = Math.sqrt(magnitudeB);

        if (magnitudeA == 0 || magnitudeB == 0) {
            return 0; // to handle division by zero
        }

        return dotProduct / (magnitudeA * magnitudeB);
}
}