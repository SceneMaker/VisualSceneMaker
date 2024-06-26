package de.dfki.vsm.xtension.mithos;

import com.google.gson.Gson;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
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

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

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

//    private void sendTempIntAct() {
//        ActKind iaIntent = ActKind.ConventionalOpening;
//        String actor = "Teacher";
//        List<String> addressees = new ArrayList<>();
//        Emotion emotion = new Emotion(0,0,0,0,10);
//        InteractionActBuilder iAB = new InteractionActBuilder(iaIntent, actor, addressees, emotion, 0, 10);
//        iAB.addAppraisalTag(GoodActSelf);
//        InteractionAct iA = iAB.getInteractionAct();
//        String iAGsonString = gson.toJson(iA);
//        ProducerRecord<String, String> record = new ProducerRecord<>("InteractionActs", 0, "iA", iAGsonString);
//        sendRecord(record);
//    }

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
            logger.failure("Invalid Dialogue Act: " + DialogueAct);
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
        }

        if (actionActivity.getName().equals("LogStudentDialogueAct")) {
            actionActivity.setType(AbstractActivity.Type.parallel);
            String DialogueAct = actionActivity.get("DA");
            sendDialogueLogEntry(DialogueAct);
            return;
        }

        if (actionActivity.getName().equals("GetConflictResolution")) {
            executeGetConflictResolution();
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
            mProject.setVariable("processAffectToolbox",false );
            sendRecordAndWait(record, id);
            mProject.setVariable("processAffectToolbox",true );
            speakingTimeEnd = System.nanoTime();
        } else {
            actionActivity.setType(AbstractActivity.Type.parallel);
            sendRecord(record);
        }
    }

    //determines given the emotionList the current conflict and conflict resolution style, these are written in corresponding VCM vars
    //also sets the ne relationship level automatically //TODO make rel level change seperate function
    public void executeGetConflictResolution(){
        logger.message("executeGetConflictResolution");
        String emotionListString = (String) mProject.getValueOf("TeacherEmotionList").getValue();
        mProject.setVariable("TeacherEmotionList","" );

        String[] emotions = emotionListString.split(";");
        int count = emotions.length;


        //count emotion appearances, store result in Hashmap
        Map<String, Integer> countMap = new HashMap<String,Integer>();
        for (String emotion : emotions){
            if (countMap.containsKey(emotion)){
                countMap.put(emotion,countMap.get(emotion) +1);
            }else {
                countMap.put(emotion,1);
            }
        }

        //get the 2 most counted emotions
        //TODO finetune
        double only_treashhold = 0.8; //min precentage treashhold at which one emotion is considered to be the only emotion
        double duo_treashhold = 0.2; //min precentage treashhold at which second emotion is considered
        //if both treashold are not met only strongest(most appearing) emotions is considered

        ArrayList<String> nStrongesEmotions = new ArrayList<>();

        //sort emotions by appearance
        List<Map.Entry<String, Integer>> countList = new ArrayList<>(countMap.entrySet());
        countList.sort((entry1, entry2) -> entry2.getValue().compareTo(entry1.getValue()));

        if(countList.size() == 1){
            Map.Entry<String, Integer> mostAppearedEmotiom = countList.get(0);
            double mostAppearedEmotiomPercentage = mostAppearedEmotiom.getValue() / count;
            nStrongesEmotions.add(mostAppearedEmotiom.getKey());
        }
        if(countList.size() > 1){
            Map.Entry<String, Integer> mostAppearedEmotiom = countList.get(0);
            double mostAppearedEmotiomPercentage = (double) mostAppearedEmotiom.getValue() / (double) count;

            Map.Entry<String, Integer> secondMostAppearedEmotiom = countList.get(1);
            double secondMostAppearedEmotiomPercentage = (double) secondMostAppearedEmotiom.getValue() / (double) count;

            if(mostAppearedEmotiomPercentage > only_treashhold){
                nStrongesEmotions.add(mostAppearedEmotiom.getKey());
            } else if (secondMostAppearedEmotiomPercentage > duo_treashhold) {
                nStrongesEmotions.add(mostAppearedEmotiom.getKey());
                nStrongesEmotions.add(secondMostAppearedEmotiom.getKey());
            }else{
                nStrongesEmotions.add(mostAppearedEmotiom.getKey());
            }
        }


        //TESTING CAN BE DELETED
        //nStrongesEmotions = new ArrayList<>();
        //nStrongesEmotions.add("Joy");
        //nStrongesEmotions.add("Fear");

        Collections.sort(nStrongesEmotions);
        //given the two most frequent emotions pick the conflict resolution
        String confRes = "problemsolving";//default value when no match is found
        String conf = "noConflict";
        List<List<String>> csvData = readCSV("plugins/mithos/data/OCCEmotion_to_ConflictResolution.csv", ",");
        for (List<String> row : csvData){
            ArrayList<String> tableVal = new ArrayList<>();
            tableVal.add(row.get(0));
            if(!Objects.equals(row.get(1), "")){
                tableVal.add(row.get(1));
            }
            Collections.sort(tableVal);
            if(tableVal.equals(nStrongesEmotions)){
                confRes = row.get(2);
                conf = row.get(3);
            }

        }
        mProject.setVariable("ConflictResolutionStyle",confRes);
        mProject.setVariable("ConflictType",conf);
        logger.message("confRes " + confRes);

        //fin change to relationshipLvl
        int deltaRelLvl = 0;

        switch (confRes){
            case "smoothing":
                deltaRelLvl = -1;
                break;
            case "withdrawing":
                deltaRelLvl = +1;
                break;
            case "forcing":
                deltaRelLvl = +1;
                break;
            case "problemsolving":
                deltaRelLvl = -1;
                break;
            default:
                logger.failure("This conflict resolution is not defined!");
        }

        //TODO maby make function parameter, to not have name hardcoded
        int relLvl = (int) mProject.getValueOf("rel_lvl_automated_suggestion").getValue();




        int nextRelLevel = relLvl + deltaRelLvl;

        //making shure level stays in range
        int upperBound = 4;
        int lowerBound = -2;
        nextRelLevel = Math.max(lowerBound, Math.min(nextRelLevel, upperBound));

        logger.message("nextRelLevel " + nextRelLevel);
        mProject.setVariable("rel_lvl_automated_suggestion",nextRelLevel);

    }

    @Override
    public void execute(AbstractActivity activity) {
        if (activity instanceof SpeechActivity) {
            executeSpeechActivity((SpeechActivity) activity);
        } else if (activity instanceof ActionActivity) {
            final String name = activity.getName();

            if (name.equalsIgnoreCase("customLog")) {
                mLogger.message("Executing CustomLog");
                sendcustomLog(activity);
                return;
            }

            executeActionActivity((ActionActivity) activity);
        }
    }

    private void sendcustomLog(AbstractActivity activity) {
        LinkedList<ActionFeature> features = activity.getFeatures();


        String msgVar = getActionFeatureValue("msg", features);//var name of intended message
        String msg = (String) mProject.getValueOf(msgVar).getValue().toString();//content of var, intendet message
        String topic = getActionFeatureValue("topic", features);//kafka topic where msg will be written


        if(Objects.nonNull(msg) && Objects.nonNull(msgVar) &&  Objects.nonNull(topic)){
            ProducerRecord<String, String> record = new ProducerRecord<>(topic, 0, msgVar, msg);
            sendRecord(record);
        }
    }

    // get the value of a feature (added PG) - quick and dirty
    private final String getActionFeatureValue(String name, LinkedList<ActionFeature> features) {
        for (ActionFeature af : features) {
            if (af.getKey().equalsIgnoreCase(name)) {
                return af.getVal();
            }
        }
        return "";
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

    //processes Interaction Act send by Semvox
    public void process(InteractionAct intAct) {
        ActKind intent = intAct.intent;
        if (intent != null) {
            logger.message("Interaction kind: " + intent);
            mProject.setVariable("intent", intent.name());
        }

        if (intAct.phase != null) {
            String ph = "P" + intAct.phase.toString();
            logger.message("Detected Interaction Phase is: " + ph);
            mProject.setVariable("phase", ph);
        }

        /*AppraisalTag appraisalTag = intAct.appraisalTag;
        if (appraisalTag != null) {
            logger.message("Appraisal Tag : " + appraisalTag);
            String appraisalList = (String) mProject.getValueOf("appraisalTag").getValue();
            appraisalList = appraisalList + appraisalTag.name() + ";";
            mProject.setVariable("appraisalTag", appraisalList);
        }*/

        int taskLvl = (int) mProject.getValueOf("task_lvl_automated_suggestion").getValue();

        if(intAct.taskFocus){
            taskLvl--;
        }else {
            taskLvl++;
        }

        int upperBound = 4;
        int lowerBound = -2;

        taskLvl  = Math.max(lowerBound, Math.min(taskLvl, upperBound));

        logger.message("taskLvl: " + taskLvl);


        String conflictType = (String) mProject.getValueOf("ConflictType").getValue();//TODO CHECK POSSIBLE ISSUE WITH CONFLICT NOT BEING UP TO DATE
        String appraisalList = processSocialNorms(conflictType,intAct.socialNorms);

        mProject.setVariable("appraisalTagsSemvox",appraisalList);


        mProject.setVariable("task_lvl_automated_suggestion",taskLvl);
        mProject.setVariable("new_interpretation",true);

//        TODO: Check logic
        int interaction_count = (int) mProject.getValueOf("interaction_count").getValue();
        interaction_count = interaction_count + 1;
        mProject.setVariable("interaction_count",interaction_count);
    }

    private String processSocialNorms(String conflictType, List<SocialNorm> socialNorms){
        //TODO DELTE
        logger.message("processSocialNorms:\n ConflictType = " + conflictType + "\n socialNorms: " + socialNorms.toString());


        Map<SocialNorm, Integer> priorityMap = getPriorityMap(conflictType,socialNorms);
        socialNorms = orderByPriorityMap(socialNorms,priorityMap);
        //socialNorms = addSaliencyToSocialNorms(socialNorms,priorityMap);
        socialNorms = addSaliencyToSocialNorms(socialNorms);

        logger.message("social Norm Priority List: " + socialNorms.toString());

        String appraisalList = "";

        for (SocialNorm norm : socialNorms){
            appraisalList = appraisalList + norm.getAppraisalTag() + "," + norm.getSaliency() + ";";
        }

        return appraisalList;
    }

    private List<SocialNorm> orderByPriorityMap(List<SocialNorm> socialNorms, Map<SocialNorm, Integer> priorityMap) {
        //sorting
        //sort emotions by appearance
        List<Map.Entry<SocialNorm, Integer>> prioList = new ArrayList<>(priorityMap.entrySet());
        prioList.sort((entry1, entry2) -> entry1.getValue().compareTo(entry2.getValue()));

        //TODO DELTE
        logger.message("resulting order, given social norms:");
        //extract lsit
        List<SocialNorm> retList = new ArrayList<SocialNorm>();
        for (Map.Entry<SocialNorm, Integer> entry : prioList){
            //TODO DELTE
            logger.message(entry.getKey().getName());

            retList.add(entry.getKey());
        }

        return retList;
    }

    //given a list of social norms, give a priority Map according to config
    private Map<SocialNorm, Integer> getPriorityMap(String conflict, List<SocialNorm> socialNorms){
        List<List<String>> csvData = readCSV("plugins/mithos/data/Conflict_to_socialNormOrder.csv", ",");

        //grete priority list of all norms according to config file
        Map<String, Integer> priorityMapForConflict = new HashMap<String,Integer>();

        //TODO DELTE
        logger.message("Preocess Social Norms with conflict style " + conflict);

        //is the ordering by Whom the SN refer to(true) or by a direct ordering of social norms(false)
        boolean orderByReference = false;

        //TODO DELTE
        logger.message("Determined ordering is");
        for (List<String> row : csvData){
            String rowConflict = row.get(0);
            //find conflict of which the ordering should be considered and create a map from norm to priority
            if(Objects.equals(rowConflict, conflict)){

                //In this case there is no specific ordering of norms but to whom the norms belong
                if (row.get(1) == "s>t"){
                    //TODO DELTE
                    logger.message("s>t");
                    orderByReference = true;
                    break;
                }

                int priority = 1;
                for(int i = 1; i < row.size(); i = i+2){
                    String norm = row.get(i);
                    //TODO DELTE
                    logger.message(norm);
                    String ref = row.get(i+1);//to whom the norm applies in regards to the config file t = techer, s = student, b = both

                    priorityMapForConflict.put(norm,priority);
                    priority++;

                }
                break;
            }
        }

        //assign priorities to given list
        Map<SocialNorm, Integer> priorityMap = new HashMap<SocialNorm,Integer>();

        //order by reference, so social norms student > social norms teacher
        if(orderByReference){
            for(SocialNorm norm : socialNorms){
                Boolean normIsStudent = getSocialNormReft(norm.getName());
                if (normIsStudent) {
                    priorityMap.put(norm, 1);
                }else{
                    priorityMap.put(norm, 2);
                }
            }
        }else{
            for(SocialNorm norm : socialNorms){
                String name = getSocialNormName(norm.getName());//map from semvox id to social norm
                if(priorityMapForConflict.containsKey(name)){
                    priorityMap.put(norm, priorityMapForConflict.get(name));
                }else{
                    priorityMap.put(norm, 1000);//NOTE this line assumes that there are no more than 1000 social norms(should be a correct assumption)
                }
            }
        }

        return priorityMap;
    }

    private Boolean getSocialNormReft(String name) {
        List<List<String>> csvData = readCSV("plugins/mithos/data/SemvoxID_SocialNorm.csv", ",");

        for (List<String> row : csvData){
            if (row.get(0) == name){
                if (row.get(2) == "s") {
                    return true;
                }else if (row.get(2) == "t"){
                    return false;
                }else {
                    logger.failure("NO matching social norm found for semvox social norm name");
                    return false;
                }
            }
        }
        logger.failure("NO matching social norm found for semvox social norm name");
        return false;
    }

    private String getSocialNormName(String name) {
        List<List<String>> csvData = readCSV("plugins/mithos/data/SemvoxID_SocialNorm.csv", ",");

        for (List<String> row : csvData){
            if (Objects.equals(row.get(0), name)){
                return row.get(1);
            }
        }
        logger.warning("NO matching social norm found for semvox social norm name");
        return "";
    }

    //given a list of social norms ORDERED from least to most important
    //assign saliency linearly from 1 to 0.3 in equidistant order
    private List<SocialNorm> addSaliencyToSocialNorms(List<SocialNorm> socialNorms, Map<SocialNorm, Integer> priorityMap){
        double maxSaliency = 1;
        double minSaliency = 0.3;

        if (socialNorms.size() == 0){
            return socialNorms;
        }
        if (socialNorms.size() == 1){
            socialNorms.get(0).setSaliency(1);
            return socialNorms;
        }

        //determine number of individual priorities
        int n = new HashSet<>(priorityMap.values()).size();//TODO TO MAKIE IT WORK ADJUST FOR NUMBER OF PRIORITIES AND NOT NUMBER OF SALIENCIES

        //diffrence in saliancy
        double delta = (maxSaliency-minSaliency)/(n-1);

        double cur_saliancy = 1.0;
        int cur_priority = priorityMap.get(socialNorms.get(0));

        for (int i = 0; i < n; i++){
            SocialNorm socialNorm = socialNorms.get(i);
            if(!priorityMap.containsKey(socialNorm)){
                logger.failure("MithosExecutor addSaliencyToSocialNorms: NO priority was assigned to social norm!");
            }else{
                if(priorityMap.get(socialNorm) == cur_priority){
                    socialNorm.setSaliency(cur_saliancy);
                }else{
                    cur_priority = priorityMap.get(socialNorm);
                    cur_saliancy -= delta;
                    socialNorm.setSaliency(cur_saliancy);
                }
            }
        }
        return socialNorms;
    }

    //given a list of social norms ordered from least to most important and a prioritz mapping
    //assign saliency linearly from 1 to 0.3 in steps of 0.1, when more then 7 stay at 0.3
    private List<SocialNorm> addSaliencyToSocialNorms(List<SocialNorm> socialNorms){
        double maxSaliency = 1;
        double minSaliency = 0.3;

        if (socialNorms.size() == 0){
            return socialNorms;
        }
        if (socialNorms.size() == 1){
            socialNorms.get(0).setSaliency(1);
            return socialNorms;
        }

        int n = socialNorms.size();

        //diffrence in saliancy
        double delta = 0.1;

        double cur_saliancy = 1.0;

        for (int i = 0; i < n; i++){
            socialNorms.get(i).setSaliency(cur_saliancy);
            if (cur_saliancy >= 0.3){
                cur_saliancy -= delta;
            }
        }

        return socialNorms;
    }

    //Add affectToolBox PAD input to VCM variable
    //Ignore zero unout
    public void process(Emotion emotion) {
        double[] padValues = {emotion.getValence(), emotion.getArousal(), emotion.getDominance()};
        if(padValues[0] == 0.0 && padValues[1] == 0.0  && padValues[2] == 0.0 ){
            return;
        }
        logger.message("PAD");

        String padList = (String) mProject.getValueOf("padAffectToolBox").getValue();

        String padString = padValues[0] + "," + padValues[1] + "," + padValues[2];
        String padAffectList = padList + padString + ";";
        mProject.setVariable("padAffectToolBox", padAffectList);
    }


    //used by getConflict resoluton
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
            logger.message(toPrint);
        }
        return csvData;
    }

}