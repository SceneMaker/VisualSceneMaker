package de.dfki.vsm.xtension.mindbotssi;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;

import de.dfki.vsm.util.ActivityLogger;
import de.dfki.vsm.xtension.ssi.SSIRunTimePlugin;
import de.dfki.vsm.xtension.ssi.event.SSIEventArray;
import de.dfki.vsm.xtension.ssi.event.SSIEventEntry;
import de.dfki.vsm.xtension.ssi.event.data.SSIEventData;
import de.dfki.vsm.xtension.ssi.event.data.SSITupleData;

import java.io.IOException;
import java.util.*;


/**
 * @author Fabrizio Nunnari
 */
public class MindBotSSIPlugin extends SSIRunTimePlugin {

    static final String[] emotionNames = new String[]{"neutral",
            "surprise", "happy", "sad",  "disgust", "anger", "fear",
            "valence", "arousal", "dominance"};
    private static final String[] focusTargets = new String[]{"away", "cobot", "table"} ;


    //
    // Will map a variable to the list of received values. Used to compute the average.
    private static Map<String, LinkedList<TimedFloat>> timedHistory;

    private static RunTimeProject PROJECT_REFERENCE;

    private static double thresholdMultilier;

    public static int TIMED_HISTORY_MAX_AGE_MILLIS = 10 * 1000 ;

    public static int TIMED_HISTORY_MIN_SIZE = 5 ;

    static final String[] log_variables = Arrays.stream(emotionNames).map(emotion -> "ssi_emotion_" + emotion + "_avg").toArray(String[]::new);
    ActivityLogger _activity_logger ;

    // Construct SSI plugin
    public MindBotSSIPlugin(
            final PluginConfig config,
            final RunTimeProject project) {
        super(config, project);
        PROJECT_REFERENCE = project;
        thresholdMultilier = Double.parseDouble(config.getProperty("threshold_multiplier","1"));
        mLogger.message("MindSSI plugin constructed...");
    }

    // Launch SSI plugin
    @Override
    public void launch() {
        mLogger.message("Launching MindBotSSI Plugin...");
        super.launch();

        timedHistory = new HashMap<>();
        Arrays.stream(emotionNames).forEach(name-> timedHistory.put(name,new LinkedList<TimedFloat>()));
        Arrays.stream(focusTargets).forEach(name-> timedHistory.put(name,new LinkedList<TimedFloat>()));
        timedHistory.put("fatigue", new LinkedList<TimedFloat>());
        timedHistory.put("pain", new LinkedList<TimedFloat>());

        try {
            _activity_logger = new ActivityLogger("MindBotSSI", mProject) ;
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    // Unload SSI plugin
    @Override
    public void unload() {
        super.unload();
        try {
            if(_activity_logger != null) {
                _activity_logger.close();
            }
            _activity_logger = null ;
        } catch (IOException e) {
            e.printStackTrace();
        }
        mLogger.message("MindBotSSI Plugin unloaded.");
    }


    // Handle SSI event array
    @Override
    public void handle(final SSIEventArray array) {
        //   mLogger.message("Got SSI message array of size " + array.size());

        for (final SSIEventEntry event_entry : array.list()) {
            final SSIEventData data = event_entry.getData();

            // Print information about the received message
            // For example, the event was produced from address "coords@mouse" or "click@mouse"
            mLogger.message(" - sender: " + event_entry.getSender() + // e.g.: "mouse"
                    "\t event: " + event_entry.getEvent() + // e.g.: "coords"
                    "\t from: " + event_entry.getFrom() + // an integer number (?)
                    "\t type: " + event_entry.getType() +  // "MAP" for probability distributions, "TUPLE" for coords, or "EMPTY" for clicks
                    "\t state: " + event_entry.getState() +  // "continued" for streamed coords and clicks down, or "completed" for clicks up.
                    "\t data: " + event_entry.getData());

            String sender = event_entry.getSender();
            String event = event_entry.getEvent();

            if (sender.equals("ecg") && event.equals("stress")) {
                assert event_entry.getType().equals("MAP");
                SSITupleData tupleData = (SSITupleData) data;
                float stressVal = Float.parseFloat(tupleData.get("stress"));
                float neutralVal = Float.parseFloat(tupleData.get("neutral"));
                mLogger.message("Got stress/neutral values: \t" + stressVal + "\t" + neutralVal);

                if (mProject.hasVariable("ssi_stress")) {
                    mProject.setVariable("ssi_stress", stressVal);
                }
                //if(mProject.hasVariable("neutral")) {
                //    mProject.setVariable("neutral", neutralVal);
                //}
            } else if (sender.equals("video") && event.equals("emotion")) {
                assert event_entry.getType().equals("MAP");
                SSITupleData tupleData = (SSITupleData) data;
                mLogger.message("Got emotion+valence+arousal values: \t" + tupleData);

                // HACK! Force the dominance value in the received map
                //float dominance = DominanceCalculator.computeDominanceArgMax(tupleData, true) ;
                float dominance = DominanceCalculator.computeDominanceAccumulated(tupleData) ;
                tupleData.put("dominance", dominance + "");

                // if there is a face detected
                boolean there_is_face = Arrays.stream(emotionNames).
                        mapToDouble(value -> Double.parseDouble(tupleData.get(value)))
                        .anyMatch(value -> value != 0.0) ;

                // For each of the variables expected in this map, compose a corresponding
                // project variable name prepending the "ssi_emotion_" prefix
                Arrays.stream(emotionNames).forEach(emotion -> {
                    float ssiVarValue = Float.parseFloat(tupleData.get(emotion));
                    String projectVarName = "ssi_emotion_" + emotion;
                    if (mProject.hasVariable(projectVarName)) {
                        mProject.setVariable(projectVarName, ssiVarValue);
                    }

                    timedAverageRemoveOldest(emotion);

                    if (there_is_face) {
                        float avgValue = timedMovingAverage(emotion, ssiVarValue);
                        String projectAvgVarName = "ssi_emotion_" + emotion + "_avg";
                        if (mProject.hasVariable(projectAvgVarName)) {
                            mProject.setVariable(projectAvgVarName, avgValue);
                        }

                        try {
                            _activity_logger.logVariables(log_variables);
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                    }
                });

            } else if (sender.equals("video") && event.equals("focus")) {
                assert event_entry.getType().equals("MAP");
                SSITupleData tupleData = (SSITupleData) data;
                mLogger.message("Got video focus values: \t" + tupleData);

                // if there is a face detected
                boolean there_is_face = Arrays.stream(focusTargets).
                        mapToDouble(value -> Double.parseDouble(tupleData.get(value)))
                        .anyMatch(value -> value != 0.0) ;

                // For each of the variables expected in this map, compose a corresponding
                // project variable name prepending the "ssi_focus_" prefix
                for (String ssiVarName : focusTargets) {
                    float ssiVarValue = Float.parseFloat(tupleData.get(ssiVarName));
                    String projectVarName = "ssi_focus_" + ssiVarName;
                    if (mProject.hasVariable(projectVarName)) {
                        mProject.setVariable(projectVarName, ssiVarValue);
                    }

                    timedAverageRemoveOldest(ssiVarName);

                    if (there_is_face) {
                        float avgValue = timedMovingAverage(ssiVarName, ssiVarValue);
                        String projectAvgVarName = "ssi_focus_" + ssiVarName + "_avg";
                        if (mProject.hasVariable(projectAvgVarName)) {
                            mProject.setVariable(projectAvgVarName, avgValue);
                        }
                    }
                }

            } else if (sender.equals("biomech") && event.equals("fatigue")) {
                assert event_entry.getType().equals("MAP");
                SSITupleData tupleData = (SSITupleData) data;
                mLogger.message("Got kinect fatigue value: \t" + tupleData);

                float fatigueValue = Float.parseFloat(tupleData.get("dim#0"));
                String projectVarName = "ssi_fatigue";
                if (mProject.hasVariable(projectVarName)) {
                    mProject.setVariable(projectVarName, fatigueValue);
                }

                // The biomechanical model already computes an average
                float fatigueAvgValue = Float.parseFloat(tupleData.get("dim#1"));
                String projectAvgVarName = "ssi_fatigue_avg";
                if (mProject.hasVariable(projectAvgVarName)) {
                    mProject.setVariable(projectAvgVarName, fatigueAvgValue);
                }

                /*
                 Float avgValue = movingAverage("fatigue",ssiVarValue);
                 String projectAvgVarName = "ssi_fatigue_avg" ;
                 if(mProject.hasVariable(projectAvgVarName)) {
                 mProject.setVariable(projectAvgVarName, avgValue);
                 }
                 */

            } else if (sender.equals("video") && event.equals("pain")) {
                assert event_entry.getType().equals("MAP");
                SSITupleData tupleData = (SSITupleData) data;
                mLogger.message("Got pain data: \t" + tupleData);

                float painValue = Float.parseFloat(tupleData.get("pain"));
                float noPainValue = Float.parseFloat(tupleData.get("no pain")) ;
                boolean there_is_face = painValue != 0.0f || noPainValue != 0.0f ;

                if (mProject.hasVariable("ssi_pain")) {
                    mProject.setVariable("ssi_pain", painValue);
                }

                timedAverageRemoveOldest("pain");

                // if there is a face detected
                if (there_is_face) {
                    // float avgPainValue = movingAverage("pain", painValue);
                    float avgPainValue = timedMovingAverage("pain", painValue) ;
                    if (mProject.hasVariable("ssi_pain_avg")) {
                        mProject.setVariable("ssi_pain_avg", avgPainValue);
                    }
                }

            }

        } // end for(...)

    } // end handle()


    private static void timedAverageRemoveOldest(String name) {
        timedAverageRemoveOldest(timedHistory.get(name));
    }

    private static void timedAverageRemoveOldest(LinkedList<TimedFloat> history) {
        timedAverageRemoveOldest(history, System.currentTimeMillis());
    }

    private static synchronized void timedAverageRemoveOldest(LinkedList<TimedFloat> history, long now) {

        // Remove all "old" elements
        while(history.size() > 0) {
            TimedFloat f = history.getFirst();
            // If the first element is too old, remove it
            if(now - f.t > TIMED_HISTORY_MAX_AGE_MILLIS) {
                history.removeFirst() ;
            } else {
                // if it is young enough, we can stop here.
                break ;
            }
        }

    }

    /** Adds a value to the history of the specified category, and computes the average.
     * Additionally, each added value is associated to a time stamp. After adding a new value, all values
     * older than a given "age" will be removed before computing the average.
     *
     * @param name
     * @param value
     * @return
     */
    private  float timedMovingAverage(String name, float value){

        LinkedList<TimedFloat> history = MindBotSSIPlugin.timedHistory.get(name);
        synchronized(history){
            long now = System.currentTimeMillis();

            // add the new element
            history.addLast(new TimedFloat(now, value));
            // and compute the average
            return (float) history.stream()
                    .mapToDouble(tf -> (double) tf.v)
                    .average().orElse(0.0);
        }

    }


    public static void measureBaselines(){
        for(String name: timedHistory.keySet()){
            measuringBaseline(name);
        }
    }

    private static void measuringBaseline(String name){
        LinkedList<TimedFloat> history = MindBotSSIPlugin.timedHistory.get(name);
        synchronized (history){

            Float mean = (float) history.stream()
                    .mapToDouble(tf -> (double) tf.v)
                    .average().orElse(0.5);
            Double variance = history.stream()
                    .mapToDouble(tf -> Math.pow((double) tf.v - mean,2))
                    .average().orElse(0.5);
            Double std = Math.sqrt(variance);
            if (PROJECT_REFERENCE.hasVariable(name+"_high")) {
                PROJECT_REFERENCE.setVariable(name+"_high",(float) Math.min(0.9, mean + thresholdMultilier*std));
            }
            if (PROJECT_REFERENCE.hasVariable(name+"_low")){
                PROJECT_REFERENCE.setVariable( name+"_low",(float) Math.max(0.1, mean - thresholdMultilier*std));
            }
        }

    }

    private static class TimedFloat {
        /* Value */
        float v;
        /* Timestamp in milliseconds */
        long t;
        TimedFloat(long t, float v) {
            this.t = t ;
            this.v = v ;
        }
        TimedFloat(float v) {
            this(System.currentTimeMillis(), v) ;
        }
    }


    public static int getTimedHistoryMaxAgeMillis() {
        return TIMED_HISTORY_MAX_AGE_MILLIS;
    }

    public static void setTimedHistoryMaxAgeMillis(int timedHistoryMaxAgeMillis) {
        TIMED_HISTORY_MAX_AGE_MILLIS = timedHistoryMaxAgeMillis;
    }

    public static int getTimedHistoryMinSize() {
        return TIMED_HISTORY_MIN_SIZE;
    }

    public static void setTimedHistoryMinSize(int timedHistoryMinSize) {
        TIMED_HISTORY_MIN_SIZE = timedHistoryMinSize;
    }

    public static synchronized boolean isTimedHistoryValid(String name) {
        LinkedList<TimedFloat> h = timedHistory.get(name);

        long now = System.currentTimeMillis() ;
        timedAverageRemoveOldest(h, now);
        assert h.size() == 0 || (now - h.getFirst().t) <= TIMED_HISTORY_MAX_AGE_MILLIS;

        return h.size() >= TIMED_HISTORY_MIN_SIZE ;
    }

    public static boolean isTimedHistoryValid() {
        return isTimedHistoryValid("pain") ;
    }

    public static synchronized void resetTimedHistory() {
        timedHistory.forEach((s, l)->l.clear());
    }


    //
    // Support functions to increase/decrease a variable with fixed steps and clamped to a range
    //

    public static float varStep = 0.2f ;
    public static float varMin = 0.2f;
    public static float varMax = 1.0f;

    public static float decreaseVar(float v) {
        v = v - varStep;
        return v < varMin ? varMin : v > varMax ? varMax : v ;
    }

    public static float increaseVar(float v) {
        v = v + varStep;
        return v < varMin ? varMin : v > varMax ? varMax : v ;
    }

}
