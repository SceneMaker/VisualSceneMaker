package de.dfki.vsm.xtension.mindbotssi;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.interpreter.value.FloatValue;
import de.dfki.vsm.runtime.project.RunTimeProject;

import de.dfki.vsm.util.ActivityLogger;
import de.dfki.vsm.xtension.ssi.SSIRunTimePlugin;
import de.dfki.vsm.xtension.ssi.event.SSIEventArray;
import de.dfki.vsm.xtension.ssi.event.SSIEventEntry;
import de.dfki.vsm.xtension.ssi.event.data.SSIEventData;
import de.dfki.vsm.xtension.ssi.event.data.SSITupleData;

import java.io.IOException;
import java.util.*;


import de.dfki.vsm.xtension.mindbotssi.ThresholdActivationCalculator.ThresholdPair ;

/**
 * @author Fabrizio Nunnari
 */
public class MindBotSSIPlugin extends SSIRunTimePlugin {

    static final String[] emotionNames = new String[]{"neutral",
            "surprise", "happy", "sad",  "disgust", "anger", "fear",
            "valence", "arousal", "dominance"};

    static final String[] ekmanNames = new String[]{"neutral",
            "surprise", "happy", "sad",  "disgust", "anger", "fear"} ;

    private static final String[] focusTargets = new String[]{"away", "cobot", "table"} ;


    // Calibration values 3.0, calibrating on the "Slow task" section after filtering with median on the last 5 frames
    private final static float RMSE_V_LO = -0.09855826509377788f ;
    private final static float RMSE_V_HI = 0.13843901708496958f;
    private final static float RMSE_A_LO = -0.1338607261206554f ;
    private final static float RMSE_A_HI = 0.0715044268674914f ;
    private final static float RMSE_D_LO = -0.020320960538339015f ;
    private final static float RMSE_D_HI = 0.05254249940970949f ;

    private final static String[] VAD_VARIABLES = { "valence", "arousal", "dominance" } ;

    private final static float RAW_VAD_HISTORY_MAX_SIZE_SECS = 15 ;
    private final static float FILTERED_VAD_HISTORY_MAX_SIZE_SECS = 60 ;
    private final static float PAIN_HISTORY_MAX_SIZE_SECS = 60 ;

    private final static float MEDIAN_FILTER_SECS = 1.0f ;

    /** This is the queue for raw VAD values, used to compute the calibration values
     * as well as the short-term denoise filtering.
     */
    private TimedHistory rawVADhistory;

    /** This is the queue for "low-pass" filtered VAD values, used to check for threshold activations. */
    private TimedHistory filteredVADhistory;

    private static int TIMED_HISTORY_MIN_SIZE = 5 ;

    /** Used to check for the activation of thresholds. */
    private ThresholdActivationCalculator VADactivationCalculator;

    /** Will be set by the constructor.
     * Dirty hack to get reference to the running instance from static methods visible to the scene flow.
     */
    private static MindBotSSIPlugin CURRENT_INSTANCE;
    private static RunTimeProject PROJECT_REFERENCE;


    /** Use to keep track of the pain value. */
    private TimedHistory rawPainHistory;
    private TimedHistory filteredPainHistory;
    private ThresholdActivationCalculator painActivationCalculator ;

    /** Compose the list of variables that will be logged. */
    // TODO -- adjust variables visibillity
    static final List<String> log_variables_list = new LinkedList<>() ;
    static {
        Collections.addAll(log_variables_list, Arrays.stream(emotionNames).map(emotion -> "ssi_emotion_" + emotion).toArray(String[]::new)) ;
        Collections.addAll(log_variables_list, Arrays.stream(focusTargets).map(target -> "ssi_focus_" + target).toArray(String[]::new)) ;
        Collections.addAll(log_variables_list, Arrays.stream(VAD_VARIABLES).map(target -> "calibration_" + target).toArray(String[]::new)) ;
        Collections.addAll(log_variables_list, Arrays.stream(VAD_VARIABLES).map(target -> "filtered_" + target).toArray(String[]::new)) ;
        Collections.addAll(log_variables_list, "VAD_threshold_multiplier") ;
        Collections.addAll(log_variables_list,"VAD_activation_code") ;
        Collections.addAll(log_variables_list,"ssi_fatigue", "ssi_fatigue_avg");
        Collections.addAll(log_variables_list,"ssi_pain", "pain_activation_code", "pain_activation_threshold");
        Collections.addAll(log_variables_list, "ssi_face_detected") ;
    }
    static final String[] log_variables = log_variables_list.toArray(new String[]{}) ;

    /** This will write the log of the variable values. */
    ActivityLogger _activity_logger ;


    // Construct SSI plugin
    public MindBotSSIPlugin(
            final PluginConfig config,
            final RunTimeProject project) {

        super(config, project);

        // Dirty trick, to make the project reference available also into static methods.
        PROJECT_REFERENCE = project;
        CURRENT_INSTANCE = this ;

        mLogger.message("MindSSI plugin constructed...");
    }

    // Launch SSI plugin
    @Override
    public void launch() {
        mLogger.message("Launching MindBotSSI Plugin...");
        super.launch();

        //
        // Setup all the stuff needed to filter the signal and compute threshold activations
        rawVADhistory = new TimedHistory(RAW_VAD_HISTORY_MAX_SIZE_SECS) ;
        filteredVADhistory = new TimedHistory(FILTERED_VAD_HISTORY_MAX_SIZE_SECS);

        ThresholdPair[] thresholds = new ThresholdPair[] {
                new ThresholdPair(RMSE_V_LO, RMSE_V_HI),
                new ThresholdPair(RMSE_A_LO, RMSE_A_HI),
                new ThresholdPair(RMSE_D_LO, RMSE_D_HI)
        } ;
        assert thresholds.length == VAD_VARIABLES.length ;

        VADactivationCalculator = new ThresholdActivationCalculator(filteredVADhistory, thresholds) ;

        rawPainHistory = new TimedHistory(PAIN_HISTORY_MAX_SIZE_SECS) ;
        filteredPainHistory = new TimedHistory(PAIN_HISTORY_MAX_SIZE_SECS) ;
        painActivationCalculator = new ThresholdActivationCalculator(filteredPainHistory,
                new ThresholdPair[] {new ThresholdPair(-0.1f, 0.8f)} ) ;

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
        // mLogger.message("Got SSI message array of size " + array.size());

        long now = System.currentTimeMillis() ;


        for (final SSIEventEntry event_entry : array.list()) {
            final SSIEventData data = event_entry.getData();

            // Print information about the received message
            // For example, the event was produced from address "coords@mouse" or "click@mouse"
/*
            mLogger.message(" - sender: " + event_entry.getSender() + // e.g.: "mouse"
                    "\t event: " + event_entry.getEvent() + // e.g.: "coords"
                    "\t from: " + event_entry.getFrom() + // an integer number (?)
                    "\t type: " + event_entry.getType() +  // "MAP" for probability distributions, "TUPLE" for coords, or "EMPTY" for clicks
                    "\t state: " + event_entry.getState() +  // "continued" for streamed coords and clicks down, or "completed" for clicks up.
                    "\t data: " + event_entry.getData());
*/

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
                // mLogger.message("Got emotion+valence+arousal values: \t" + tupleData);

                // if there is a face detected
                boolean there_is_face = Arrays.stream(ekmanNames).
                        mapToDouble(value -> Double.parseDouble(tupleData.get(value)))
                        .anyMatch(value -> value != 0.0) ;

                // HACK! Force the dominance value in the received map
                //float dominance = DominanceCalculator.computeDominanceArgMax(tupleData, true) ;
                float dominance = 0.0f;
                if(there_is_face) {
                    dominance = DominanceCalculator.computeDominanceAccumulated(tupleData);
                }
                tupleData.put("dominance", dominance + "");
                // end HACK. From now on, it is AS IF we received dominance from SSI
                //

                if(mProject.hasVariable("ssi_face_detected")) {
                    mProject.setVariable("ssi_face_detected", there_is_face) ;
                    //System.err.println(there_is_face) ;
                    //System.err.flush();
                }

                if(mProject.hasVariable("history_size")) {
                    int s = rawVADhistory.historySize() ;
                    //int s = timedHistory.get("valence").size();
                    mProject.setVariable("history_size", s) ;
                    //System.err.println(s) ;
                    //System.err.flush();
                }


                // For each of the variables expected in this map, compose a corresponding
                // project variable name prepending the "ssi_emotion_" prefix
                Arrays.stream(emotionNames).forEach(emotion -> {
                    float ssiVarValue = Float.parseFloat(tupleData.get(emotion));
                    String projectVarName = "ssi_emotion_" + emotion;
                    if (mProject.hasVariable(projectVarName)) {
                        mProject.setVariable(projectVarName, ssiVarValue);
                    }

                });

                // Clean the history also if the face is not detected.
                // otherwise the queue will be always "valid", because its size doesn't decrease
                // synch because the scene flow is doing the same through static methods
                synchronized (this) {
                    rawVADhistory.removeOldSamples(now);
                    filteredVADhistory.removeOldSamples(now);
                }

                if (there_is_face) {

                    float[] VAD_values = new float[] {
                            Float.parseFloat(tupleData.get("valence")),
                            Float.parseFloat(tupleData.get("arousal")),
                            Float.parseFloat(tupleData.get("dominance")),
                    } ;


                    //
                    // Low-pass filter for the raw signals
                    TimedHistory filter_buffer ;
                    synchronized (this) {
                        // Append the raw data to the full queue
                        rawVADhistory.appendData(now, VAD_values);
                        // Extract a few recent samples for median computation
                        filter_buffer = rawVADhistory.extractMostRecent(MEDIAN_FILTER_SECS);
                    }
                    // Compute the median over the recent buffer
                    float[] filteredVAD = MedianCalculator.computeMedians(filter_buffer) ;
                    for (int i=0 ; i<VAD_VARIABLES.length ; i++) {
                        if (mProject.hasVariable("filtered_" + VAD_VARIABLES[i])) {
                            mProject.setVariable("filtered_" + VAD_VARIABLES[i], filteredVAD[i]);
                        }
                    }

                    //
                    // Center according to the calibration data
                    //
                    float [] calibrated_VAD = new float[VAD_VARIABLES.length] ;
                    for (int i=0 ; i<VAD_VARIABLES.length ; i++) {
                        if(mProject.hasVariable("calibration_"+VAD_VARIABLES[i])) {
                            float val = ((FloatValue)mProject.getValueOf("calibration_"+VAD_VARIABLES[i])).floatValue() ;
                            calibrated_VAD[i] = filteredVAD[i] - val ;
                        } else {
                            calibrated_VAD[i] = filteredVAD[i] - 0.5f ;
                        }
                    }

                    //
                    // Compute the activation threshold according to the filtered queue
                    //

                    // System.out.println(now + "\t" + filteredVADtimedHistory.historySize());
                    // First, retrieve the acivation multiplier
                    float threshold_mult = 1.0f;
                    if(mProject.hasVariable( "VAD_threshold_multiplier")) {
                        FloatValue fv = (FloatValue)mProject.getValueOf("VAD_threshold_multiplier") ;
                        threshold_mult = fv.floatValue();
                    }

                    String activation_code ;
                    synchronized (this) {
                        // Append the filtered data to the queue
                        filteredVADhistory.appendData(calibrated_VAD);
                        // this following line is also in the synch section, because it references the filtered history.
                        activation_code = VADactivationCalculator.triggersAreActivated(threshold_mult);
                    }

                    if(mProject.hasVariable("VAD_activation_code")) {
                        mProject.setVariable("VAD_activation_code", activation_code) ;
                    }

                }


                // Log all the registered variables
                try {
                    _activity_logger.logVariables(log_variables);
                } catch (IOException e) {
                    e.printStackTrace();
                }


            } else if (sender.equals("video") && event.equals("focus")) {
                assert event_entry.getType().equals("MAP");
                SSITupleData tupleData = (SSITupleData) data;
                // mLogger.message("Got video focus values: \t" + tupleData);

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


            } else if (sender.equals("video") && event.equals("pain")) {
                assert event_entry.getType().equals("MAP");
                SSITupleData tupleData = (SSITupleData) data;
                // mLogger.message("Got pain data: \t" + tupleData);

                float painValue = Float.parseFloat(tupleData.get("pain"));
                float noPainValue = Float.parseFloat(tupleData.get("no pain")) ;
                boolean there_is_face = painValue != 0.0f || noPainValue != 0.0f ;

                if (mProject.hasVariable("ssi_pain")) {
                    mProject.setVariable("ssi_pain", painValue);
                }

                // Clean the history also if the face is not detected.
                // otherwise the queue will be always "valid", because its size doesn't decrease
                // synch because the scene flow is doing the same through static methods
                synchronized (this) {
                    rawPainHistory.removeOldSamples(now);
                    filteredPainHistory.removeOldSamples(now);
                }

                if (there_is_face) {

                    //
                    // Low-pass filter for the raw signals
                    TimedHistory filter_buffer ;
                    synchronized (this) {
                        // Append the raw data to the full queue
                        rawPainHistory.appendData(now, new float[]{painValue});
                        filter_buffer = rawPainHistory.extractMostRecent(MEDIAN_FILTER_SECS);
                    }
                    // Compute the median over the recent buffer
                    float[] filterRes = MedianCalculator.computeMedians(filter_buffer) ;
                    assert filterRes.length == 1 ;
                    float filteredPain = filterRes[0] ;
                    if (mProject.hasVariable("filtered_pain")) {
                        mProject.setVariable("filtered_pain", filteredPain);
                    }

                    //
                    // Compute the activation threshold according to the filtered queue
                    //

                    // Compute the pain activation threshold
                    float pain_threshold_mult = 1.0f;
                    if (mProject.hasVariable("pain_activation_threshold")) {
                        FloatValue fv = (FloatValue) mProject.getValueOf("pain_activation_threshold");
                        pain_threshold_mult = fv.floatValue();
                    }

                    String pain_activation_code;
                    synchronized (this) {
                        filteredPainHistory.appendData(now, new float[]{painValue});
                        pain_activation_code = painActivationCalculator.triggersAreActivated(pain_threshold_mult);
                    }

                    if (mProject.hasVariable("pain_activation_code")) {
                        // System.out.println("Pain threshold="+pain_threshold_mult+"\t code '"+pain_activation_code+"'");
                        mProject.setVariable("pain_activation_code", pain_activation_code);
                    }

                }

            }

        } // end for(...) - iteration on event list

    } // end handle()


    private synchronized void _resetTimedHistory() {
        rawVADhistory.clearDataHistory();
        filteredVADhistory.clearDataHistory();
        rawPainHistory.clearDataHistory();
        filteredPainHistory.clearDataHistory();
    }


    /** Computes the median of the raw variables history queue and set the values on the project variables "calibration_*"
     */
    private synchronized void _measureBaselines(){

        // Compute the median of the values in the threshold activation history
        float[] calibration_medians = MedianCalculator.computeMedians(rawVADhistory) ;

        // There are as many medians as the number of VAD variables
        assert calibration_medians.length == VAD_VARIABLES.length ;

        // Set the proper variables
        for(int i = 0; i < VAD_VARIABLES.length ; i++) {
            String calibration_var = "calibration_" + VAD_VARIABLES[i];
            if(PROJECT_REFERENCE.hasVariable(calibration_var)) {
                PROJECT_REFERENCE.setVariable(calibration_var, calibration_medians[i]) ;
            }
        }

    }

    public synchronized boolean _isTimedHistoryValid() {
        return filteredVADhistory.historySize() >= TIMED_HISTORY_MIN_SIZE ;
    }


    //
    // STATIC methods, visible from the project
    //

    public static int getTimedHistoryMaxAgeMillis() {
        // TODO -- verfy it is used in the project
        //return (int) filteredVADhistory.getTimeWindowSizeMillis();
        return 0 ;
    }

    public static void setTimedHistoryMaxAgeMillis(int timedHistoryMaxAgeMillis) {
        // TODO -- verfy it is used in the project
        //filteredVADhistory.setTimeWindowSizeMillis(timedHistoryMaxAgeMillis);
    }

    public static int getTimedHistoryMinSize() {
        // TODO -- verfy it is used in the project
        return TIMED_HISTORY_MIN_SIZE;
    }

    public static void setTimedHistoryMinSize(int timedHistoryMinSize) {
        // TODO -- verfy it is used in the project
        TIMED_HISTORY_MIN_SIZE = timedHistoryMinSize;
    }

    public static boolean isTimedHistoryValid() {
        return CURRENT_INSTANCE._isTimedHistoryValid();
    }

    public static void resetTimedHistory() {
        CURRENT_INSTANCE._resetTimedHistory();
    }

    public static void measureBaselines() {
        CURRENT_INSTANCE._measureBaselines();
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
