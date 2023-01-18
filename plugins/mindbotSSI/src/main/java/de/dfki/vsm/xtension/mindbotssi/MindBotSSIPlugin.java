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

    private final static float RAW_VAD_HISTORY_SIZE_SECS = 15 ;
    private final static float FILTERED_VAD_HISTORY_SIZE_SECS = 60 ;

    private final static float MEDIAN_FILTER_SECS = 1.0f ;

    /** This is the queue for raw VAD values, used to compute the calibration values
     * as well as the short-term denoise filtering.
     */
    private static TimedHistory rawVADtimedHistory ;

    /** This is the queue for "low-pass" filtered VAD values, used to check for threshold activations. */
    private static TimedHistory filteredVADtimedHistory ;

    private static int TIMED_HISTORY_MIN_SIZE = 5 ;

    /** Used to check for the activation of thresholds. */
    private ThresholdActivationCalculator activationCalculator ;

    private static RunTimeProject PROJECT_REFERENCE;


    // static final String[] log_variables_old = Arrays.stream(emotionNames).map(emotion -> "ssi_emotion_" + emotion + "_avg").toArray(String[]::new);
    static final List<String> log_variables_list = new LinkedList<>() ;
    static {
        // Collections.addAll(log_variables_list, Arrays.stream(emotionNames).map(emotion -> "ssi_emotion_" + emotion + "_avg").toArray(String[]::new)) ;
        Collections.addAll(log_variables_list, Arrays.stream(emotionNames).map(emotion -> "ssi_emotion_" + emotion).toArray(String[]::new)) ;
        // Collections.addAll(log_variables_list, Arrays.stream(focusTargets).map(target -> "ssi_focus_" + target + "_avg").toArray(String[]::new)) ;
        Collections.addAll(log_variables_list, Arrays.stream(focusTargets).map(target -> "ssi_focus_" + target).toArray(String[]::new)) ;
        Collections.addAll(log_variables_list, Arrays.stream(VAD_VARIABLES).map(target -> "calibration_" + target).toArray(String[]::new)) ;
        Collections.addAll(log_variables_list, Arrays.stream(VAD_VARIABLES).map(target -> "filtered_" + target).toArray(String[]::new)) ;
        Collections.addAll(log_variables_list, "threshold_multiplier") ;
        Collections.addAll(log_variables_list,"VAD_activation_code") ;
        Collections.addAll(log_variables_list, "ssi_face_detected") ;
    }
    static final String[] log_variables = log_variables_list.toArray(new String[]{}) ;
    ActivityLogger _activity_logger ;



    // Construct SSI plugin
    public MindBotSSIPlugin(
            final PluginConfig config,
            final RunTimeProject project) {

        super(config, project);

        // Dirty trick, to make the project reference available also into static methods.
        PROJECT_REFERENCE = project;

        mLogger.message("MindSSI plugin constructed...");
    }

    // Launch SSI plugin
    @Override
    public void launch() {
        mLogger.message("Launching MindBotSSI Plugin...");
        super.launch();

        //
        // Setup all the stuff needed to filter the signal and compute threshold activations
        rawVADtimedHistory = new TimedHistory(RAW_VAD_HISTORY_SIZE_SECS) ;
        filteredVADtimedHistory = new TimedHistory(FILTERED_VAD_HISTORY_SIZE_SECS);

        ThresholdPair[] thresholds = new ThresholdPair[] {
                new ThresholdPair(RMSE_V_LO, RMSE_V_HI),
                new ThresholdPair(RMSE_A_LO, RMSE_A_HI),
                new ThresholdPair(RMSE_D_LO, RMSE_D_HI)
        } ;
        assert thresholds.length == VAD_VARIABLES.length ;

        activationCalculator = new ThresholdActivationCalculator(filteredVADtimedHistory, thresholds) ;

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
                    int s = rawVADtimedHistory.historySize() ;
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
                long now = System.currentTimeMillis() ;
                rawVADtimedHistory.removeOldSamples(now);
                filteredVADtimedHistory.removeOldSamples(now);

                if (there_is_face) {

                    float[] VAD_values = new float[] {
                            Float.parseFloat(tupleData.get("valence")),
                            Float.parseFloat(tupleData.get("arousal")),
                            Float.parseFloat(tupleData.get("dominance")),
                    } ;


                    // Append the raw data to the full queue
                    rawVADtimedHistory.appendData(now, VAD_values);
                    // Extract a few recent samples for median computation
                    TimedHistory filter_buffer = rawVADtimedHistory.extractMostRecent(MEDIAN_FILTER_SECS) ;
                    // Compute the median over the recent buffer
                    float[] filteredVAD = MedianCalculator.computeMedians(filter_buffer) ;
                    for (int i=0 ; i<VAD_VARIABLES.length ; i++) {
                        if (mProject.hasVariable("filtered_" + VAD_VARIABLES[i])) {
                            mProject.setVariable("filtered_" + VAD_VARIABLES[i], filteredVAD[i]);
                        }
                    }

                    //
                    // Calibrate according to the calibration data
                    //
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

                    // Append the filtered data to the queue
                    filteredVADtimedHistory.appendData(calibrated_VAD);
                    // System.out.println(now + "\t" + filteredVADtimedHistory.historySize());
                    // check for activations in the filtered queue
                    float threshold_mult = 1.0f;
                    if(mProject.hasVariable( "threshold_multiplier")) {
                        FloatValue fv = (FloatValue)mProject.getValueOf("threshold_multiplier") ;
                        threshold_mult = fv.floatValue();
                    }
                    String activation_code = activationCalculator.triggersAreActivated(threshold_mult);

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
                mLogger.message("Got pain data: \t" + tupleData);

                float painValue = Float.parseFloat(tupleData.get("pain"));
                float noPainValue = Float.parseFloat(tupleData.get("no pain")) ;
                boolean there_is_face = painValue != 0.0f || noPainValue != 0.0f ;

                if (mProject.hasVariable("ssi_pain")) {
                    mProject.setVariable("ssi_pain", painValue);
                }

            }

        } // end for(...)

    } // end handle()




    public static void measureBaselines(){

        // Compute the median of the values in the threshold activation history
        float[] calibration_medians = MedianCalculator.computeMedians(rawVADtimedHistory) ;

        // Set the proper variables
        for(int i = 0; i < VAD_VARIABLES.length ; i++) {
            String calibration_var = "calibration_" + VAD_VARIABLES[i];
            if(PROJECT_REFERENCE.hasVariable(calibration_var)) {
                PROJECT_REFERENCE.setVariable(calibration_var, calibration_medians[i]) ;
            }
        }

    }


    public static int getTimedHistoryMaxAgeMillis() {
        return (int)filteredVADtimedHistory.getTimeWindowSizeMillis();
    }

    public static void setTimedHistoryMaxAgeMillis(int timedHistoryMaxAgeMillis) {
        filteredVADtimedHistory.setTimeWindowSizeMillis(timedHistoryMaxAgeMillis);
    }

    public static int getTimedHistoryMinSize() {
        return TIMED_HISTORY_MIN_SIZE;
    }

    public static void setTimedHistoryMinSize(int timedHistoryMinSize) {
        TIMED_HISTORY_MIN_SIZE = timedHistoryMinSize;
    }

    public static synchronized boolean isTimedHistoryValid() {
        return filteredVADtimedHistory.historySize() >= TIMED_HISTORY_MIN_SIZE ;
    }

    public static synchronized void resetTimedHistory() {
        rawVADtimedHistory.clearDataHistory();
        filteredVADtimedHistory.clearDataHistory();
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
