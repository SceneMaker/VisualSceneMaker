package de.dfki.vsm.xtension.mindbotssi;

import java.util.*;

/** Support class to analize the history of SSI values about valence, arousal and dominance,
 * and possibly detect whenever we are in one of the eight octants.
 */
public class ThresholdActivatonCalculator {

    public class ThresholdPair {
        public float lo ;
        public float hi ;

        public ThresholdPair(float lo_threshold, float hi_lo_threshold) {
            this.lo = lo_threshold;
            this.hi = hi_lo_threshold;
        }

    }

    private class TimedFloats {
        /* Timestamp in milliseconds */
        long t;
        /* Value */
        float[] vs;

        TimedFloats(long t, float[] values) {
            this.t = t ;
            this.vs = values ;
        }

        TimedFloats(float[] v) {
            this(System.currentTimeMillis(), v) ;
        }
    }


    // Calibration values 3.0, calibrating on the "Slow task" section after filtering with median on the last 5 frames
    final static float RMSE_V_LO = -0.09855826509377788f ;
    final static float RMSE_V_HI = 0.13843901708496958f;
    final static float RMSE_A_LO = -0.1338607261206554f ;
    final static float RMSE_A_HI = 0.0715044268674914f ;
    final static float RMSE_D_LO = -0.020320960538339015f ;
    final static float RMSE_D_HI = 0.05254249940970949f ;


    private float thresholdsMultiplier;

    private long timeWindowSizeMillis ;

    private List<ThresholdPair> thresholds;
    private ThresholdPair[] modulatedThresholds;

    private LinkedList<TimedFloats> dataHistory = new LinkedList<>() ;


    public ThresholdActivatonCalculator(List<ThresholdPair> threshold_list, float thresholds_multiplier, float time_window_size_secs) {
        this.timeWindowSizeMillis = (long)time_window_size_secs * 1000L ;
        this.thresholds = threshold_list ;
        // this.thresholdMultiplier = thresholds_multiplier ;
        this.setThresholdsMultiplier(thresholds_multiplier);
    }

    public float getThresholdsMultiplier() {
        return thresholdsMultiplier;
    }

    public void setThresholdsMultiplier(float rmseMultiplier) {
        thresholdsMultiplier = rmseMultiplier;

        // Allocate fixed array space for the modulated values
        modulatedThresholds = new ThresholdPair[thresholds.size()] ;

        ListIterator<ThresholdPair> it = thresholds.listIterator();
        while(it.hasNext()) {
            ThresholdPair tp = it.next();
            int i = it.nextIndex() ;

            ThresholdPair modulated_tp = new ThresholdPair(
                    tp.lo * thresholdsMultiplier,
                    tp.hi * thresholdsMultiplier) ;

            modulatedThresholds[i] = modulated_tp ;
        }

        assert thresholds.size() == modulatedThresholds.length ;
    }

    /** Add a new entry into the times history, and also delete old values, getting too old with respect to the time window size.
     *
     * @param new_data
     */
    public void appendData(float[] new_data) {

        if(new_data.length != this.thresholds.size()) {
            throw new RuntimeException("The number of values in the datapoint (" + new_data.length + ")" +
                    " doesn't match the number of thresholds registered for this instance (" + this.thresholds.size() + ")") ;
        }

        long now = System.currentTimeMillis() ;
        TimedFloats new_entry = new TimedFloats(now, new_data) ;
        dataHistory.add(new_entry) ;

        // Remove all "old" elements
        while(dataHistory.size() > 0) {
            TimedFloats oldest_entry = dataHistory.getFirst();
            // If the first element is too old, remove it
            if(now - oldest_entry.t > timeWindowSizeMillis) {
                dataHistory.removeFirst() ;
            } else {
                // if it is young enough, we can stop here.
                break ;
            }
        }

    }

    /** Scans the data samples, starting from the oldest, and recording an instance if all of the thresholds
     * simultaneously exceed the thresholds. If yes, the entry is inserted in the output list.
     * @return
     */
    public String triggersAreActivated() {

        int num_vars = modulatedThresholds.length ;

        String[] threshold_codes = new String[num_vars];

        // For each variable (or threshold pair), we will scan the data history
        for(int var_num=0 ; var_num < num_vars ; var_num++) {
            ThresholdPair threshold_pair = modulatedThresholds[var_num];

            // We scan the history, but starting from the youngest value
            Iterator<TimedFloats> history_it = dataHistory.descendingIterator();
            while(history_it.hasNext()) {
                TimedFloats entry = history_it.next();
                float var_value = entry.vs[var_num] ;
                // if the value exceeds the thresholds in any direction
                if(var_value < threshold_pair.lo) {
                    // register the excess and break the search in this variable
                    threshold_codes[var_num] = "-" + var_num;
                    break;
                } else if (var_value > threshold_pair.hi) {
                    threshold_codes[var_num] = "+" + var_num;
                    break;
                }
            }

        }

        // If at least one of the elements of the analysis is still null,
        // then we don't report anything.
        if(Arrays.stream(threshold_codes).anyMatch(Objects::isNull)) {
            return "" ;
        }

        // Else, compose the return code
        StringBuilder out = new StringBuilder();
        for(String threshold_res : threshold_codes) {
            assert threshold_res != null ;
            out.append(threshold_res);
        }

        return out.toString();

    }

}
