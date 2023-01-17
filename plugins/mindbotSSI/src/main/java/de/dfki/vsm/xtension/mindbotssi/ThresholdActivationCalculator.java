package de.dfki.vsm.xtension.mindbotssi;

import java.util.*;

/** Support class to analize the history of SSI values about valence, arousal and dominance,
 * and possibly detect whenever we are in one of the eight octants.
 */
public class ThresholdActivationCalculator {

    public static class ThresholdPair {
        public float lo ;
        public float hi ;

        public ThresholdPair(float lo_threshold, float hi_lo_threshold) {
            this.lo = lo_threshold;
            this.hi = hi_lo_threshold;
        }

    }

    private final TimedHistory timedHistory;
    private final ThresholdPair[] thresholds;



    public ThresholdActivationCalculator(TimedHistory history, ThresholdPair[] thresholds) {
        this.timedHistory = history ;
        this.thresholds = thresholds ;
    }


    /** Scans the data samples, starting from the oldest, and recording an instance if all of the thresholds
     * simultaneously exceed the thresholds. If yes, the entry is inserted in the output list.
     * @return
     */
    public String triggersAreActivated(float thresholdsMultiplier) {

        int num_vars = thresholds.length ;

        String[] threshold_codes = new String[num_vars];

        // For each variable (or threshold pair), we will scan the data history
        for(int var_num=0 ; var_num < num_vars ; var_num++) {
            ThresholdPair threshold_pair = thresholds[var_num];

            // We scan the history, but starting from the youngest value
            Iterator<TimedFloats> history_it = timedHistory.dataHistory.descendingIterator();
            while(history_it.hasNext()) {
                TimedFloats entry = history_it.next();
                float var_value = entry.vs[var_num] ;
                // if the value exceeds the thresholds in any direction
                if(var_value < threshold_pair.lo * thresholdsMultiplier) {
                    // register the excess and break the search in this variable
                    threshold_codes[var_num] = "-" + var_num;
                    break;
                } else if (var_value > threshold_pair.hi * thresholdsMultiplier) {
                    threshold_codes[var_num] = "+" + var_num;
                    break;
                }
            }

        }

        // If at least one of the elements of the analysis is still null,
        // then we don't report anything.
        if(Arrays.stream(threshold_codes).anyMatch(Objects::isNull)) {
            // System.out.println("thMul " + thresholdsMultiplier + "\tcode ''") ;
            return "" ;
        }

        // Else, compose the return code
        StringBuilder out = new StringBuilder();
        for(String threshold_res : threshold_codes) {
            assert threshold_res != null ;
            out.append(threshold_res);
        }

        // System.out.println("thMul " + thresholdsMultiplier + "\tcode "+out) ;

        return out.toString();

    }

}
