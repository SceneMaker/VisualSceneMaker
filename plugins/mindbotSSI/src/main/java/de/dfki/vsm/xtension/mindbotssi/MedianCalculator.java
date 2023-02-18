package de.dfki.vsm.xtension.mindbotssi;

import java.util.Arrays;
import java.util.LinkedList ;

public class MedianCalculator {

    public static float[] computeMedians(TimedHistory history) {

        LinkedList<TimedFloats> dataHistory = history.dataHistory;

        int nVariables = history.getNumVariables() ;

        // Prepare the buffer for the results of the median on each variable
        float[] medians = new float[nVariables] ;
        for(int i=0 ; i<medians.length; i++) { medians[i] = Float.NaN ; }

        // Compute the medians for each variable
        for(int var_num=0 ; var_num < nVariables ; var_num++) {
            final int final_var_num = var_num ;

            // First, convert the value evolution of one variable into an array
            double[] var_values = dataHistory.stream()
                    .mapToDouble(entry -> entry.vs[final_var_num])
                    .toArray();

            medians[var_num] = (float) median(var_values);
        }

        return medians ;

    }


    public static double median(double[] values) {

        if(values.length == 0) {
            return Double.NaN ;
        }

        double[] sorted_value = values.clone() ;
        Arrays.sort(sorted_value);
        // Now, knowing that the values are sorted, we can easily compute the median
        int middleIDX = sorted_value.length/2;
        double medianValue ; //declare variable
        if (sorted_value.length % 2 == 1) {  // Odd length
            medianValue = sorted_value[middleIDX];
        } else { // Even length
            medianValue = (sorted_value[middleIDX - 1] + sorted_value[middleIDX]) / 2;
        }

        return medianValue ;
    }

}
