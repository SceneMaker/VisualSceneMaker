package de.dfki.vsm.xtension.mindbotssi;

import de.dfki.vsm.xtension.ssi.event.data.SSITupleData;

import java.util.HashMap;
import java.util.LinkedList;

public class DominanceCalculator {

    final static HashMap<String, Float> ekmanToDominance = new HashMap<String, Float>() {{
        put("surprise", -0.16f);
        put("happy", 0.46f);
        put("sad", -0.33f);
        put("neutral", 0.0f);
        put("disgust", -0.36f);
        put("anger", 0.25f);
        put("fear", -0.43f);
        put("contempt", 0.4f);
    }} ;


    // Fill in the list of Ekman expressions
    static String[] ekmanExpressions = new String[] {"surprise", "happy", "sad", "neutral", "disgust", "anger", "fear" } ;


    public static int argmax(Double[] array) {
        double max = array[0];
        int re = 0;
        for (int i = 1; i < array.length; i++) {
            if (array[i] > max) {
                max = array[i];
                re = i;
            }
        }
        return re;
    }


    public static float computeDominanceArgMax(SSITupleData tupleData, boolean modulate) {
        // Retrieves the list of Ekman predictions from the SSI dictionary
        LinkedList<Double> predictions = new LinkedList<>() ;
        for(String e: ekmanExpressions) {
            Double v = Double.parseDouble(tupleData.get(e)) ;
            predictions.add(v) ;
        }

        assert ekmanExpressions.length == predictions.size() ;

        // calc argmax
        Double[] array = predictions.toArray(new Double[0]) ;  // predictions in array form
        int max_expression_idx = argmax(array) ;
        String max_expression = ekmanExpressions[max_expression_idx] ;

        // Convert into dominance
        float dom = ekmanToDominance.get(max_expression) ;

        if (modulate) {
            double max_expression_val = array[max_expression_idx] ;
            dom *= max_expression_val ;
        }

        dom = (dom + 1.0f) / 2.0f ;  // rescale to range [0,1]
        return dom ;
    }

    public static float computeDominanceAccumulated(SSITupleData tupleData) {

        float dom = 0.0f;

        // Retrieves the list of Ekman predictions from the SSI dictionary
        for(String e: ekmanExpressions) {
            float prediction_for_e = Float.parseFloat(tupleData.get(e)) ;
            float dom_for_e = ekmanToDominance.get(e) ;

            float modulated_dom = dom_for_e * prediction_for_e ;
            dom += modulated_dom ;
        }

        dom = (dom + 1.0f) / 2.0f ;  // rescale to range [0,1]
        return dom ;
    }

}
