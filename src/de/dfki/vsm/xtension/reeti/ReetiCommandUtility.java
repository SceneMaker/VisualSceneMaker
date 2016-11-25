package de.dfki.vsm.xtension.reeti;

import java.util.HashMap;

/**
 *
 * @author Robbie
 * This is used to record the value of the variable in the command.
 * For example, String test = "10". [motor neckPan=test smooth='0.6']
 * ReetiCommandUtility stores ("test","10") in HashMap, and updates the command to [motor neckPan='10' smooth='0.6'].
 */
public class ReetiCommandUtility {
    private static HashMap<String,String> valRecord = new HashMap<String,String>();
    
    public static void setCommandValue(String key, String value){
        valRecord.put(key,value);
        System.out.println("HashMapvalRecord: " + "key:" + key + "   value: " + valRecord.get(key));
    }
    
    public static Boolean checkCommandValue(String key){
        return valRecord.containsKey(key);
    }
    
    public static String updateCommandValue(String key){
        return valRecord.get(key);
    }
}
