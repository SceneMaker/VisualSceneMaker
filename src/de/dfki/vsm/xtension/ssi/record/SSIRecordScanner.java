package de.dfki.vsm.xtension.ssi.record;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.xml.XMLUtilities;
import de.dfki.vsm.xtension.ssi.event.SSIEventArray;
import de.dfki.vsm.xtension.ssi.event.SSIEventEntry;
import java.io.File;
import java.util.HashMap;

/**
 * @author Gregor Mehlmann
 */
public final class SSIRecordScanner {

    // The singelton logger instance
    private final static LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();
    //
    private final static SSIEventArray sEventArray = new SSIEventArray();
    //
    private final static HashMap<String, SSIEventArray> sEventArrayMap = new HashMap();

    public static void load(final String file) {
        sEventArray.clear();
        sEventArrayMap.clear();
        if(XMLUtilities.parseFromXMLFile(sEventArray, new File(file))){
            split();
        }
    }
    
    public static void split(){
        for(final SSIEventEntry entry : sEventArray.list()) {
            // Create a new array for data
            if(sEventArrayMap.get(entry.getData().toString()) == null) {
                sEventArrayMap.put(entry.getData().toString(), new SSIEventArray("vsm"));
            }
            // Add the entry to the array
            sEventArrayMap.get(entry.getData().toString()).add(entry);
        }
        
        for(final SSIEventArray array : sEventArrayMap.values()) {
            sLogger.message(array.toString());
        }
    }
    

}


/*
    private static String get(
            final String file,
            final String event,
            final String state,
            final String content) {
        //sLogger.message("Scanning file '" + file + "'");
        try {
            final Scanner scanner = new Scanner(new File(file));
            while (scanner.hasNextLine()) {
                final String line = scanner.nextLine();
                //sLogger.message("Reading line '" + line + "'");
            }
            //
            scanner.close();
        } catch (final FileNotFoundException exc) {
            sLogger.failure(exc.toString());
        }
        //
        return null;
    }*/
