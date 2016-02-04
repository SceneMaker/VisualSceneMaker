package de.dfki.vsm.model.project;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.Preferences;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.xml.XMLUtilities;

//~--- JDK imports ------------------------------------------------------------
import java.awt.Dimension;

import java.io.*;

import java.util.Properties;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * @author Patrick Gebhard
 * @author Sergio Soto
 *
 * This class saves project related configurations.
 */
public class EditorConfig {

    // The Logger Instance
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    ////////////////////////////////////////////////////////////////////////////
    // SCENEMAKER PROPERTIES
    ////////////////////////////////////////////////////////////////////////////
    private final Properties sPROPERTIES = new Properties();

    ////////////////////////////////////////////////////////////////////////////
    // VARIABLE FIELDS
    ////////////////////////////////////////////////////////////////////////////
    public int sNODEWIDTH = 100;
    public int sNODEHEIGHT = 100;
    public int sSUPERNODEWIDTH = sNODEWIDTH;
    public int sSUPERNODEHEIGHT = sNODEWIDTH;
    public Dimension sNODESIZE = new Dimension(sNODEWIDTH, sNODEHEIGHT);
    public Dimension sSUPERNODESIZE = new Dimension(sSUPERNODEWIDTH, sSUPERNODEHEIGHT);
    public int sGRID_NODEWIDTH = sNODEWIDTH;
    public int sGRID_NODEHEIGHT = sNODEHEIGHT;
    public int sGRID_XSCALE = 1;
    public int sGRID_YSCALE = sGRID_XSCALE;
    public int sGRID_XSPACE = sNODEWIDTH * sGRID_XSCALE;
    public int sGRID_YSPACE = sNODEHEIGHT * sGRID_YSCALE;
    public int sXOFFSET = sGRID_NODEWIDTH / 3;
    public int sYOFFSET = sGRID_NODEHEIGHT / 3;
    public int sWORKSPACEFONTSIZE = 16;
    public float sEDITORFONTSIZE = 11;
    public boolean sLAUNCHPLAYER = false;
    public boolean sSHOWGRID = true;
    public boolean sVISUALISATION = true;
    public boolean sACTIVITYTRACE = true;
    public int sVISUALISATIONTIME = 15;    // 25 = 1 second
    public boolean sSHOW_VARIABLE_BADGE_ON_WORKSPACE = true;
    public boolean sSHOW_SMART_PATH_DEBUG = false;
    public boolean sSHOWIDSOFNODES = true;
    public String sSCRIPT_FONT_TYPE = "Monospaced";
    public int sSCRIPT_FONT_SIZE = 16;
    public boolean sSHOWSCENE_ELEMENTS = false;
    public boolean sAUTOHIDE_BOTTOMPANEL = true; // Saves the pricked pin of the bottom panel of the editor
    public String sMAINSUPERNODENAME = "default"; 

    public EditorConfig() {
        
        if (!sPROPERTIES.containsKey("node_width")) {
            sPROPERTIES.setProperty("node_width", "90");
        }
        
        if (!sPROPERTIES.containsKey("node_height")) {
            sPROPERTIES.setProperty("node_height", "90");
        }
        
        if (!sPROPERTIES.containsKey("grid_x")) {
            sPROPERTIES.setProperty("grid_x", "1");
        }
        
        if (!sPROPERTIES.containsKey("grid_y")) {
            sPROPERTIES.setProperty("grid_y", "1");
        }
        
        if (!sPROPERTIES.containsKey("visualization")) {
            sPROPERTIES.setProperty("visualization", "true");
        }
        
        if (!sPROPERTIES.containsKey("visualizationtrace")) {
            sPROPERTIES.setProperty("visualizationtrace", "true");
        }
        
        if (!sPROPERTIES.containsKey("shownodeid")) {
            sPROPERTIES.setProperty("shownodeid", "true");
        }
        
        if (!sPROPERTIES.containsKey("showvariables")) {
            sPROPERTIES.setProperty("showvariables", "true");
        }
        
        if (!sPROPERTIES.containsKey("showsmartpathcalculations")) {
            sPROPERTIES.setProperty("showsmartpathcalculations", "false");
        }
        
        if (!sPROPERTIES.containsKey("showsceneelements")) {
            sPROPERTIES.setProperty("showsceneelements", "false");
        }

        // default values for editor appearance
        if (!sPROPERTIES.containsKey("showelements")) {
            sPROPERTIES.setProperty("showelements", "true");
        }
        
        if (!sPROPERTIES.containsKey("showelementproperties")) {
            sPROPERTIES.setProperty("showelementproperties", "true");
        }
        
        if (!sPROPERTIES.containsKey("propertiesdividerlocation")) {
            sPROPERTIES.setProperty("propertiesdividerlocation", "790");
        }
        
        if (!sPROPERTIES.containsKey("showscenefloweditor")) {
            sPROPERTIES.setProperty("showscenefloweditor", "true");
        }
        
        if (!sPROPERTIES.containsKey("showsceneeditor")) {
            sPROPERTIES.setProperty("showsceneeditor", "true");
        }
        
        if (!sPROPERTIES.containsKey("sceneflow_sceneeditor_ratio")) {
            sPROPERTIES.setProperty("sceneflow_sceneeditor_ratio", "0.75");
        }
        
        if (!sPROPERTIES.containsKey("showgestures")) {
            sPROPERTIES.setProperty("showgestures", "true");
        }

        // visual appearance of workspace and its elements
        if (!sPROPERTIES.containsKey("grid")) {
            sPROPERTIES.setProperty("grid", "true");
        }
        
        if (!sPROPERTIES.containsKey("num_magnets")) {
            sPROPERTIES.setProperty("num_magnets", "8");
        }
        
        if (!sPROPERTIES.containsKey("workspace_fontsize")) {
            sPROPERTIES.setProperty("workspace_fontsize", "11");
        }
        
        if (!sPROPERTIES.containsKey("scriptfonsize")) {
            sPROPERTIES.setProperty("scriptfonsize", "16");
        }
        
        if (!sPROPERTIES.containsKey("scriptfonttype")) {
            sPROPERTIES.setProperty("scriptfonttype", "Monospaced");
        }
        
        if (!sPROPERTIES.containsKey("launchPlayer")) {
            sPROPERTIES.setProperty("launchPlayer", "false");
        }
        if (!sPROPERTIES.containsKey("autohidebottombar")) { //state of the bottom bar (pricked pin)
            sPROPERTIES.setProperty("autohidebottombar", "true");
        }
        if (!sPROPERTIES.containsKey("defaultsupernodename")) { //state of the bottom bar (pricked pin)
            sPROPERTIES.setProperty("defaultsupernodename", "default");
        }
        
    }

    /**
     *
     */
    private synchronized void init() {
        sNODEWIDTH = Integer.valueOf(sPROPERTIES.getProperty("node_width"));
        sNODEHEIGHT = Integer.valueOf(sPROPERTIES.getProperty("node_height"));
        sGRID_XSCALE = Integer.valueOf(sPROPERTIES.getProperty("grid_x"));
        sGRID_YSCALE = Integer.valueOf(sPROPERTIES.getProperty("grid_y"));
        sWORKSPACEFONTSIZE = Integer.valueOf(sPROPERTIES.getProperty("workspace_fontsize"));
        sLAUNCHPLAYER = Boolean.valueOf(sPROPERTIES.getProperty("launchPlayer"));
        sSHOWGRID = Boolean.valueOf(sPROPERTIES.getProperty("grid"));
        sVISUALISATION = Boolean.valueOf(sPROPERTIES.getProperty("visualization"));
        sACTIVITYTRACE = Boolean.valueOf(sPROPERTIES.getProperty("visualizationtrace"));
        sSHOWIDSOFNODES = Boolean.valueOf(sPROPERTIES.getProperty("shownodeid"));
        sSHOW_VARIABLE_BADGE_ON_WORKSPACE = Boolean.valueOf(sPROPERTIES.getProperty("showvariables"));
        sSHOW_SMART_PATH_DEBUG = Boolean.valueOf(sPROPERTIES.getProperty("showsmartpathcalculations"));
        sSCRIPT_FONT_TYPE = String.valueOf(sPROPERTIES.getProperty("scriptfonttype"));
        sSCRIPT_FONT_SIZE = Integer.valueOf(sPROPERTIES.getProperty("scriptfonsize"));
        sSHOWSCENE_ELEMENTS = Boolean.valueOf(sPROPERTIES.getProperty("showsceneelements"));
        sAUTOHIDE_BOTTOMPANEL = Boolean.valueOf(sPROPERTIES.getProperty("autohidebottombar")); //loads pricked pin status
        sSUPERNODEWIDTH = sNODEWIDTH;
        sSUPERNODEHEIGHT = sNODEWIDTH;
        sGRID_NODEWIDTH = sNODEWIDTH;
        sGRID_NODEHEIGHT = sNODEHEIGHT;
        sNODESIZE = new Dimension(sNODEWIDTH, sNODEHEIGHT);
        sSUPERNODESIZE = new Dimension(sSUPERNODEWIDTH, sSUPERNODEHEIGHT);
        sGRID_XSPACE = sNODEWIDTH * sGRID_XSCALE;
        sGRID_YSPACE = sNODEHEIGHT * sGRID_YSCALE;
        sXOFFSET = sGRID_NODEWIDTH / 3;
        sYOFFSET = sGRID_NODEHEIGHT / 3;
        sMAINSUPERNODENAME = String.valueOf(sPROPERTIES.getProperty("defaultsupernodename"));
    }

    ////////////////////////////////////////////////////////////////////////////
    //
    ////////////////////////////////////////////////////////////////////////////
    public boolean save(final File base) {

        // Create the project configuration file
        final File file = new File(base, "editorconfig.xml");

        // Check if the configuration does exist
        if (!file.exists()) {

            // Print a warning message if this case
            mLogger.warning("Warning: Creating the new project editor configuration file '" + file + "'");

            // Create a new configuration file now
            try {

                // Try to create a new configuration file
                if (!file.createNewFile()) {

                    // Print an error message if this case
                    mLogger.warning("Warning: There already exists a project editor configuration file '" + file + "'");
                }
            } catch (final IOException exc) {

                // Print an error message if this case
                mLogger.failure("Failure: Cannot create the new project editor configuration file '" + file + "'");

                // Return failure if it does not exist
                return false;
            }
        }

        // Write the project configuration file
        if (!XMLUtilities.writeToXMLFile(sPROPERTIES, file)) {

            // Print an error message if this case
            mLogger.failure("Error: Cannot write project editor configuration file '" + file + "'");

            // Return failure if it does not exist
            return false;
        }
        
        init();

        // Print an information message if this case
        mLogger.message("Saved project configuration file '" + file + "':\n" + sPROPERTIES);

        // Return success if the project was saved
        return true;
        
    }
    
    public synchronized boolean load(final String path) {

//        InputStream inputStream = null;
//        if(path.startsWith(Preferences.sSAMPLE_PROJECTS)){
//            inputStream = ClassLoader.getSystemResourceAsStream(path + System.getProperty("file.separator")  + "editorconfig.xml");
//            if (inputStream == null) {
//                // Print an error message in this case
//                mLogger.failure("Error: Cannot find gesticon configuration file  ");
//                // Return failure if it does not exist
//                return false;
//            }
//
//        }
//        else {
            final File file = new File(path, "editorconfig.xml");
            // Check if the configuration file does exist
            if (!file.exists()) {
                // Print an error message in this case
                mLogger.failure("Error: Cannot find editor configuration file '" + file + "'");
                // Return failure if it does not exist
                return false;
            }
//            try {
//                inputStream = new FileInputStream(file);
//            } catch (FileNotFoundException e) {
//                mLogger.failure("Error: Cannot find editor configuration file '" + file + "'");
//            }
//        }
        if(!XMLUtilities.parseFromXMLFile(sPROPERTIES, file)){
            mLogger.failure("Error: Cannot parse editor configuration file  in path" + path);
            return false;
        }




        
        /*try {

                sPROPERTIES.loadFromXML(inputStream);

        } catch (IOException e) {
            e.printStackTrace(System.out);
        }*/
        
        if (!sPROPERTIES.containsKey("defaultsupernodename")) { 
            sPROPERTIES.setProperty("defaultsupernodename", "default");
        }
           
        if (!sPROPERTIES.containsKey("frame_title")) {
            sPROPERTIES.setProperty("frame_title", "Visual SceneMaker");
        }
        
        if (!sPROPERTIES.containsKey("frame_name")) {
            sPROPERTIES.setProperty("frame_name", "SceneFlowEditor");
        }
        
        if (!sPROPERTIES.containsKey("icon_file")) {
            sPROPERTIES.setProperty("icon_file", "res/img/icon.png");
        }
        
        if (!sPROPERTIES.containsKey("frame_posx")) {
            sPROPERTIES.setProperty("frame_posx", "0");
        }
        
        if (!sPROPERTIES.containsKey("frame_posy")) {
            sPROPERTIES.setProperty("frame_posy", "0");
        }
        
        if (!sPROPERTIES.containsKey("frame_width")) {
            sPROPERTIES.setProperty("frame_width", "800");
        }
        
        if (!sPROPERTIES.containsKey("frame_height")) {
            sPROPERTIES.setProperty("frame_height", "600");
        }
        
        if (!sPROPERTIES.containsKey("node_width")) {
            sPROPERTIES.setProperty("node_width", "90");
        }
        
        if (!sPROPERTIES.containsKey("node_height")) {
            sPROPERTIES.setProperty("node_height", "90");
        }
        
        if (!sPROPERTIES.containsKey("grid_x")) {
            sPROPERTIES.setProperty("grid_x", "1");
        }
        
        if (!sPROPERTIES.containsKey("grid_y")) {
            sPROPERTIES.setProperty("grid_y", "1");
        }
        
        if (!sPROPERTIES.containsKey("visualization")) {
            sPROPERTIES.setProperty("visualization", "true");
        }
        
        if (!sPROPERTIES.containsKey("visualizationtrace")) {
            sPROPERTIES.setProperty("visualizationtrace", "true");
        }
        
        if (!sPROPERTIES.containsKey("shownodeid")) {
            sPROPERTIES.setProperty("shownodeid", "true");
        }
        
        if (!sPROPERTIES.containsKey("showvariables")) {
            sPROPERTIES.setProperty("showvariables", "true");
        }
        
        if (!sPROPERTIES.containsKey("showsmartpathcalculations")) {
            sPROPERTIES.setProperty("showsmartpathcalculations", "false");
        }
        
        if (!sPROPERTIES.containsKey("showsceneelements")) {
            sPROPERTIES.setProperty("showsceneelements", "false");
        }

        // default values for editor appearance
        if (!sPROPERTIES.containsKey("showelements")) {
            sPROPERTIES.setProperty("showelements", "true");
        }
        
        if (!sPROPERTIES.containsKey("showelementproperties")) {
            sPROPERTIES.setProperty("showelementproperties", "true");
        }
        
        if (!sPROPERTIES.containsKey("propertiesdividerlocation")) {
            sPROPERTIES.setProperty("propertiesdividerlocation", "790");
        }
        
        if (!sPROPERTIES.containsKey("showscenefloweditor")) {
            sPROPERTIES.setProperty("showscenefloweditor", "true");
        }
        
        if (!sPROPERTIES.containsKey("showsceneeditor")) {
            sPROPERTIES.setProperty("showsceneeditor", "true");
        }
        
        if (!sPROPERTIES.containsKey("sceneflow_sceneeditor_ratio")) {
            sPROPERTIES.setProperty("sceneflow_sceneeditor_ratio", "0.75");
        }
        
        if (!sPROPERTIES.containsKey("showgestures")) {
            sPROPERTIES.setProperty("showgestures", "true");
        }

        // visual appearance of workspace and its elements
        if (!sPROPERTIES.containsKey("grid")) {
            sPROPERTIES.setProperty("grid", "true");
        }
        
        if (!sPROPERTIES.containsKey("num_magnets")) {
            sPROPERTIES.setProperty("num_magnets", "8");
        }
        
        if (!sPROPERTIES.containsKey("workspace_fontsize")) {
            sPROPERTIES.setProperty("workspace_fontsize", "11");
        }
        
        if (!sPROPERTIES.containsKey("scriptfonsize")) {
            sPROPERTIES.setProperty("scriptfonsize", "11");
        }
        
        if (!sPROPERTIES.containsKey("scriptfonttype")) {
            sPROPERTIES.setProperty("scriptfonttype", "Arial");
        }
        
        if (!sPROPERTIES.containsKey("launchPlayer")) {
            sPROPERTIES.setProperty("launchPlayer", "false");
        }
        if (!sPROPERTIES.containsKey("autohidebottombar")) { // load state of the pin of the bottom bar
            sPROPERTIES.setProperty("autohidebottombar", "true");
        }
        
        
        //
        init();

        // Print an information message if this case
        mLogger.message("Loaded project editor configuration file in path'" + path+ "':\n");
        ByteArrayOutputStream stream = new ByteArrayOutputStream();
        if (XMLUtilities.writeToXMLStream(sPROPERTIES, stream)) {
            try {
                mLogger.message(stream.toString("UTF-8"));
            } catch (Exception exc) {
                mLogger.failure(exc.toString());
            }
        }

        // Return success if the project was loaded
        return true;
    }

    // TODO: This should actually be private
    public synchronized String getProperty(String key) {
        return sPROPERTIES.getProperty(key);
    }

    // TODO: This should actually be private
    public synchronized Object setProperty(String key, String value) {
        return sPROPERTIES.setProperty(key, value);
    }

    // TODO: This should actually be private
    public synchronized Object removeProperty(String key) {
        return sPROPERTIES.remove(key);
    }

    // TODO: This should actually be private
    public synchronized SortedSet<Object> getKeySet() {
        return new TreeSet<>(sPROPERTIES.keySet());
    }

    // Get the string representation of the configuration
    @Override
    public final String toString() {

        // Create a new byte array buffer stream
        final ByteArrayOutputStream buffer = new ByteArrayOutputStream();

        // Try to write the project into the stream
        XMLUtilities.writeToXMLStream(sPROPERTIES, buffer);

        // Return the stream string representation
        try {
            return buffer.toString("UTF-8");
        } catch (Exception exc) {
            return buffer.toString();
        }
    }
}
