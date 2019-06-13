package de.dfki.vsm;

import de.dfki.vsm.util.log.LOGDefaultLogger;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Properties;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.logging.Level;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 * @author Lenny Händler
 * <p>
 * Standard VSM configurations
 */
public class Preferences {
    public static final String sSYSPROPS_LINE_SEPR          = System.getProperty("line.separator");
    public static final String sSYSPROPS_FILE_SEPR          = System.getProperty("file.separator");
    public static final String sSYSPROPS_PATH_SEPR          = System.getProperty("path.separator");
    public static final String sSYSPROPS_JAVA_PATH          = System.getProperty("java.class.path");
    public static final String sSYSPROPS_JAVA_HOME          = System.getProperty("java.home");
    public static final String sSYSPROPS_JAVA_VEND          = System.getProperty("java.vendor");
    public static final String sSYSPROPS_JAVA_VURL          = System.getProperty("java.vendor.url");
    public static final String sSYSPROPS_OSYS_ARCH          = System.getProperty("os.arch");
    public static final String sSYSPROPS_OSYS_NAME          = System.getProperty("os.name");
    public static final String sSYSPROPS_OSYS_VERS          = System.getProperty("os.version");
    ////////////////////////////////////////////////////////////////////////////
    // LOGFILE BASE CONFIGURATION
    ////////////////////////////////////////////////////////////////////////////
    public static final String sLOGFILE_FILE_NAME           = "./log/vsm3.log";
    ////////////////////////////////////////////////////////////////////////////
    // LOGFILE BASE CONFIGURATION
    ////////////////////////////////////////////////////////////////////////////
    // PG: Why is this in the VSM preferences?
	public static final String sNOVAFILE_FILE_NAME          = "./log/nova.log";
    ////////////////////////////////////////////////////////////////////////////
    // LOGFILE BASE CONFIGURATION
    ////////////////////////////////////////////////////////////////////////////
    public static final String sSOCKFILE_FILE_NAME          = "./log/sock.log";
    //////////////////////////////////////////////////////////////////////////////
    // DATE FORMAT
    //////////////////////////////////////////////////////////////////////////////
    public static final SimpleDateFormat sDATE_FORMAT = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss");
    ////////////////////////////////////////////////////////////////////////////
    // VERSION INFORMATION
    ////////////////////////////////////////////////////////////////////////////
    public static final URL sVSM_VERSIONURL = Core.class.getResource("/res/version.ini");
    ////////////////////////////////////////////////////////////////////////////
    // URL RESOURCES
    ////////////////////////////////////////////////////////////////////////////
    public static final URL sSTYLESURL = Core.class.getResource("/res/sty/scripts.xml");
    ////////////////////////////////////////////////////////////////////////////
    // DIRECTORIES
    ////////////////////////////////////////////////////////////////////////////
    public static final String sUSER_NAME                   = System.getProperty("user.name");
    public static final String sUSER_HOME                   = System.getProperty("user.home");
    public static final String sUSER_DIR                    = System.getProperty("user.dir");
    // The editor properties object
    protected static final Properties sPROPERTIES = new Properties();
    // The global properties file
    protected static final String sCONFIG_FILE
            = System.getProperty("user.home")
            + System.getProperty("file.separator") + ".vsm";

    /**
     *
     */


    // TODO: This should actually be private
    public static synchronized String getProperty(String key) {
        return sPROPERTIES.getProperty(key);
    }

    // TODO: This should actually be private
    public static synchronized Object setProperty(String key, String value) {
        return sPROPERTIES.setProperty(key, value);
    }

    // TODO: This should actually be private
    public static synchronized Object removeProperty(String key) {
        return sPROPERTIES.remove(key);
    }

    // TODO: This should actually be private
    public static synchronized SortedSet<Object> getKeySet() {
        return new TreeSet<>(sPROPERTIES.keySet());
    }

    protected static synchronized void parseConfigFile() {
        if ((new File(sCONFIG_FILE)).canRead()) {
            try {
                try (FileInputStream in = new FileInputStream(sCONFIG_FILE)) {
                    sPROPERTIES.loadFromXML(in);
                }
            } catch (IOException e) {
                LOGDefaultLogger.getInstance().failure("Error: " + e.getMessage());
            }
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
            sPROPERTIES.setProperty("visualization", "false");
        }

        if (!sPROPERTIES.containsKey("visualizationtrace")) {
            sPROPERTIES.setProperty("visualizationtrace", "false");
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

        if (!sPROPERTIES.containsKey("xmlns")) {
            sPROPERTIES.setProperty("xmlns", "xml.sceneflow.dfki.de");
        }

        if (!sPROPERTIES.containsKey("xmlns_xsi")) {
            sPROPERTIES.setProperty("xmlns_xsi", "http://www.w3.org/2001/XMLSchema-instance");
        }

        if (!sPROPERTIES.containsKey("xsi_schemeLocation")) {
            sPROPERTIES.setProperty("xsi_schemeLocation", "res/xsd/sceneflow.xsd");
        }

        if (!sPROPERTIES.containsKey("workspace_fontsize")) {
            sPROPERTIES.setProperty("workspace_fontsize", "11");
        }
    }

    public static Level getLogLevel() {
        return Level.ALL;
    }
}
