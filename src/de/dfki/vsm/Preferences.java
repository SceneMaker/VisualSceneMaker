package de.dfki.vsm;

import de.dfki.vsm.SceneMaker3;
import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------
import java.awt.Color;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.KeyEvent;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import java.lang.reflect.Method;

import java.net.URL;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Properties;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.swing.ImageIcon;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 * 
 * Standard VSM configurations 
 */
public final class Preferences {

    // The editor properties object
    private static final Properties sPROPERTIES = new Properties();
    // The global properties file    
    private static final String sCONFIG_FILE
            = System.getProperty("user.home")
            + System.getProperty("file.separator") + ".vsm";
    
    //////////////////////////////////////////////////////////////////////////////
    // SYSTEM PROPERTIES
    //////////////////////////////////////////////////////////////////////////////

    public static final String sSYSPROPS_LINE_SEPR = System.getProperty("line.separator");
    public static final String sSYSPROPS_FILE_SEPR = System.getProperty("file.separator");
    public static final String sSYSPROPS_PATH_SEPR = System.getProperty("path.separator");
    public static final String sSYSPROPS_JAVA_PATH = System.getProperty("java.class.path");
    public static final String sSYSPROPS_JAVA_HOME = System.getProperty("java.home");
    public static final String sSYSPROPS_JAVA_VEND = System.getProperty("java.vendor");
    public static final String sSYSPROPS_JAVA_VURL = System.getProperty("java.vendor.url");
    public static final String sSYSPROPS_OSYS_ARCH = System.getProperty("os.arch");
    public static final String sSYSPROPS_OSYS_NAME = System.getProperty("os.name");
    public static final String sSYSPROPS_OSYS_VERS = System.getProperty("os.version");
    
    ////////////////////////////////////////////////////////////////////////////
    // LOGFILE BASE CONFIGURATION
    ////////////////////////////////////////////////////////////////////////////
    public static final String sLOGFILE_FILE_NAME = "./log/vsm3.log";

    ////////////////////////////////////////////////////////////////////////////
    // LOGFILE BASE CONFIGURATION
    ////////////////////////////////////////////////////////////////////////////
    public static final String sNOVAFILE_FILE_NAME = "./log/nova.log";

    ////////////////////////////////////////////////////////////////////////////
    // LOGFILE BASE CONFIGURATION
    ////////////////////////////////////////////////////////////////////////////
    public static final String sSOCKFILE_FILE_NAME = "./log/sock.log";

    ////////////////////////////////////////////////////////////////////////////
    // URL RESOURCES
    ////////////////////////////////////////////////////////////////////////////
    public static final URL sSTYLESURL = SceneMaker3.class.getResource("/res/sty/scripts.xml");
    
    ////////////////////////////////////////////////////////////////////////////
    // DIRECTORIES
    ////////////////////////////////////////////////////////////////////////////
    public static final String sUSER_NAME = System.getProperty("user.name");
    public static final String sUSER_HOME = System.getProperty("user.home");
    public static final String sUSER_DIR  = System.getProperty("user.dir");
   
    //////////////////////////////////////////////////////////////////////////////
    // NODE COLORS
    //////////////////////////////////////////////////////////////////////////////
    public static final Color sBASIC_NODE_COLOR = new Color(200, 200, 200);
    public static final Color sHISTORY_NODE_COLOR = new Color(255, 255, 255);
    public static final Color sSUPER_NODE_COLOR = new Color(200, 200, 200);
    public static final Color sSELECTED_NODE_COLOR = new Color(211, 211, 211);

    //////////////////////////////////////////////////////////////////////////////
    // EDGE COLORS
    //////////////////////////////////////////////////////////////////////////////
    public static final Color sFEDGE_COLOR = new Color(150, 192, 206);  //BLUE
    public static final Color sEEDGE_COLOR = new Color(190, 185, 181);  //GRAY
    public static final Color sTEDGE_COLOR = new Color(200, 171, 101);  //BROWN
    public static final Color sCEDGE_COLOR = new Color(238, 230, 11);   //YELLOW
    public static final Color sPEDGE_COLOR = new Color(113, 178, 56);   //GREEN
    public static final Color sIEDGE_COLOR = new Color(229, 61, 60);    //RED

    //////////////////////////////////////////////////////////////////////////////
    // VISUALIZATION COLORS
    //////////////////////////////////////////////////////////////////////////////
    public static final Color sHIGHLIGHT_COLOR = new Color(211, 211, 211);
    public static final Color sTRANSLUCENT_HIGHLIGHT_COLOR = new Color(111, 251, 211, 100);    // Do not change opacity!
    public static final Color sTRANSLUCENT_RED_COLOR = new Color(246, 0, 0, 100);        // Do not change opacity!
    public static final Color sTRANSLUCENT_BLUE_COLOR = new Color(0, 0, 246, 100);        // Do not change opacity!
    public static final Color sTRANSLUCENT_GREEN_COLOR = new Color(0, 246, 0, 100);        // Do not change opacity!
    public static final Color sTRANSLUCENT_YELLOW_COLOR = new Color(0, 246, 246, 100);      // Do not change opacity!
    public static final Color sCOMMENT_BADGE_COLOR = new Color(246, 231, 191, 128);
    public static final Color sSTART_SIGN_COLOR = new Color(181, 45, 13);
    public static final Color sMESSAGE_COLOR = new Color(181, 45, 13);
    public static final Color sHIGHLIGHT_SCENE_COLOR = Color.YELLOW;

    //////////////////////////////////////////////////////////////////////////////
    // APPEARANCE CONFIGURATION
    //////////////////////////////////////////////////////////////////////////////
    public static boolean sSHOW_ELEMENTS = true;
    public static boolean sSHOW_ELEMENT_PROPERTIES = true;
    public static boolean sSHOW_SCENEEDITOR = true;
    public static boolean sSHOW_SCENEFLOWEDITOR = true;
    public static float sSCENEFLOW_SCENE_EDITOR_RATIO = 0.75f;
    public static boolean sSHOW_GESTURES = true;

    //////////////////////////////////////////////////////////////////////////////
    // RECENT FILES
    //////////////////////////////////////////////////////////////////////////////
    public static final int sMAX_RECENT_FILE_COUNT = 8;
    public static final int sMAX_RECENT_PROJECTS = 8;
    public static final ArrayList<Integer> sDYNAMIC_KEYS = new ArrayList<>(Arrays.asList(KeyEvent.VK_1,
            KeyEvent.VK_2, KeyEvent.VK_3, KeyEvent.VK_4,
            KeyEvent.VK_5, KeyEvent.VK_6, KeyEvent.VK_7,
            KeyEvent.VK_8, KeyEvent.VK_9));

    //////////////////////////////////////////////////////////////////////////////
    // FILE RESSOURCES
    //////////////////////////////////////////////////////////////////////////////
    public static final URL sABOUT_FILE = SceneMaker3.class.getResource("/res/doc/about.html");
    public static final URL sHELP_FILE = SceneMaker3.class.getResource("/res/doc/index.html");

    //////////////////////////////////////////////////////////////////////////////
    // FONT DATA
    //////////////////////////////////////////////////////////////////////////////
    public static final String sFONT_FAMILY_LIST[]
            = GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
    public static final Integer sFONT_SIZE_LIST[] = {
        6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30
    };

    // public static final int sFONT_SIZE_LIST[] = {6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30};
    //////////////////////////////////////////////////////////////////////////////
    // IMAGE RESSOURCES
    //////////////////////////////////////////////////////////////////////////////
    public static final ImageIcon sSCENEMAKER_LOGO
            = new ImageIcon(Toolkit.getDefaultToolkit().createImage(SceneMaker3.class.getResource("/res/img/smlogo.png")));
    public static final ImageIcon sSCENEMAKER_DOCICON
            = new ImageIcon(Toolkit.getDefaultToolkit().createImage(SceneMaker3.class.getResource("/res/img/docicon.png")));
    public static final ImageIcon sSHOW_GRID_ICON
            = new ImageIcon(Toolkit.getDefaultToolkit().createImage(SceneMaker3.class.getResource("/res/img/grid.png")));
    public static final ImageIcon sSCREENSHOT_ICON = new ImageIcon(
            Toolkit.getDefaultToolkit().createImage(
                    SceneMaker3.class.getResource("/res/img/screenshot.png")));
    public static final ImageIcon sVISUALISATIONICON = new ImageIcon(
            Toolkit.getDefaultToolkit().createImage(
                    SceneMaker3.class.getResource(
                            "/res/img/visualisation.png")));
    public static final ImageIcon sCOMMENT_ENTRY = new ImageIcon(
            Toolkit.getDefaultToolkit().createImage(
                    SceneMaker3.class.getResource(
                            "/res/img/elementtree/COMMENT_ENTRY.png")));
    public static final ImageIcon sSUPERNODE_ENTRY = new ImageIcon(
            Toolkit.getDefaultToolkit().createImage(
                    SceneMaker3.class.getResource(
                            "/res/img/elementtree/SUPERNODE_ENTRY.png")));
    public static final ImageIcon sBASICNODE_ENTRY = new ImageIcon(
            Toolkit.getDefaultToolkit().createImage(
                    SceneMaker3.class.getResource(
                            "/res/img/elementtree/BASICNODE_ENTRY.png")));
    public static final ImageIcon sEEDGE_ENTRY = new ImageIcon(
            Toolkit.getDefaultToolkit().createImage(
                    SceneMaker3.class.getResource(
                            "/res/img/elementtree/EEDGE_ENTRY.png")));
    public static final ImageIcon sTEDGE_ENTRY = new ImageIcon(
            Toolkit.getDefaultToolkit().createImage(
                    SceneMaker3.class.getResource(
                            "/res/img/elementtree/TEDGE_ENTRY.png")));
    public static final ImageIcon sPEDGE_ENTRY = new ImageIcon(
            Toolkit.getDefaultToolkit().createImage(
                    SceneMaker3.class.getResource(
                            "/res/img/elementtree/PEDGE_ENTRY.png")));
    public static final ImageIcon sCEDGE_ENTRY = new ImageIcon(
            Toolkit.getDefaultToolkit().createImage(
                    SceneMaker3.class.getResource(
                            "/res/img/elementtree/CEDGE_ENTRY.png")));
    public static final ImageIcon sIEDGE_ENTRY = new ImageIcon(
            Toolkit.getDefaultToolkit().createImage(
                    SceneMaker3.class.getResource(
                            "/res/img/elementtree/IEDGE_ENTRY.png")));
    public static final ImageIcon sFEDGE_ENTRY = new ImageIcon(
            Toolkit.getDefaultToolkit().createImage(
                    SceneMaker3.class.getResource(
                            "/res/img/elementtree/FEDGE_ENTRY.png")));
    public static final ImageIcon sROOT_FOLDER = new ImageIcon(
            Toolkit.getDefaultToolkit().createImage(
                    SceneMaker3.class.getResource(
                            "/res/img/elementtree/ROOT_FOLDER.png")));
    public static final ImageIcon sSCENE_FOLDER = new ImageIcon(
            Toolkit.getDefaultToolkit().createImage(
                    SceneMaker3.class.getResource(
                            "/res/img/elementtree/SCENE_FOLDER.png")));
    public static final ImageIcon sBASIC_FOLDER = new ImageIcon(
            Toolkit.getDefaultToolkit().createImage(
                    SceneMaker3.class.getResource(
                            "/res/img/elementtree/BASIC_FOLDER.png")));
    public static final ImageIcon sRADIOBUTTON_UNSELECTED
            = new ImageIcon(
                    Toolkit.getDefaultToolkit().createImage(
                            SceneMaker3.class.getResource("/res/img/elementtree/RADIOBUTTON_UNSELECTED.png")));
    public static final ImageIcon sRADIOBUTTON_SELECTED
            = new ImageIcon(
                    Toolkit.getDefaultToolkit().createImage(
                            SceneMaker3.class.getResource("/res/img/elementtree/RADIOBUTTON_SELECTED.png")));
    public static final ImageIcon sFUNCTION_ENTRY = new ImageIcon(
            Toolkit.getDefaultToolkit().createImage(
                    SceneMaker3.class.getResource(
                            "/res/img/elementtree/FUNCTION_ENTRY.png")));
    public static final ImageIcon sFUNCTION_ERROR_ENTRY
            = new ImageIcon(
                    Toolkit.getDefaultToolkit().createImage(
                            SceneMaker3.class.getResource("/res/img/elementtree/FUNCTION_ERROR_ENTRY.png")));


    /**
     *
     */
    private static synchronized void init() {

        // load visual appearance settings
        sSHOW_ELEMENTS = Boolean.valueOf(sPROPERTIES.getProperty("showelements"));
        sSHOW_ELEMENT_PROPERTIES = Boolean.valueOf(sPROPERTIES.getProperty("showelementproperties"));
        sSHOW_SCENEEDITOR = Boolean.valueOf(sPROPERTIES.getProperty("showsceneeditor"));
        sSHOW_SCENEFLOWEDITOR = Boolean.valueOf(sPROPERTIES.getProperty("showscenefloweditor"));
       // sSCENEFLOW_SCENE_EDITOR_RATIO = Float.valueOf(sPROPERTIES.getProperty("sceneflow_sceneeditor_ratio"));
        sSHOW_GESTURES = Boolean.valueOf(sPROPERTIES.getProperty("showgestures"));
    }
    
    public static synchronized void save() {
        try {
            try (FileOutputStream fileOutputStream = new FileOutputStream(sCONFIG_FILE)) {
                sPROPERTIES.storeToXML(fileOutputStream, "Properties for the Sceneflow Editor", "ISO8859_1");
            }
        } catch (IOException e) {
            LOGDefaultLogger.getInstance().failure("Error: " + e.getMessage());
        }
        init();
    }
    
    public static synchronized void load() {
        parseConfigFile();
        init();
    }
   
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

    private static synchronized void parseConfigFile() {
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

    public static synchronized void configure() {
        try {

            // Mac/Apple Settings
            System.setProperty("apple.laf.useScreenMenuBar", "true");
            System.setProperty("com.apple.mrj.application.apple.menu.about.name", "Visual SceneMaker");

            // Use system look and feel
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());

            UIManager.put("TabbedPane.contentBorderInsets", new Insets(0, 0, 0, 0));
            UIManager.put("TabbedPane.background", new Color(100, 100, 100));
            UIManager.put("TabbedPane.contentAreaColor", new Color(100, 100, 100));
            UIManager.put("TabbedPane.tabAreaBackground", new Color(100, 100, 100));

            // paint a nice doc icon when os is mac
            if (isMac()) {
                final Class appClass = Class.forName("com.apple.eawt.Application");
                // Get the application and the method to set the dock icon
                final Object app = appClass.getMethod("getApplication", new Class[]{}).invoke(null, new Object[]{});
                final Method setDockIconImage = appClass.getMethod("setDockIconImage", new Class[]{Image.class});
                // Set the dock icon to the logo of Visual Scene Maker 3  
                setDockIconImage.invoke(app, new Object[]{Preferences.sSCENEMAKER_DOCICON.getImage()});
            }
        } catch (final ClassNotFoundException | InstantiationException | IllegalAccessException | UnsupportedLookAndFeelException | NoSuchMethodException | SecurityException | IllegalArgumentException | InvocationTargetException exc) {
            LOGDefaultLogger.getInstance().failure("Error: " + exc.getMessage());
        }
    }

    // Check if we are on a WINDOWS system
    private static synchronized boolean isWindows() {
        final String os = System.getProperty("os.name").toLowerCase();
        return (os.contains("win"));
    }

    // Check if we are on a MAC system
    private static synchronized boolean isMac() {
        final String os = System.getProperty("os.name").toLowerCase();
        return (os.contains("mac"));
    }

    // Check if we are on a UNIX system
    private static synchronized boolean isUnix() {
        final String os = System.getProperty("os.name").toLowerCase();
        return ((os.contains("nix")) || (os.contains("nux")));
    }
}
