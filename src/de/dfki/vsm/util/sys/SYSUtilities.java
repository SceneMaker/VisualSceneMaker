package de.dfki.vsm.util.sys;

import de.dfki.vsm.SceneMaker3;
import java.net.URL;
import java.util.Properties;

/**
 * @author Not me
 */
public final class SYSUtilities {

    // The system properties object
    private static final Properties sSYSTEM_PROPERTIES = System.getProperties();

    // Some of the system properties
    public static final String sSYSPROPS_LINE_SEPR = sSYSTEM_PROPERTIES.getProperty("line.separator");
    public static final String sSYSPROPS_FILE_SEPR = sSYSTEM_PROPERTIES.getProperty("file.separator");
    public static final String sSYSPROPS_PATH_SEPR = sSYSTEM_PROPERTIES.getProperty("path.separator");
    public static final String sSYSPROPS_JAVA_PATH = sSYSTEM_PROPERTIES.getProperty("java.class.path");
    public static final String sSYSPROPS_JAVA_HOME = sSYSTEM_PROPERTIES.getProperty("java.home");
    public static final String sSYSPROPS_JAVA_VEND = sSYSTEM_PROPERTIES.getProperty("java.vendor");
    public static final String sSYSPROPS_JAVA_VURL = sSYSTEM_PROPERTIES.getProperty("java.vendor.url");
    public static final String sSYSPROPS_OSYS_ARCH = sSYSTEM_PROPERTIES.getProperty("os.arch");
    public static final String sSYSPROPS_OSYS_NAME = sSYSTEM_PROPERTIES.getProperty("os.name");
    public static final String sSYSPROPS_OSYS_VERS = sSYSTEM_PROPERTIES.getProperty("os.version");
    // The user name and directories
    public static final String sSYSPROPS_USER_HDIR = sSYSTEM_PROPERTIES.getProperty("user.dir");
    public static final String sSYSPROPS_USER_HOME = sSYSTEM_PROPERTIES.getProperty("user.home");
    public static final String sSYSPROPS_USER_NAME = sSYSTEM_PROPERTIES.getProperty("user.name");

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
}
