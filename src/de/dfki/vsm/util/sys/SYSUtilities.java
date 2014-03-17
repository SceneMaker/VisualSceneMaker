package de.dfki.vsm.util.sys;

import java.net.URL;
import java.util.Properties;

/**
 * @author Gregor Mehlmann
 */
public final class SYSUtilities {

    ////////////////////////////////////////////////////////////////////////////
    // RESOURCE CLASS
    ////////////////////////////////////////////////////////////////////////////
    private static final Class sRESOURCE_CLASS = SYSUtilities.class;
    ////////////////////////////////////////////////////////////////////////////
    // PROPERTY INSTANCES
    ////////////////////////////////////////////////////////////////////////////
    private static final Properties sSYSPROPS = System.getProperties();
    private static final Properties sAPPPROPS = new Properties();
    ////////////////////////////////////////////////////////////////////////////
    // SYSTEM PROPERTIES
    ////////////////////////////////////////////////////////////////////////////
    public static final String sSYSPROPS_LINE_SEPR = sSYSPROPS.getProperty("line.separator");
    public static final String sSYSPROPS_FILE_SEPR = sSYSPROPS.getProperty("file.separator");
    public static final String sSYSPROPS_PATH_SEPR = sSYSPROPS.getProperty("path.separator");
    public static final String sSYSPROPS_JAVA_PATH = sSYSPROPS.getProperty("java.class.path");
    public static final String sSYSPROPS_JAVA_HOME = sSYSPROPS.getProperty("java.home");
    public static final String sSYSPROPS_JAVA_VEND = sSYSPROPS.getProperty("java.vendor");
    public static final String sSYSPROPS_JAVA_VURL = sSYSPROPS.getProperty("java.vendor.url");
    public static final String sSYSPROPS_OSYS_ARCH = sSYSPROPS.getProperty("os.arch");
    public static final String sSYSPROPS_OSYS_NAME = sSYSPROPS.getProperty("os.name");
    public static final String sSYSPROPS_OSYS_VERS = sSYSPROPS.getProperty("os.version");
    public static final String sSYSPROPS_USER_HDIR = sSYSPROPS.getProperty("user.dir");
    public static final String sSYSPROPS_USER_HOME = sSYSPROPS.getProperty("user.home");
    public static final String sSYSPROPS_USER_NAME = sSYSPROPS.getProperty("user.name");
    ////////////////////////////////////////////////////////////////////////////
    // LOGFILE BASE CONFIGURATION
    ////////////////////////////////////////////////////////////////////////////
    public static final String sLOGFILE_FILE_NAME = "./log/logs.txt";
    ////////////////////////////////////////////////////////////////////////////
    // LOGFILE BASE CONFIGURATION
    ////////////////////////////////////////////////////////////////////////////
    public static final String sNOVAFILE_FILE_NAME = "./log/nova.txt";
     ////////////////////////////////////////////////////////////////////////////
    // LOGFILE BASE CONFIGURATION
    ////////////////////////////////////////////////////////////////////////////
    public static final String sSOCKFILE_FILE_NAME = "./log/sock.txt";
    ////////////////////////////////////////////////////////////////////////////
    // PROLOG BASE CONFIGURATION
    ////////////////////////////////////////////////////////////////////////////
    public static final String sPROLOG_FILE_BASE = "./res/pl/*.pl";
    ////////////////////////////////////////////////////////////////////////////
    // URL RESOURCES
    ////////////////////////////////////////////////////////////////////////////
    public static final URL sSTYLESURL = sRESOURCE_CLASS.getResource("/res/sty/scripts.xml");
}
