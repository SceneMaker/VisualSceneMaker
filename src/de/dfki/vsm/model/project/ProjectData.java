package de.dfki.vsm.model.project;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.model.acticon.ActiconObject;
import de.dfki.vsm.model.configs.ConfigData;
import de.dfki.vsm.model.configs.ConfigEntry;
import de.dfki.vsm.model.configs.ProjectPreferences;
import de.dfki.vsm.model.gesticon.GesticonObject;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.script.SceneScript;
import de.dfki.vsm.model.visicon.VisiconObject;
import de.dfki.vsm.runtime.dialogact.DialogActInterface;
import de.dfki.vsm.runtime.dialogact.DummyDialogAct;
import de.dfki.vsm.runtime.player.DefaultDialogueActPlayer;
import de.dfki.vsm.runtime.player.DefaultSceneGroupPlayer;
import de.dfki.vsm.runtime.player.DialogueActPlayer;
import de.dfki.vsm.runtime.player.SceneGroupPlayer;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.plugin.Plugin;
import de.dfki.vsm.util.xml.XMLParseTools;

//~--- JDK imports ------------------------------------------------------------
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.Serializable;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import java.nio.file.Path;
import java.nio.file.Paths;

import java.util.HashMap;
import java.util.Map.Entry;
import java.util.zip.CRC32;
import java.util.zip.CheckedOutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * @author Gregor Mehlmann
 */
public class ProjectData implements Serializable {

    // The System Logger
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    // Flag if project is freshly created
    private boolean mIsPending = false;

    // Maintained Plugins
    private final HashMap<String, Plugin> mPluginList = new HashMap<>();

    // Project Information
    private final File mProjectBaseFile;
    private final File mProjectDirFile;
    private String mProjectFileName;
    private String mProjectDirPath;
    private String mProjectPathName;
    private String mProjectFullFileName;

    // Project Content
    private String mProjectName;
    private String mSceneFlowFileName;
    private String mSceneScriptFileName;
    private String mGesticonFileName;
    private String mVisiconFileName;
    private String mActiconFileName;
    private String mPreferencesFileName;

    // DialogueAct Content
    private String mDialogueActClassName;
    private String mDialogueActPlayerClassName;

    // ScenePlayer Content
    private String mScenePlayerClassName;
    private String mScenePlayerConfigFile;
    private final ConfigData mPlayerConfig;

    // According Properties
    private final ConfigData mProjectConfig;

    // Maintained Structures
    private final SceneFlow mSceneFlow;
    private final SceneScript mSceneScript;
    private final ActiconObject mActicon;
    private final GesticonObject mGesticon;
    private final VisiconObject mVisicon;

    // Maintained ScenePlayer
    private SceneGroupPlayer mScenePlayer;
    protected int mProjectInitialHash;
    private ProjectPreferences mProjectPreferences;

    // DialogueActInterface
    private DialogActInterface mDialogueAct;

    // The Dialogue Act Player
    private DialogueActPlayer mDialogueActPlayer;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public ProjectData(final File file) {

        // Initialize The File And Name Members
        mProjectBaseFile = file;
        mProjectFileName = file.getName();
        mProjectDirFile = mProjectBaseFile.getParentFile();
        mProjectDirPath = mProjectDirFile.getPath();
        mProjectPathName = mProjectDirPath + System.getProperty("file.separator");

        // Create Project and Player Property
        mProjectConfig = new ConfigData("ProjectConfig");
        mPlayerConfig = new ConfigData("PlayerConfig");

        // Load And Sort Project Properties
        loadProjectConfig();
        mProjectConfig.sort();

        // Read Project Properties
        mProjectName = mProjectConfig.property("project.basic.name");
        mProjectFullFileName = mProjectPathName + mProjectFileName;
        mSceneFlowFileName = mProjectPathName + mProjectConfig.property("project.data.sceneflow");
        mSceneScriptFileName = mProjectPathName + mProjectConfig.property("project.data.scenes");
        mGesticonFileName = mProjectPathName + mProjectConfig.property("project.data.gesticon");
        mVisiconFileName = mProjectPathName + mProjectConfig.property("project.data.visicon");
        mActiconFileName = mProjectPathName + mProjectConfig.property("project.data.acticon");
        mDialogueActClassName = mProjectConfig.property("project.dialogact.class");
        mDialogueActPlayerClassName = mProjectConfig.property("project.dialogact.player");

        // Added condition for legacy support for project independent preferences
        if (mProjectConfig.property("project.data.preferences") == null) {
            mPreferencesFileName = mProjectPathName + "preferences.xml";
        } else {
            mPreferencesFileName = mProjectPathName + mProjectConfig.property("project.data.preferences");
        }

        // Read Player Propertiesy
        mScenePlayerClassName = mProjectConfig.property("project.player.class");
        mScenePlayerConfigFile = mProjectPathName + mProjectConfig.property("project.player.config");

        // Load And Sort Player Properties
        loadPlayerConfig();
        mPlayerConfig.sort();

        // Load The Project Plugin Properties
        for (ConfigEntry entry : mProjectConfig.getEntryList()) {
            if (((String) entry.getKey()).startsWith("project.plugin.static.")) {
                String value = ((String) entry.getVal());

                mPluginList.put(value, null);
            }
        }

        // Finally Initialize The Project By
        // Create The Internal Data Structures
        mSceneFlow = new SceneFlow();
        mActicon = new ActiconObject();
        mVisicon = new VisiconObject();
        mGesticon = new GesticonObject();
        mSceneScript = new SceneScript();

        // Load The Internal Data Structures
        loadDataStructures();

        //
        loadDialogueAct();

        // Load Project Preferences
        mProjectPreferences = new ProjectPreferences();
        mProjectPreferences.load(mPreferencesFileName);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public ProjectData(final String name) {
        this(new File(name));
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized boolean save() {

        // Save Data Structures
        return (saveActicon() && saveVisicon() && saveGesticon() && saveSceneFlow() && saveSceneScript()
                && saveProjectConfig() && savePlayerConfig());
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized boolean loadDataStructures() {

        // Load Data Structures
        return (loadActicon() && loadVisicon() && loadGesticon() && loadSceneFlow() && loadSceneScript());
    }

    public final synchronized boolean createProject() {
        return false;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void setPending(final boolean state) {
        mIsPending = state;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized boolean loadProjectConfig() {

        // Check The Acticon File
        if (mProjectBaseFile.exists()) {

            // Parse The Acticon File
            XMLParseTools.parseFromXMLFile(mProjectConfig, mProjectBaseFile);

            // Print Some Debug Information
            mLogger.message("Loading Project Configuration\n" + mProjectConfig.toString());

            // Return At Parse Success
            return true;
        }

        // Return At Load Failure
        return false;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized boolean saveProjectConfig() {

        // Check The Acticon File
        if (mProjectBaseFile.exists()) {

            // Parse The Acticon File
            XMLParseTools.writeToXMLFile(mProjectConfig, mProjectBaseFile);

            // Print Some Debug Information
            mLogger.message("Saving Project Configuration\n" + mProjectConfig.toString());

            // Return At Parse Success
            return true;
        }

        // Return At Load Failure
        return false;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized boolean loadPlayerConfig() {

        // Create The Acticon File
        final File file = new File(mScenePlayerConfigFile);

        // Check The Acticon File
        if (file.exists()) {

            // Parse The Acticon File
            XMLParseTools.parseFromXMLFile(mPlayerConfig, file);

            // Print Some Debug Information
            mLogger.message("Loading Player Configuration\n" + mPlayerConfig.toString());

            // Return At Parse Success
            return true;
        }

        // Return At Load Failure
        return false;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized boolean savePlayerConfig() {

        // Create The Acticon File
        final File file = new File(mScenePlayerConfigFile);

        // Check The Acticon File
        if (file.exists()) {

            // Parse The Acticon File
            XMLParseTools.writeToXMLFile(mPlayerConfig, file);

            // Print Some Debug Information
            mLogger.message("Saving Player Configuration\n" + mPlayerConfig.toString());

            // Return At Parse Success
            return true;
        }

        // Return At Load Failure
        return false;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized boolean loadSceneFlow() {

        // Create The Acticon File
        final File file = new File(mSceneFlowFileName);

        // Check The Acticon File
        if (file.exists()) {

            // Parse The Acticon File
            XMLParseTools.parseFromXMLFile(mSceneFlow, file);

            // Establish Postprocessing
            mSceneFlow.establishStartNodes();
            mSceneFlow.establishTargetNodes();
            mSceneFlow.establishAltStartNodes();

            // Print Some Debug Information
            mLogger.message("Loading Sceneflow\n" + mSceneFlow.toString());

            // Return At Parse Success
            return true;
        }

        // Return At Load Failure
        return false;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized boolean saveSceneFlow() {

        // Create The Acticon File
        final File file = new File(mSceneFlowFileName);

        // Check The Acticon File
        if (file.exists()) {

            // Parse The Acticon File
            XMLParseTools.writeToXMLFile(mSceneFlow, file);

            // Print Some Debug Information
            mLogger.message("Saving Sceneflow\n" + mSceneFlow.toString());

            // Return At Parse Success
            return true;
        }

        // Return At Save Failure
        return false;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized boolean loadSceneScript() {

        // Create The Acticon File
        final File file = new File(mSceneScriptFileName);

        // Check The Acticon File
        if (file.exists()) {

            // Parse The Acticon File
            XMLParseTools.parseFromXMLFile(mSceneScript, file);

            // Print Some Debug Information
            mLogger.message("Loading SceneScript\n" + mSceneScript.toString());

            // Return At Parse Success
            return true;
        }

        // Return At Load Failure
        return false;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized boolean saveSceneScript() {

        // Create The Acticon File
        final File file = new File(mSceneScriptFileName);

        // Check The Acticon File
        if (file.exists()) {

            // Parse The Acticon File
            XMLParseTools.writeToXMLFile(mSceneScript, file);

            // Print Some Debug Information
            mLogger.message("Saving Scenescript\n" + mSceneScript.toString());

            // Return At Parse Success
            return true;
        }

        // Return At Save Failure
        return false;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized boolean loadActicon() {

        // Create The Acticon File
        final File file = new File(mActiconFileName);

        // Check The Acticon File
        if (file.exists()) {

            // Parse The Acticon File
            XMLParseTools.parseFromXMLFile(mActicon, file);

            // Print Some Debug Information
            mLogger.message("Loading Acticon\n" + mActicon.toString());

            // Return At Parse Success
            return true;
        }

        // Return At Load Failure
        return false;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized boolean saveActicon() {

        // Create The Acticon File
        final File file = new File(mActiconFileName);

        // Check The Acticon File
        if (file.exists()) {

            // Parse The Acticon File
            XMLParseTools.writeToXMLFile(mActicon, file);

            // Print Some Debug Information
            mLogger.message("Saving Acticon\n" + mActicon.toString());

            // Return At Parse Success
            return true;
        }

        // Return At Save Failure
        return false;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized boolean loadGesticon() {

        // Create The Acticon File
        final File file = new File(mGesticonFileName);

        // Check The Acticon File
        if (file.exists()) {

            // Parse The Acticon File
            XMLParseTools.parseFromXMLFile(mGesticon, file);

            // Print Some Debug Information
            mLogger.message("Loading Gesticon\n" + mGesticon.toString());

            // Return At Parse Success
            return true;
        }

        // Return At Load Failure
        return false;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized boolean saveGesticon() {

        // Create The Acticon File
        final File file = new File(mGesticonFileName);

        // Check The Acticon File
        if (file.exists()) {

            // Parse The Acticon File
            XMLParseTools.writeToXMLFile(mGesticon, file);

            // Print Some Debug Information
            mLogger.message("Saving Gesticon\n" + mGesticon.toString());

            // Return At Parse Success
            return true;
        }

        // Return At Save Failure
        return false;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized boolean loadVisicon() {

        // Create The Acticon File
        final File file = new File(mVisiconFileName);

        // Check The Acticon File
        if (file.exists()) {

            // Parse The Acticon File
            XMLParseTools.parseFromXMLFile(mVisicon, file);

            // Print Some Debug Information
            mLogger.message("Loading Visicon\n" + mVisicon.toString());

            // Return At Parse Success
            return true;
        }

        // Return At Load Failure
        return false;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized boolean saveVisicon() {

        // Create The Acticon File
        final File file = new File(mVisiconFileName);

        // Check The Acticon File
        if (file.exists()) {

            // Parse The Acticon File
            XMLParseTools.writeToXMLFile(mVisicon, file);

            // Print Some Debug Information
            mLogger.message("Saving Visicon\n" + mVisicon.toString());

            // Return At Parse Success
            return true;
        }

        // Return At Save Failure
        return false;
    }

    // TODO: Does actually not work correct!
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized boolean hasChanged() {
        boolean hasChanged = false;

        if (getSceneInitialHash() != getHashCode()) {
            hasChanged = true;
        }

        return hasChanged;

        /*
         * Remove commented code as comparison is now made through project hash
         *
         * String currentFileName = mProjectPathName + "~." + mProjectName + ".zip";
         * // Export the current version
         * exportZIP(currentFileName);
         * // Load the current version to a file
         * File currentFile = new File(currentFileName);
         * // Load the old version to a file
         * File zipFile = new File(mZipFileName);
         * // Compare the two versions
         * boolean hasChanged = false;
         * try {
         * hasChanged = !FileAttributes.compare(currentFile, zipFile);
         * } catch (IOException e) {
         * e.printStackTrace();
         * return true;
         * } finally {
         * try {
         * //currentFile.delete();
         * } catch (SecurityException e) {
         * e.printStackTrace();
         * }
         * }
         * return hasChanged;
         */
    }

    public final synchronized void loadDialogueAct() {
        if (mDialogueActClassName != null) {
            try {
                Class daClass = Class.forName(mDialogueActClassName);

                mDialogueAct = (DialogActInterface) daClass.getConstructor().newInstance();
            } catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | ClassNotFoundException | NoSuchMethodException | SecurityException ex) {

                // do nothing
            }
        } else {
            mDialogueAct = new DummyDialogAct();
        }
    }

    public final synchronized void loadDialogueActPlayer() {
        if (mDialogueActPlayerClassName != null) {
            try {
                Class daPlayerClass = Class.forName(mDialogueActPlayerClassName);

                mDialogueActPlayer
                        = (DialogueActPlayer) daPlayerClass.getConstructor(ProjectData.class).newInstance(this);
            } catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | ClassNotFoundException | NoSuchMethodException | SecurityException ex) {
                mDialogueActPlayer = new DefaultDialogueActPlayer(this);
            }
        } else {
            mDialogueActPlayer = new DefaultDialogueActPlayer(this);
        }

        mDialogueActPlayer.launch();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void loadScenePlayer() {

        // Try to load the plugin
        SceneGroupPlayer player = null;

        if (player == null) {
            try {
                Class playerClass = Class.forName(mScenePlayerClassName);

                player = (SceneGroupPlayer) playerClass.getConstructor(ProjectData.class).newInstance(this);
            } catch (Exception exc) {
                mLogger.warning(exc.toString());
            }
        }

        if (player == null) {
            try {
                Class playerClass = Class.forName(mScenePlayerClassName);
                Method methodone = playerClass.getMethod("getInstance", ProjectData.class);

                player = (SceneGroupPlayer) methodone.invoke(null, this);
            } catch (Exception exc) {
                mLogger.warning(exc.toString());
            }
        }

        if (player == null) {
            try {
                Class playerClass = Class.forName(mScenePlayerClassName);
                Method methodtwo = playerClass.getMethod("getInstance");

                player = (SceneGroupPlayer) methodtwo.invoke(null);
            } catch (Exception exc) {
                mLogger.warning(exc.toString());
            }
        }

        // Check if the plugin was loaded
        if (player == null) {
            player = new DefaultSceneGroupPlayer(this);
        }

        //
        mScenePlayer = player;

        //
        mScenePlayer.launch();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void loadPluginList() {
        for (ConfigEntry entry : mProjectConfig.getEntryList()) {
            if (((String) entry.getKey()).startsWith("project.plugin.static.")) {
                String value = ((String) entry.getVal());
                int lastDotIndex = value.lastIndexOf('.');

                // Get classname and port of the service
                String className = value.substring(0, lastDotIndex);
                int port = Integer.parseInt(value.substring(lastDotIndex + 1));

                // Try to load the plugin
                Plugin plugin = null;

                try {
                    Class pluginClass = Class.forName(className);
                    Method methodone = pluginClass.getMethod("getInstance", ProjectData.class);

                    plugin = (Plugin) methodone.invoke(null, this);
                } catch (Exception exc) {
                    System.err.println(exc.toString());
                }

                try {
                    Class pluginClass = Class.forName(className);
                    Method methodtwo = pluginClass.getMethod("getInstance");

                    plugin = (Plugin) methodtwo.invoke(null);
                } catch (Exception exc) {
                    System.err.println(exc.toString());
                }

                // Add the service
                mPluginList.put(value, plugin);

                // Launch The Plugin
                plugin.launch();
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void unloadScenePlayer() {

        // Print Server Info
        mScenePlayer.unload();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void unloadPluginList() {
        for (Entry<String, Plugin> entry : mPluginList.entrySet()) {

            // Get the service
            Plugin plugin = entry.getValue();

            // If service is running
            if (plugin != null) {

                // Unload the plugin
                plugin.unload();
            }
        }

        // Clear the list
        mPluginList.clear();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized File getProjectBaseFile() {
        return mProjectBaseFile;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized File getProjectDirFile() {
        return mProjectDirFile;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized String getProjectFileName() {
        return mProjectFileName;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized String getProjectDirPath() {
        return mProjectDirPath;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized String getProjectPathName() {
        return mProjectPathName;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized String getProjectName() {
        return mProjectName;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized String getSceneFlowFileName() {
        return mSceneFlowFileName;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized String getSceneScriptFileName() {
        return mSceneScriptFileName;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized String getGesticonFileName() {
        return mGesticonFileName;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized String getVisiconFileName() {
        return mVisiconFileName;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized ConfigData getProjectConfig() {
        return mProjectConfig;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized SceneFlow getSceneFlow() {
        return mSceneFlow;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized GesticonObject getGesticon() {
        return mGesticon;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized VisiconObject getVisicon() {
        return mVisicon;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized ActiconObject getSceneActions() {
        return mActicon;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized SceneGroupPlayer getScenePlayer() {
        return mScenePlayer;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized DialogueActPlayer getDialogueActPlayer() {
        return mDialogueActPlayer;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized ProjectPreferences getPreferences() {
        return mProjectPreferences;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized String getPreferencesFileName() {
        return mPreferencesFileName;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized String getScenePlayerClassName() {
        return mScenePlayerClassName;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized String getScenePlayerConfigFile() {
        return mScenePlayerConfigFile;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized ConfigData getScenePlayerProperties() {
        return mPlayerConfig;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized ConfigData getProjectProperties() {
        return mProjectConfig;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized DialogActInterface getDialogAct() {
        return mDialogueAct;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized boolean isPending() {
        return mIsPending;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized SceneScript getSceneScript() {
        return mSceneScript;
    }

    public void updateFileNames(String ProjectFileName, String ProjectDirPath) {
        Path path = Paths.get(ProjectDirPath);

        mProjectFileName = ProjectFileName;
        mProjectDirPath = path.getParent().toString();
        mProjectPathName = mProjectDirPath + System.getProperty("file.separator");

        // Read Project Properties
        mProjectName = mProjectConfig.property("project.basic.name");
        mProjectFullFileName = mProjectPathName + mProjectFileName;
        mSceneFlowFileName = mProjectPathName + mProjectConfig.property("project.data.sceneflow");
        mSceneScriptFileName = mProjectPathName + mProjectConfig.property("project.data.scenes");
        mGesticonFileName = mProjectPathName + mProjectConfig.property("project.data.gesticon");
        mVisiconFileName = mProjectPathName + mProjectConfig.property("project.data.visicon");
        mActiconFileName = mProjectPathName + mProjectConfig.property("project.data.acticon");

        // Read Player Propertiesy
        mScenePlayerClassName = mProjectConfig.property("project.player.class");
        mScenePlayerConfigFile = mProjectPathName + mProjectConfig.property("project.player.config");

    }

    public synchronized void setSceneInitialHash(int value) {
        mProjectInitialHash = value;
    }

    public synchronized int getSceneInitialHash() {
        return mProjectInitialHash;
    }

    public synchronized int getHashCode() {
        int hashCode = ((mSceneFlow == null)
                ? 0
                : mSceneFlow.getHashCode());

        return hashCode;
    }
}
