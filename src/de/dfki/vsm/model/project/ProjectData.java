package de.dfki.vsm.model.project;

import de.dfki.vsm.model.acticon.ActiconObject;
import de.dfki.vsm.model.config.ConfigData;
import de.dfki.vsm.model.config.ConfigEntry;
import de.dfki.vsm.model.config.ProjectPreferences;
import de.dfki.vsm.model.gesticon.GesticonObject;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.script.SceneScript;
import de.dfki.vsm.model.visicon.VisiconObject;
import de.dfki.vsm.runtime.dialogact.DialogActInterface;
import de.dfki.vsm.runtime.dialogact.DummyDialogAct;
import de.dfki.vsm.runtime.player.DefaultDialogueActPlayer;
import de.dfki.vsm.runtime.player.DefaultSceneGroupPlayer;
import de.dfki.vsm.runtime.player.Player;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.runtime.plugin.Plugin;
import de.dfki.vsm.util.xml.XMLUtilities;
import java.io.File;
import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;

/**
 * @author Gregor Mehlmann
 */
public class ProjectData implements Serializable {

    // The Logger Instance
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();
    // Project Information
    private String mConfigFileName;
    private String mConfigPathName;
    // Project File Names
    private String mProjectName;
    private String mPluginsFileName;
    private String mPlayersFileName;
    private String mGesticonFileName;
    private String mVisiconFileName;
    private String mActiconFileName;
    private String mSceneFlowFileName;
    private String mSceneScriptFileName;
    private String mPreferencesFileName;

    // Maintained Structures
    private final SceneFlow mSceneFlow = new SceneFlow();
    private final SceneScript mSceneScript = new SceneScript();
    private final ActiconObject mActicon = new ActiconObject();
    private final GesticonObject mGesticon = new GesticonObject();
    private final VisiconObject mVisicon = new VisiconObject();
    // Project Configurations
    private final ConfigData mProjectConfig = new ConfigData("Project");
    private final ConfigData mPlayersConfig = new ConfigData("Players");
    private final ConfigData mPluginsConfig = new ConfigData("Plugins");
    private final ConfigData mDPlayerConfig = new ConfigData("Player");
    // Maintained Plugins
    private final HashMap<String, Plugin> mPluginList = new HashMap<>();
    // Maintained Players
    private final HashMap<String, Player> mPlayerList = new HashMap<>();

    // DialogueAct Content
    private String mDialogueActClassName;
    private String mDialogueActPlayerClassName;
    // ScenePlayer Content
    private String mScenePlayerClassName;
    private String mScenePlayerConfigFile;
    // Maintained ScenePlayer
    private Player mScenePlayer;
    protected int mProjectInitialHash;
    private ProjectPreferences mProjectPreferences;
    // DialogueActInterface
    private DialogActInterface mDialogueAct;
    // The Dialogue Act Player
    private Player mDialogueActPlayer;
    // Freshly Created Flag
    private boolean mIsPending = false;

    ////////////////////////////////////////////////////////////////////////////
    public ProjectData(final File file) {

        // Init File And Path  
        mConfigFileName = file.getAbsolutePath();
        mConfigPathName = file.getParentFile().getAbsolutePath();

        // Print Some Information
        mLogger.message("Creating Project '" + mConfigFileName + "'");

        // Load The Project Content
        load();

        // TODO: Clean
        // Load Project Preferences
        mProjectPreferences = new ProjectPreferences();
        mProjectPreferences.load(mPreferencesFileName);

    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void load() {
        loadConfig();
        loadPlayers();
        loadPlugins();
        loadActicon();
        loadVisicon();
        loadGesticon();
        loadSceneFlow();
        loadSceneScript();
        // TODO: Clean
        loadDefaultScenePlayer();
        loadDialogueAct();
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void save() {
        saveConfig();
        savePlayers();
        savePlugins();
        saveActicon();
        saveVisicon();
        saveGesticon();
        saveSceneFlow();
        saveSceneScript();
        // TODO: Clean
        saveDefaultScenePlayer();
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void loadConfig() {
        // Create The File
        final File file = new File(mConfigFileName);
        // Check The File
        if (file.exists()) {
            // Parse The File
            XMLUtilities.parseFromXMLFile(mProjectConfig, file);
            // Print Information
            mLogger.message("Loading Project Configuration\n" + mProjectConfig.toString());
            // Get Project Path
            final String path = mConfigPathName + System.getProperty("file.separator");
            // Get Project Name
            mProjectName = mProjectConfig.property("project.name");
            // Get Project Data            
            mPluginsFileName = path + mProjectConfig.property("project.plugins");
            mPlayersFileName = path + mProjectConfig.property("project.players");
            mActiconFileName = path + mProjectConfig.property("project.acticon");
            mVisiconFileName = path + mProjectConfig.property("project.visicon");
            mGesticonFileName = path + mProjectConfig.property("project.gesticon");
            mSceneFlowFileName = path + mProjectConfig.property("project.sceneflow");
            mSceneScriptFileName = path + mProjectConfig.property("project.scenescript");

            // TODO: Clean
            // Added condition for legacy support for project independent preferences
            if (mProjectConfig.property("project.data.preferences") == null) {
                mPreferencesFileName = path + "preferences.xml";
            } else {
                mPreferencesFileName = path + mProjectConfig.property("project.data.preferences");
            }

            // Read Player Propertiesy   // TODO: Clean
            mScenePlayerClassName = mProjectConfig.property("project.player.class");
            mScenePlayerConfigFile = path + mProjectConfig.property("project.player.config");
            // Get Project ...   // TODO: Clean
            mDialogueActClassName = mProjectConfig.property("project.dialogact.class");
            mDialogueActPlayerClassName = mProjectConfig.property("project.dialogact.player");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void saveConfig() {
        // Create The File
        final File file = new File(mConfigFileName);
        // Check The File
        if (file.exists()) {
            // Write The File
            XMLUtilities.writeToXMLFile(mProjectConfig, file);
            // Print Information
            mLogger.message("Saving Project Configuration\n" + mProjectConfig.toString());
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void loadDefaultScenePlayer() {
        // Create The File
        final File file = new File(mScenePlayerConfigFile);
        // Check The File
        if (file.exists()) {
            // Parse The File
            XMLUtilities.parseFromXMLFile(mDPlayerConfig, file);
            // Print Information 
            mLogger.message("Loading Default Scene Player Configuration\n" + mDPlayerConfig.toString());
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void saveDefaultScenePlayer() {
        // Create The File
        final File file = new File(mScenePlayerConfigFile);
        // Check The File
        if (file.exists()) {
            // Parse The File
            XMLUtilities.writeToXMLFile(mDPlayerConfig, file);
            // Print Information
            mLogger.message("Saving Default Scene Player Configuration\n" + mDPlayerConfig.toString());
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void loadPlayers() {
        // Create The Config File
        final File file = new File(mPlayersFileName);
        // Check The Config File
        if (file.exists()) {
            // Parse The Config File
            XMLUtilities.parseFromXMLFile(mPlayersConfig, file);
            // Print Some Information 
            mLogger.message("Loading Players Configuration\n" + mPlayersConfig.toString());
            // Get Individual Plugins
            for (final ConfigEntry entry : mPlayersConfig.getEntryList()) {
                // Get Name And File
                final String key = ((String) entry.getKey());
                final String val = ((String) entry.getVal());
                // Get The Config File
                final File base = new File(val);
                // Check The Config File
                if (base.exists()) {
                    // Get Plugin Config File
                    final ConfigData data = new ConfigData("Player");
                    // Parse The Config File
                    XMLUtilities.parseFromXMLFile(data, base);
                    // Print Some Information 
                    mLogger.message("Loading Player Configuration\n" + data.toString());
                    // Get Plugin Class Name
                    final String name = data.property("class");
                    // Check Plugin Class Name
                    if (name != null) {
                        // Try To Load Plugin
                        try {
                            // Find The Class
                            final Class clazz = Class.forName(name);
                            // Call Constructor
                            final Player player = (Player) clazz.getConstructor(ProjectData.class, ConfigData.class).newInstance(this, data);
                            // Add The Plugin
                            mPlayerList.put(key, player);
                            // Print Some Information 
                            mLogger.message("Registering Player Name '" + key + "' With Player Object '" + player + "'");
                        } catch (Exception exc) {
                            mLogger.warning(exc.toString());
                        }
                    }
                } else {
                    // Print Some Information 
                    mLogger.warning("Missing Player Configuration\n" + base.getAbsolutePath());
                }
            }
        } else {
            // Print Some Information 
            mLogger.warning("Missing Players Configuration\n" + file.getAbsolutePath());
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void savePlayers() {
        // Create The File
        final File file = new File(mPlayersFileName);
        // Check The File
        if (file.exists()) {
            // Parse The File
            XMLUtilities.writeToXMLFile(mPlayersConfig, file);
            // Print Information
            mLogger.message("Saving Players Configuration\n" + mPlayersConfig.toString());
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void loadPlugins() {
        // Create The Config File
        final File file = new File(mPluginsFileName);
        // Check The Config File
        if (file.exists()) {
            // Parse The Config File
            XMLUtilities.parseFromXMLFile(mPluginsConfig, file);
            // Print Some Information 
            mLogger.message("Loading Plugins Configuration\n" + mPluginsConfig.toString());
            // Get Individual Plugins
            for (final ConfigEntry entry : mPluginsConfig.getEntryList()) {
                // Get Name And File
                final String key = ((String) entry.getKey());
                final String val = ((String) entry.getVal());
                // Get The Config File
                final File base = new File(val);
                // Check The Config File
                if (base.exists()) {
                    // Get Plugin Config File
                    final ConfigData data = new ConfigData("Plugin");
                    // Parse The Config File
                    XMLUtilities.parseFromXMLFile(data, base);
                    // Print Some Information 
                    mLogger.message("Loading Plugin Configuration\n" + data.toString());
                    // Get Plugin Class Name
                    final String name = data.property("class");
                    // Check Plugin Class Name
                    if (name != null) {
                        // Try To Load Plugin
                        try {
                            // Find The Class
                            final Class clazz = Class.forName(name);
                            // Call Constructor
                            final Plugin plugin = (Plugin) clazz.getConstructor(ProjectData.class, ConfigData.class).newInstance(this, data);
                            // Add The Plugin
                            mPluginList.put(key, plugin);
                            // Print Some Information 
                            mLogger.message("Registering Plugin Name '" + key + "' With Plugin Object '" + plugin + "'");
                        } catch (Exception exc) {
                            mLogger.warning(exc.toString());
                        }
                    }
                } else {
                    // Print Some Information 
                    mLogger.warning("Missing Plugin Configuration\n" + base.getAbsolutePath());
                }
            }
        } else {
            // Print Some Information 
            mLogger.warning("Missing Plugins Configuration\n" + file.getAbsolutePath());
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void savePlugins() {
        // Create The File
        final File file = new File(mPluginsFileName);
        // Check The File
        if (file.exists()) {
            // Parse The File
            XMLUtilities.writeToXMLFile(mPluginsConfig, file);
            // Print Information
            mLogger.message("Saving Plugins Configuration\n" + mPluginsConfig.toString());
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void loadSceneFlow() {
        // Create The File
        final File file = new File(mSceneFlowFileName);
        // Check The File
        if (file.exists()) {
            // Parse The File
            XMLUtilities.parseFromXMLFile(mSceneFlow, file);
            // Postprocessing
            mSceneFlow.establishStartNodes();
            mSceneFlow.establishTargetNodes();
            mSceneFlow.establishAltStartNodes();
            // Print Information
            mLogger.message("Loading Sceneflow\n" + mSceneFlow.toString());
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void saveSceneFlow() {
        // Create The File
        final File file = new File(mSceneFlowFileName);
        // Check The File
        if (file.exists()) {
            // Parse The File
            XMLUtilities.writeToXMLFile(mSceneFlow, file);
            // Print Information
            mLogger.message("Saving Sceneflow\n" + mSceneFlow.toString());
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void loadSceneScript() {
        // Create The File
        final File file = new File(mSceneScriptFileName);
        // Check The File
        if (file.exists()) {
            // Parse The File
            XMLUtilities.parseFromXMLFile(mSceneScript, file);
            // Print Information
            mLogger.message("Loading SceneScript\n" + mSceneScript.toString());
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void saveSceneScript() {
        // Create The File
        final File file = new File(mSceneScriptFileName);
        // Check The File
        if (file.exists()) {
            // Parse The File
            XMLUtilities.writeToXMLFile(mSceneScript, file);
            // Print Information
            mLogger.message("Saving Scenescript\n" + mSceneScript.toString());
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void loadActicon() {
        // Create The File
        final File file = new File(mActiconFileName);
        // Check The File
        if (file.exists()) {
            // Parse The File
            XMLUtilities.parseFromXMLFile(mActicon, file);
            // Print Information
            mLogger.message("Loading Acticon\n" + mActicon.toString());
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void saveActicon() {
        // Create The File
        final File file = new File(mActiconFileName);
        // Check The File
        if (file.exists()) {
            // Parse The File
            XMLUtilities.writeToXMLFile(mActicon, file);
            // Print Information
            mLogger.message("Saving Acticon\n" + mActicon.toString());
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void loadGesticon() {
        // Create The File
        final File file = new File(mGesticonFileName);
        // Check The File
        if (file.exists()) {
            // Parse The File
            XMLUtilities.parseFromXMLFile(mGesticon, file);
            // Print Information
            mLogger.message("Loading Gesticon\n" + mGesticon.toString());
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void saveGesticon() {
        // Create The File
        final File file = new File(mGesticonFileName);
        // Check The File
        if (file.exists()) {
            // Parse The File
            XMLUtilities.writeToXMLFile(mGesticon, file);
            // Print Information
            mLogger.message("Saving Gesticon\n" + mGesticon.toString());
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void loadVisicon() {
        // Create The File
        final File file = new File(mVisiconFileName);
        // Check The File
        if (file.exists()) {
            // Parse The File
            XMLUtilities.parseFromXMLFile(mVisicon, file);
            // Print Information
            mLogger.message("Loading Visicon\n" + mVisicon.toString());
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void saveVisicon() {
        // Create The File
        final File file = new File(mVisiconFileName);
        // Check The File
        if (file.exists()) {
            // Parse The File
            XMLUtilities.writeToXMLFile(mVisicon, file);
            // Print Information
            mLogger.message("Saving Visicon\n" + mVisicon.toString());
        }
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
    }

    public final synchronized void loadDialogueAct() {
        if (mDialogueActClassName != null) {
            try {
                Class daClass = Class.forName(mDialogueActClassName);

                mDialogueAct = (DialogActInterface) daClass.getConstructor().newInstance();
            } catch (Exception exc) {

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
                        = (Player) daPlayerClass.getConstructor(ProjectData.class).newInstance(this);
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
    public final synchronized void launchDefaultScenePlayer() {

        // Try to load the plugin
        Player player = null;

        if (player == null) {
            try {
                Class playerClass = Class.forName(mScenePlayerClassName);

                player = (Player) playerClass.getConstructor(ProjectData.class).newInstance(this);
            } catch (Exception exc) {
                mLogger.warning(exc.toString());
            }
        }

        if (player == null) {
            try {
                Class playerClass = Class.forName(mScenePlayerClassName);
                Method methodone = playerClass.getMethod("getInstance", ProjectData.class);

                player = (Player) methodone.invoke(null, this);
            } catch (Exception exc) {
                mLogger.warning(exc.toString());
            }
        }

        if (player == null) {
            try {
                Class playerClass = Class.forName(mScenePlayerClassName);
                Method methodtwo = playerClass.getMethod("getInstance");

                player = (Player) methodtwo.invoke(null);
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

        // Launch The Player
        mScenePlayer.launch();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void unloadDefaultScenePlayer() {
        // Unload The Player
        mScenePlayer.unload();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void launchPlayerList() {
        for (final Player player : mPlayerList.values()) {
            // Launch The Player
            player.launch();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void unloadPlayerList() {
        for (final Player player : mPlayerList.values()) {
            // Unload The Player
            player.unload();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void launchPluginList() {
        for (final Plugin plugin : mPluginList.values()) {
            // Launch The Plugin
            plugin.launch();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void unloadPluginList() {
        for (final Plugin plugin : mPluginList.values()) {
            // Unload The Plugin
            plugin.unload();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized String getProjectFileName() {
        return mConfigFileName;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized String getProjectDirPath() {
        return mConfigPathName;
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
    public final synchronized Player getScenePlayer() {
        return mScenePlayer;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized Player getDialogueActPlayer() {
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
        return mDPlayerConfig;
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
    public final synchronized void setPending(final boolean state) {
        mIsPending = state;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized SceneScript getSceneScript() {
        return mSceneScript;
    }

    public void setProjectFileName(String ProjectFileName, String ProjectDirPath) {
        Path path = Paths.get(ProjectDirPath);

        mConfigFileName = ProjectFileName;
        mConfigPathName = path.getParent().toString();

        // Read Project Properties
        mProjectName = mProjectConfig.property("project.basic.name");

        mSceneFlowFileName = mConfigPathName + System.getProperty("file.separator") + mProjectConfig.property("project.data.sceneflow");
        mSceneScriptFileName = mConfigPathName + System.getProperty("file.separator") + mProjectConfig.property("project.data.scenes");
        mGesticonFileName = mConfigPathName + System.getProperty("file.separator") + mProjectConfig.property("project.data.gesticon");
        mVisiconFileName = mConfigPathName + System.getProperty("file.separator") + mProjectConfig.property("project.data.visicon");
        mActiconFileName = mConfigPathName + System.getProperty("file.separator") + mProjectConfig.property("project.data.acticon");

        // Read Player Propertiesy
        mScenePlayerClassName = mProjectConfig.property("project.player.class");
        mScenePlayerConfigFile = mConfigPathName + System.getProperty("file.separator") + mProjectConfig.property("project.player.config");

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
