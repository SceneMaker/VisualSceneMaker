package de.dfki.vsm.runtime.project;

import de.dfki.vsm.model.acticon.ActiconConfig;
import de.dfki.vsm.model.config.ConfigElement;
import de.dfki.vsm.model.project.ProjectConfig;
import de.dfki.vsm.model.gesticon.GesticonConfig;
import de.dfki.vsm.model.project.PlayerConfig;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.model.visicon.VisiconConfig;
import de.dfki.vsm.runtime.dialogact.DialogActInterface;
import de.dfki.vsm.runtime.dialogact.DummyDialogAct;
import de.dfki.vsm.runtime.player.DefaultDialogueActPlayer;
import de.dfki.vsm.runtime.player.Player;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.runtime.plugin.Plugin;
import de.dfki.vsm.util.xml.XMLUtilities;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;

/**
 * @author Gregor Mehlmann
 */
public class RunTimeProject {

    // The Logger Instance
    protected final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    private final SceneFlow mSceneFlow = new SceneFlow();
    private final SceneScript mSceneScript = new SceneScript();
    private final PlayerConfig mPlayerConfig = new PlayerConfig();
    private final ProjectConfig mProjectConfig = new ProjectConfig();
    private final ActiconConfig mActiconConfig = new ActiconConfig();
    private final VisiconConfig mVisiconConfig = new VisiconConfig();
    private final GesticonConfig mGesticonConfig = new GesticonConfig();

    // Maintained Plugins
    private final HashMap<String, Plugin> mPluginMap = new HashMap<>();
    // Maintained Players
    private final HashMap<String, Player> mPlayerMap = new HashMap<>();

    // Default DialogPlayer
    private Player mDialogPlayer;
    // TODO:  Refactor The Dialog Act Interface
    private DialogActInterface mDialogueAct = new DummyDialogAct();
    private String mDialogActClassName;
    private String mDialogPlayerClassName;

    // Default ScenePlayer
    private Player mDefaultScenePlayer;
//    private String mScenePlayerClassName;
//    private String mScenePlayerFileName;

    public RunTimeProject() {
    }

    public RunTimeProject(final File file) {
        load(file);
    }

    public final ActiconConfig getActicon() {
        return mActiconConfig;
    }

    public final VisiconConfig getVisicon() {
        return mVisiconConfig;
    }

    public final GesticonConfig getGesticon() {
        return mGesticonConfig;
    }

    public final SceneFlow getSceneFlow() {
        return mSceneFlow;
    }

    public final SceneScript getSceneScript() {
        return mSceneScript;
    }

    public final Player getDefaultScenePlayer() {
        return mDefaultScenePlayer;
    }

    public final Player getDefaultDialogPlayer() {
        return mDialogPlayer;
    }

    public final ConfigElement getPlayerConfig() {
        return mPlayerConfig;
    }

    public final DialogActInterface getDialogAct() {
        return mDialogueAct;
    }

    public final Plugin getPlugin(final String name) {
        return mPluginMap.get(name);
    }

    public final Player getPlayer(final String name) {
        return mPlayerMap.get(name);
    }

    public final String getProjectName() {
        return mProjectConfig.getProjectName();
    }

    public final void setProjectName(final String name) {
        mProjectConfig.setProjectName(name);
    }

    public boolean load(final File file) {
        // Get the absolute file for this directory
        final File base = file.getAbsoluteFile();
        // Check if the project directory does exist
        if (!base.exists()) {
            // Print an error message in this case
            mLogger.failure("Error: Cannot find runtime project base directory '" + base + "'");
            // Return failure if it does not exist
            return false;
        }

        // Load the project from the configuration
        return loadProjectConfig(base)
                && loadSceneFlow(base)
                && loadSceneScript(base)
                && loadPlayerConfig(base)
                && loadActiconConfig(base)
                && loadVisiconConfig(base)
                && loadGesticonConfig(base)
                && loadDefaultScenePlayer();

        /*       
         loadPlayers();
         loadPlugins();        
         // TODO: Clean
         loadScenePlayer();
         loadDialogPlayer();
         loadDialogueAct();
         */
    }

    public boolean save(final File file) {
        // Get the absolute file for the directory
        final File base = file.getAbsoluteFile();
        // Check if the base directory does exist
        if (!base.exists()) {
            // Print a warning message in this case
            mLogger.warning("Warning: Creating the new runtime project base directory '" + base + "'");
            // Try to create a project base directory
            if (!base.mkdir()) {
                // Print an error message in this case
                mLogger.failure("Failure: Cannot create the new runtime project base directory '" + base + "'");
                // Return failure if it does not exist
                return false;
            }
        }
        // Save the project to the base directory
        return (saveProjectConfig(base)
                && saveSceneFlow(base)
                && saveSceneScript(base)
                && savePlayerConfig(base)
                && saveActiconConfig(base)
                && saveVisiconConfig(base)
                && saveGesticonConfig(base));

        /*
         saveProject();
         savePlayers();
         savePlugins();
         saveActicon();
         saveVisicon();
         saveGesticon();
         saveSceneFlow();
         saveSceneScript();
         // TODO: Clean
         saveScenePlayer();
         */
    }

    private boolean loadProjectConfig(final File base) {
        // Create the project configuration file
        final File file = new File(base, "project.xml");
        // Check if the  configuration does exist
        if (!file.exists()) {
            // Print an error message in this case
            mLogger.failure("Error: Cannot find project configuration file '" + file + "'");
            // Return failure if it does not exist
            return false;
        }
        // Parse the project configuration file
        if (!XMLUtilities.parseFromXMLFile(mProjectConfig, file)) {
            // Print an error message in this case
            mLogger.failure("Error: Cannot parse project configuration file '" + file + "'");
            // Return failure if it does not exist
            return false;
        }
        // Print an information message in this case
        mLogger.message("Loaded project configuration file '" + file + "':\n" + mProjectConfig);
        // Return success if the project was loaded
        return true;
    }

    private boolean saveProjectConfig(final File base) {
        // Create the project configuration file
        final File file = new File(base, "project.xml");
        // Check if the configuration does exist
        if (!file.exists()) {
            // Print a warning message in this case
            mLogger.warning("Warning: Creating the new project configuration file '" + file + "'");
            // Create a new configuration file now
            try {
                // Try to create a new configuration file
                if (!file.createNewFile()) {
                    // Print an error message in this case
                    mLogger.warning("Warning: There already exists a project configuration file '" + file + "'");
                }
            } catch (final IOException exc) {
                // Print an error message in this case
                mLogger.failure("Failure: Cannot create the new project configuration file '" + file + "'");
                // Return failure if it does not exist
                return false;
            }
        }
        // Write the project configuration file
        if (!XMLUtilities.writeToXMLFile(mProjectConfig, file)) {
            // Print an error message in this case
            mLogger.failure("Error: Cannot write project configuration file '" + file + "'");
            // Return failure if it does not exist
            return false;
        }
        // Print an information message in this case
        mLogger.message("Saved project configuration file '" + file + "':\n" + mProjectConfig);
        // Return success if the project was saved
        return true;
    }

    private boolean loadSceneFlow(final File base) {
        // Create the sceneflow configuration file
        final File file = new File(base, "sceneflow.xml");
        // Check if the configuration file does exist
        if (!file.exists()) {
            // Print an error message in this case
            mLogger.failure("Error: Cannot find sceneflow configuration file '" + file + "'");
            // Return failure if it does not exist
            return false;
        }
        // Parse the sceneflow configuration file
        if (!XMLUtilities.parseFromXMLFile(mSceneFlow, file)) {
            // Print an error message in this case
            mLogger.failure("Error: Cannot parse sceneflow configuration file '" + file + "'");
            // Return failure if it does not exist
            return false;
        }
        // Perform all the postprocessing steps
        mSceneFlow.establishStartNodes();
        mSceneFlow.establishTargetNodes();
        mSceneFlow.establishAltStartNodes();
        // Print an information message in this case
        mLogger.message("Loaded sceneflow configuration file '" + file + "':\n" + mSceneFlow);
        // Return success if the project was loaded
        return true;
    }

    private boolean saveSceneFlow(final File base) {
        // Create the sceneflow configuration file
        final File file = new File(base, "sceneflow.xml");
        // Check if the configuration file does exist
        if (!file.exists()) {
            // Print a warning message in this case
            mLogger.warning("Warning: Creating the new sceneflow configuration file '" + file + "'");
            // Create a new configuration file now
            try {
                // Try to create a new configuration file
                if (!file.createNewFile()) {
                    // Print an error message in this case
                    mLogger.warning("Warning: There already exists a sceneflow configuration file '" + file + "'");
                }
            } catch (final IOException exc) {
                // Print an error message in this case
                mLogger.failure("Failure: Cannot create the new sceneflow configuration file '" + file + "'");
                // Return failure if it does not exist
                return false;
            }
        }
        // Write the sceneflow configuration file
        if (!XMLUtilities.writeToXMLFile(mSceneFlow, file)) {
            // Print an error message in this case
            mLogger.failure("Error: Cannot write sceneflow configuration file '" + file + "'");
            // Return failure if it does not exist
            return false;
        }
        // Print an information message in this case
        mLogger.message("Saved sceneflow configuration file '" + file + "':\n" + mSceneFlow);
        // Return success if the project was saved
        return true;
    }

    private boolean loadSceneScript(final File base) {
        // Create the scenescript configuration file
        final File file = new File(base, "scenescript.xml");
        // Check if the configuration file does exist
        if (!file.exists()) {
            // Print an error message in this case
            mLogger.failure("Error: Cannot find scenescript configuration file '" + file + "'");
            // Return failure if it does not exist
            return false;
        }
        // Parse the scenescript configuration file
        if (!XMLUtilities.parseFromXMLFile(mSceneScript, file)) {
            // Print an error message in this case
            mLogger.failure("Error: Cannot parse scenescript configuration file '" + file + "'");
            // Return failure if it does not exist
            return false;
        }
        // Print an information message in this case
        mLogger.message("Loaded scenescript configuration file '" + file + "':\n" + mSceneScript);
        // Return success if the project was loaded
        return true;
    }

    private boolean saveSceneScript(final File base) {
        // Create the scenescript configuration file
        final File file = new File(base, "scenescript.xml");
        // Check if the configuration file does exist
        if (!file.exists()) {
            // Print a warning message in this case
            mLogger.warning("Warning: Creating the new scenescript configuration file '" + file + "'");
            // Create a new configuration file now
            try {
                // Try to create a new configuration file
                if (!file.createNewFile()) {
                    // Print an error message in this case
                    mLogger.warning("Warning: There already exists a scenescript configuration file '" + file + "'");
                }
            } catch (final IOException exc) {
                // Print an error message in this case
                mLogger.failure("Failure: Cannot create the new scenescript configuration file '" + file + "'");
                // Return failure if it does not exist
                return false;
            }
        }
        // Write the scenescript configuration file
        if (!XMLUtilities.writeToXMLFile(mSceneScript, file)) {
            // Print an error message in this case
            mLogger.failure("Error: Cannot write scenescript configuration file '" + file + "'");
            // Return failure if it does not exist
            return false;
        }
        // Print an information message in this case
        mLogger.message("Saved scenescript configuration file '" + file + "':\n" + mSceneScript);
        // Return success if the project was saved
        return true;
    }

    private boolean loadActiconConfig(final File base) {
        // Create the acticon configuration file
        final File file = new File(base, "acticon.xml");
        // Check if the configuration file does exist
        if (!file.exists()) {
            // Print an error message in this case
            mLogger.failure("Error: Cannot find acticon configuration file '" + file + "'");
            // Return failure if it does not exist
            return false;
        }
        // Parse the acticon configuration file
        if (!XMLUtilities.parseFromXMLFile(mActiconConfig, file)) {
            // Print an error message in this case
            mLogger.failure("Error: Cannot parse acticon configuration file '" + file + "'");
            // Return failure if it does not exist
            return false;
        }
        // Print an information message in this case
        mLogger.message("Loaded acticon configuration file '" + file + "':\n" + mActiconConfig);
        // Return success if the project was loaded
        return true;
    }

    private boolean saveActiconConfig(final File base) {
        // Create the acticon configuration file
        final File file = new File(base, "acticon.xml");
        // Check if the configuration file does exist
        if (!file.exists()) {
            // Print a warning message in this case
            mLogger.warning("Warning: Creating the new acticon configuration file '" + file + "'");
            // Create a new configuration file now
            try {
                // Try to create a new configuration file
                if (!file.createNewFile()) {
                    // Print an error message in this case
                    mLogger.warning("Warning: There already exists a acticon configuration file '" + file + "'");
                }
            } catch (final IOException exc) {
                // Print an error message in this case
                mLogger.failure("Failure: Cannot create the new acticon configuration file '" + file + "'");
                // Return failure if it does not exist
                return false;
            }
        }
        // Write the acticon configuration file
        if (!XMLUtilities.writeToXMLFile(mActiconConfig, file)) {
            // Print an error message in this case
            mLogger.failure("Error: Cannot write acticon configuration file '" + file + "'");
            // Return failure if it does not exist
            return false;
        }
        // Print an information message in this case
        mLogger.message("Saved acticon configuration file '" + file + "':\n" + mActiconConfig);
        // Return success if the project was saved
        return true;
    }

    private boolean loadGesticonConfig(final File base) {
        // Create the gesticon configuration file
        final File file = new File(base, "gesticon.xml");
        // Check if the configuration file does exist
        if (!file.exists()) {
            // Print an error message in this case
            mLogger.failure("Error: Cannot find gesticon configuration file '" + file + "'");
            // Return failure if it does not exist
            return false;
        }
        // Parse the gesticon configuration file
        if (!XMLUtilities.parseFromXMLFile(mGesticonConfig, file)) {
            // Print an error message in this case
            mLogger.failure("Error: Cannot parse gesticon configuration file '" + file + "'");
            // Return failure if it does not exist
            return false;
        }
        // Print an information message in this case
        mLogger.message("Loaded gesticon configuration file '" + file + "':\n" + mGesticonConfig);
        // Return success if the project was loaded
        return true;
    }

    private boolean saveGesticonConfig(final File base) {
        // Create the gesticon configuration file
        final File file = new File(base, "gesticon.xml");
        // Check if the configuration file does exist
        if (!file.exists()) {
            // Print a warning message in this case
            mLogger.warning("Warning: Creating the new gesticon configuration file '" + file + "'");
            // Create a new configuration file now
            try {
                // Try to create a new configuration file
                if (!file.createNewFile()) {
                    // Print an error message in this case
                    mLogger.warning("Warning: There already exists a gesticon configuration file '" + file + "'");
                }
            } catch (final IOException exc) {
                // Print an error message in this case
                mLogger.failure("Failure: Cannot create the new gesticon configuration file '" + file + "'");
                // Return failure if it does not exist
                return false;
            }
        }
        // Write the gesticon configuration file
        if (!XMLUtilities.writeToXMLFile(mGesticonConfig, file)) {
            // Print an error message in this case
            mLogger.failure("Error: Cannot write gesticon configuration file '" + file + "'");
            // Return failure if it does not exist
            return false;
        }
        // Print an information message in this case
        mLogger.message("Saved gesticon configuration file '" + file + "':\n" + mGesticonConfig);
        // Return success if the project was saved
        return true;
    }

    private boolean loadVisiconConfig(final File base) {
        // Create the visicon configuration file
        final File file = new File(base, "visicon.xml");
        // Check if the configuration file does exist
        if (!file.exists()) {
            // Print an error message in this case
            mLogger.failure("Error: Cannot find visicon configuration file '" + file + "'");
            // Return failure if it does not exist
            return false;
        }
        // Parse the visicon configuration file
        if (!XMLUtilities.parseFromXMLFile(mVisiconConfig, file)) {
            // Print an error message in this case
            mLogger.failure("Error: Cannot parse visicon configuration file '" + file + "'");
            // Return failure if it does not exist
            return false;
        }
        // Print an information message in this case
        mLogger.message("Loaded visicon configuration file '" + file + "':\n" + mVisiconConfig);
        // Return success if the project was loaded
        return true;
    }

    private boolean saveVisiconConfig(final File base) {
        // Create the visicon configuration file
        final File file = new File(base, "visicon.xml");
        // Check if the configuration file does exist
        if (!file.exists()) {
            // Print a warning message in this case
            mLogger.warning("Warning: Creating the new visicon configuration file '" + file + "'");
            // Create a new configuration file now
            try {
                // Try to create a new configuration file
                if (!file.createNewFile()) {
                    // Print an error message in this case
                    mLogger.warning("Warning: There already exists a visicon configuration file '" + file + "'");
                }
            } catch (final IOException exc) {
                // Print an error message in this case
                mLogger.failure("Failure: Cannot create the new visicon configuration file '" + file + "'");
                // Return failure if it does not exist
                return false;
            }
        }
        // Write the visicon configuration file
        if (!XMLUtilities.writeToXMLFile(mVisiconConfig, file)) {
            // Print an error message in this case
            mLogger.failure("Error: Cannot write visicon configuration file '" + file + "'");
            // Return failure if it does not exist
            return false;
        }
        // Print an information message in this case
        mLogger.message("Saved visicon configuration file '" + file + "':\n" + mVisiconConfig);
        // Return success if the project was saved
        return true;
    }

    private boolean loadPlayerConfig(final File base) {
        // Create the player configuration file
        final File file = new File(base, "player.xml");
        // Check if the configuration file does exist
        if (!file.exists()) {
            // Print an error message in this case
            mLogger.failure("Error: Cannot find player configuration file '" + file + "'");
            // Return failure if it does not exist
            return false;
        }
        // Parse the player configuration file
        if (!XMLUtilities.parseFromXMLFile(mPlayerConfig, file)) {
            // Print an error message in this case
            mLogger.failure("Error: Cannot parse player configuration file '" + file + "'");
            // Return failure if it does not exist
            return false;
        }
        // Print an information message in this case
        mLogger.message("Loaded player configuration file '" + file + "':\n" + mPlayerConfig);
        // Return success if the project was loaded
        return true;
    }

    private boolean savePlayerConfig(final File base) {
        // Create the player configuration file
        final File file = new File(base, "player.xml");
        // Check if the configuration file does exist
        if (!file.exists()) {
            // Print a warning message in this case
            mLogger.warning("Warning: Creating the new player configuration file '" + file + "'");
            // Create a new configuration file now
            try {
                // Try to create a new configuration file
                if (!file.createNewFile()) {
                    // Print an error message in this case
                    mLogger.warning("Warning: There already exists a player configuration file '" + file + "'");
                }
            } catch (final IOException exc) {
                // Print an error message in this case
                mLogger.failure("Failure: Cannot create the new player configuration file '" + file + "'");
                // Return failure if it does not exist
                return false;
            }
        }
        // Write the player configuration file
        if (!XMLUtilities.writeToXMLFile(mPlayerConfig, file)) {
            // Print an error message in this case
            mLogger.failure("Error: Cannot write player configuration file '" + file + "'");
            // Return failure if it does not exist
            return false;
        }
        // Print an information message in this case
        mLogger.message("Saved player configuration file '" + file + "':\n" + mPlayerConfig);
        // Return success if the project was saved
        return true;
    }

    private boolean loadDefaultScenePlayer() {
        try {
            // Find the scene player class by name
            final Class clazz = Class.forName(mPlayerConfig.getClassName());
            // Get the player initialization method
            final Method method = clazz.getMethod("getInstance", RunTimeProject.class, PlayerConfig.class);
            // Call the player initialization method
            mDefaultScenePlayer = (Player) method.invoke(null, this, mPlayerConfig);
            // Print an information message in this case
            mLogger.message("Loading default scene player '" + mDefaultScenePlayer + "' with player config:\n" + mPlayerConfig);
            // Return success if the player was loaded
            return true;
        } catch (Exception exc) {
            // Print an error message in this case
            mLogger.failure("Failure: Cannot initialize default scene player with class name '" + mPlayerConfig.getClassName() + "'");
            /// Return failure if it does not exist
            return false;
        }
    }

    /*
     ////////////////////////////////////////////////////////////////////////////
     public final synchronized void loadProject() {
     // Create The Project Config File
     final File file = new File(mProjectFile, "project.xml");
     // Check The Project Config File
     if (file.exists()) {
     // Parse The Project Config File
     if (XMLUtilities.parseFromXMLFile(mProjectConfig, file)) {
     // Print Some Information
     mLogger.message("Success: Parsing Project Configuration '"
     + file.getAbsolutePath() + "':\n" + mProjectConfig.toString());
     // Get Project Data
     mProjectName = mProjectConfig.getProperty("project.name");
     mPluginsFileName = mProjectConfig.getProperty("project.plugins");
     mPlayersFileName = mProjectConfig.getProperty("project.players");
     mActiconFileName = mProjectConfig.getProperty("project.acticon");
     mVisiconFileName = mProjectConfig.getProperty("project.visicon");
     mGesticonFileName = mProjectConfig.getProperty("project.gesticon");
     mSceneFlowFileName = mProjectConfig.getProperty("project.sceneflow");
     mSceneScriptFileName = mProjectConfig.getProperty("project.scenescript");

     // TODO: Clean
     // Added condition for legacy support for project independent preferences
     if (mProjectConfig.getProperty("project.preferences") == null) {
     mPreferencesFileName = "preferences.xml";
     } else {
     mPreferencesFileName = mProjectConfig.getProperty("project.preferences");
     }

     // Read Player Propertiesy   // TODO: Clean
     mScenePlayerClassName = mProjectConfig.getProperty("project.player.default.class");
     mScenePlayerFileName = mProjectConfig.getProperty("project.player.default.config");
     // Get Project ...   // TODO: Clean
     mDialogActClassName = mProjectConfig.getProperty("project.dialogact.class");
     mDialogPlayerClassName = mProjectConfig.getProperty("project.dialogact.player");
     } else {
     // Print Some Information
     mLogger.warning("Failure: Cannot Parse Project Configuration '"
     + file.getAbsolutePath() + "'");
     }
     } else {
     // Print Some Information
     mLogger.failure("Failure: Cannot Find Project Configuration '"
     + file.getAbsolutePath() + "'");
     }
     } 

     ////////////////////////////////////////////////////////////////////////////
     ////////////////////////////////////////////////////////////////////////////
     ////////////////////////////////////////////////////////////////////////////
     public final synchronized void loadPlayers() {
     // Check The Players Config File Name
     if (mPlayersFileName != null) {
     // Create The Players Config File
     final File file = new File(mProjectFile, mPlayersFileName);
     // Check The Players Config File
     if (file.exists()) {
     // Parse The Players Config File
     if (XMLUtilities.parseFromXMLFile(mPlayersConfig, file)) {
     // Print Some Information 
     mLogger.message("Success: Parsing Players Configuration '"
     + file.getAbsolutePath() + "':\n" + mPlayersConfig.toString());
     // Get The Individual Players
     for (final ConfigFeature entry : mPlayersConfig.getEntryList()) {
     // Get Name And File
     final String key = ((String) entry.getKey());
     final String val = ((String) entry.getValue());
     // Get The Config File
     final File base = new File(val);
     // Check The Config File
     if (base.exists()) {
     // Get Plugin Config File
     final ConfigElement data = new ConfigElement("Player", "Feature");
     // Parse The Config File
     if (XMLUtilities.parseFromXMLFile(data, base)) {
     // Print Some Information
     mLogger.message("Success: Parsing Player Configuration '"
     + base.getAbsolutePath() + "':\n" + data.toString());
     // Get Plugin Class Name
     final String name = data.getProperty("class");
     // Check Plugin Class Name
     if (name != null) {
     // Try To Load Plugin
     try {
     // Find The Class
     final Class clazz = Class.forName(name);
     // Get The Method
     final Method method = clazz.getMethod("getInstance", ProjectData.class, ConfigElement.class);
     // Call The Method
     final Player player = (Player) method.invoke(null, this, data);
     // Add The Plugin
     mPlayerMap.put(key, player);
     // Print Some Information 
     mLogger.message("Success: Registering Player Name '"
     + key + "' With Player Object '" + player + "' And Config:\n" + data);
     } catch (Exception exc) {
     // Print Some Information 
     mLogger.failure("Failure: Cannot Initialize Player Of Class '" + name + "'");
     // Print Some Information 
     mLogger.failure(exc.toString());
     }
     } else {
     // Print Some Information 
     mLogger.failure("Failure: Cannot Find Player Classpath Attribute In \n"
     + data.toString());
     }
     } else {
     // Print Some Information 
     mLogger.failure("Failure: Cannot Parse Player Configuration '"
     + base.getAbsolutePath() + "'");
     }
     } else {
     // Print Some Information 
     mLogger.failure("Failure: Cannot Find Player Configuration '"
     + base.getAbsolutePath() + "'");
     }
     }
     } else {
     // Print Some Information 
     mLogger.failure("Failure: Cannot Parse Players Configuration '"
     + file.getAbsolutePath() + "'");
     }
     } else {
     // Print Some Information 
     mLogger.failure("Failure: Cannot Find Players Configuration '"
     + file.getAbsolutePath() + "'");
     }
     } else {
     // Print Some Information 
     mLogger.failure("Failure: Invalid Players Configuration Filename");
     }
     }

  
     ////////////////////////////////////////////////////////////////////////////
     ////////////////////////////////////////////////////////////////////////////
     ////////////////////////////////////////////////////////////////////////////
     public final synchronized void loadPlugins() {
     // Check The Plugins Config File Name
     if (mPluginsFileName != null) {
     // Create The Plugins Config File
     final File file = new File(mProjectFile, mPluginsFileName);
     // Check The Plugins Config File
     if (file.exists()) {
     // Parse The Plugins Config File
     if (XMLUtilities.parseFromXMLFile(mPluginsConfig, file)) {
     // Print Some Information 
     mLogger.message("Success: Parsing Plugins Configuration '"
     + file.getAbsolutePath() + "':\n" + mPluginsConfig.toString());
     // Get The Individual Plugins
     for (final ConfigFeature entry : mPluginsConfig.getEntryList()) {
     // Get Name And File
     final String key = ((String) entry.getKey());
     final String val = ((String) entry.getValue());
     // Get The Config File
     final File base = new File(val);
     // Check The Config File
     if (base.exists()) {
     // Get Plugin Config File
     final ConfigElement data = new ConfigElement("Plugin", "Feature");
     // Parse The Config File
     if (XMLUtilities.parseFromXMLFile(data, base)) {
     // Print Some Information
     mLogger.message("Success: Parsing Plugin Configuration '"
     + base.getAbsolutePath() + "':\n" + data.toString());
     // Get Plugin Class Name
     final String name = data.getProperty("class");
     // Check Plugin Class Name
     if (name != null) {
     // Try To Load Plugin
     try {
     // Find The Class
     final Class clazz = Class.forName(name);
     // Get The Method
     final Method method = clazz.getMethod("getInstance", ProjectData.class, ConfigElement.class);
     // Call The Method
     final Plugin plugin = (Plugin) method.invoke(null, this, data);
     // Add The Plugin
     mPluginMap.put(key, plugin);
     // Print Some Information 
     mLogger.message("Success: Registering Plugin Name '"
     + key + "' With Plugin Object '" + plugin + "' And Config:\n" + data);
     } catch (Exception exc) {
     // Print Some Information 
     mLogger.failure("Failure: Cannot Initialize Plugin Of Class '" + name + "'");
     // Print Some Information 
     mLogger.failure(exc.toString());
     }
     } else {
     // Print Some Information 
     mLogger.failure("Failure: Cannot Find Plugin Classpath Attribute In \n"
     + data.toString());
     }
     } else {
     // Print Some Information 
     mLogger.failure("Failure: Cannot Parse Plugin Configuration '"
     + base.getAbsolutePath() + "'");
     }
     } else {
     // Print Some Information 
     mLogger.failure("Failure: Cannot Find Plugin Configuration '"
     + base.getAbsolutePath() + "'");
     }
     }
     } else {
     // Print Some Information 
     mLogger.failure("Failure: Cannot Parse Players Configuration '"
     + file.getAbsolutePath() + "'");
     }
     } else {
     // Print Some Information 
     mLogger.failure("Failure: Cannot Find Players Configuration '"
     + file.getAbsolutePath() + "'");
     }
     } else {
     // Print Some Information 
     mLogger.failure("Failure: Invalid Plugins Configuration Filename");
     }
     }

     */
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void launchPlayerList() {
        // Launch All Players
        for (final Player player : mPlayerMap.values()) {
            // Launch The Player
            player.launch();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void unloadPlayerList() {
        // Unload All Players
        for (final Player player : mPlayerMap.values()) {
            // Unload The Player
            player.unload();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void launchPluginList() {
        // Launch All Plugins
        for (final Plugin plugin : mPluginMap.values()) {
            // Launch The Plugin
            plugin.launch();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void unloadPluginList() {
        // Unload All Plugins
        for (final Plugin plugin : mPluginMap.values()) {
            // Unload The Plugin
            plugin.unload();
        }
    }

    public final synchronized void loadDialogueAct() {
        if (mDialogActClassName != null) {
            try {
                Class daClass = Class.forName(mDialogActClassName);

                mDialogueAct = (DialogActInterface) daClass.getConstructor().newInstance();
            } catch (Exception exc) {

                // do nothing
            }
        } //else {

        //}
    }

    public final synchronized void loadDialogPlayer() {
        if (mDialogPlayerClassName != null) {
            try {
                Class daPlayerClass = Class.forName(mDialogPlayerClassName);

                mDialogPlayer
                        = (Player) daPlayerClass.getConstructor(RunTimeProject.class).newInstance(this);
            } catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | ClassNotFoundException | NoSuchMethodException | SecurityException ex) {
                mDialogPlayer = new DefaultDialogueActPlayer(this);
            }
        } else {
            mDialogPlayer = new DefaultDialogueActPlayer(this);
        }

        mDialogPlayer.launch();
    }

    public final synchronized void launchScenePlayer() {
        // Launch The Player
        mDefaultScenePlayer.launch();
    }

    public final synchronized void unloadScenePlayer() {
        // Unload The Player
        mDefaultScenePlayer.unload();
    }

    public synchronized int getHashCode() {
        int hashCode = ((mSceneFlow == null)
                ? 0
                : mSceneFlow.getHashCode());
        // TODO: Why Is The Hash Computed
        // Only Based On The SceneFlow's 
        // Hash And Not Based Also On The 
        // Other Project Data Structures?
        return hashCode;
    }

}
