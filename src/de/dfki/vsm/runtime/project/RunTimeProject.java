package de.dfki.vsm.runtime.project;

import de.dfki.vsm.Preferences;
import de.dfki.vsm.model.acticon.ActiconConfig;
import de.dfki.vsm.model.project.ProjectConfig;
import de.dfki.vsm.model.gesticon.GesticonConfig;
import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.model.project.PlayerConfig;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.model.visicon.VisiconConfig;
import de.dfki.vsm.players.DefaultDialogPlayer;
import de.dfki.vsm.players.DefaultScenePlayer;
import de.dfki.vsm.runtime.dialogacts.DialogActInterface;
import de.dfki.vsm.runtime.dialogacts.DummyDialogAct;
import de.dfki.vsm.runtime.players.RunTimePlayer;
import de.dfki.vsm.runtime.plugins.RunTimePlugin;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.xml.XMLUtilities;

import java.io.*;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map.Entry;

/**
 * @author Gregor Mehlmann
 */
public class RunTimeProject {

    // The singelton logger instance
    protected final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    // The sceneflow of the project
    private final SceneFlow mSceneFlow = new SceneFlow();
    // The scenescript of the project
    private final SceneScript mSceneScript = new SceneScript();
    // The project configuration of the project
    private final ProjectConfig mProjectConfig = new ProjectConfig();
    // The acticon configuration of the project
    private final ActiconConfig mActiconConfig = new ActiconConfig();
    // The visicon configuration of the project
    private final VisiconConfig mVisiconConfig = new VisiconConfig();
    // The gesticon configuration of the project
    private final GesticonConfig mGesticonConfig = new GesticonConfig();

    // The plugins maintained within this project
    private final HashMap<String, RunTimePlugin> mPluginMap = new HashMap<>();
    // The players maintained within this project
    private final HashMap<String, RunTimePlayer> mPlayerMap = new HashMap<>();

    // TODO:  Refactor The Dialog Act Stuff
    // Maybe use a configuration file for that
    private final DialogActInterface mDialogueAct = new DummyDialogAct();

    // Construct an empty runtime project
    public RunTimeProject() {
        // Print some information
        mLogger.message("Creating a new empty runtime project");
    }

    // Construct a project from a directory
    public RunTimeProject(final File file) {
        // Call the local parsing method
        parse(file.getPath());
    }

    // Get the name of the project's configuration
    public final String getProjectName() {
        return mProjectConfig.getProjectName();
    }

    // Set the name in the project's configuration
    public final void setProjectName(final String name) {
        mProjectConfig.setProjectName(name);
    }

    // Get a specific config from the map of players
    public final PlayerConfig getPlayerConfig(final String name) {
        return mProjectConfig.getPlayerConfig(name);
    }

    // Get a specific config from the map of plugins
    public final PluginConfig getPluginConfig(final String name) {
        return mProjectConfig.getPluginConfig(name);
    }

    // Get a specific config from the map of plugins
    public final AgentConfig getAgentConfig(final String name) {
        return mProjectConfig.getAgentConfig(name);
    }

    // Get the project specific name of a player
    public final String getPlayerName(final RunTimePlayer player) {
        for (final Entry<String, RunTimePlayer> entry : mPlayerMap.entrySet()) {
            if (entry.getValue().equals(player)) {
                return entry.getKey();
            }
        }
        return null;
    }

    // Get the project specific name of a plugin
    public final String getPluginName(final RunTimePlugin plugin) {
        for (final Entry<String, RunTimePlugin> entry : mPluginMap.entrySet()) {
            if (entry.getValue().equals(plugin)) {
                return entry.getKey();
            }
        }
        return null;
    }

    // Get the sceneflow of the project
    public final SceneFlow getSceneFlow() {
        return mSceneFlow;
    }

    // Get the scenescript of the project
    public final SceneScript getSceneScript() {
        return mSceneScript;
    }

    // Get the acticon of the project
    public final ActiconConfig getActicon() {
        return mActiconConfig;
    }

    // Get the visicon of the project
    public final VisiconConfig getVisicon() {
        return mVisiconConfig;
    }

    // Get the gesticon of the project
    public final GesticonConfig getGesticon() {
        return mGesticonConfig;
    }

    // TODO: refactor this
    // Get the default dialog act taxonomy of the project
    public final DialogActInterface getDialogAct() {
        return mDialogueAct;
    }

    // Get a specific player from the map of players
    public final RunTimePlayer getPlayer(final String name) {
        return mPlayerMap.get(name);
    }

    // Get a specific plugin from the map of plugins
    public final RunTimePlugin getPlugin(final String name) {
        return mPluginMap.get(name);
    }

    // Get the scene player of the project
    public final RunTimePlayer getDefaultScenePlayer() {
        // Get the player from the map
        final RunTimePlayer player = getPlayer("defaultsceneplayer");
        // Check if the player exists
        if (player != null) {
            // And return it in this case
            return player;
        } else {
            // Or construct a default scene 
            // player without a configuration
            return DefaultScenePlayer.getInstance();
        }
    }

    // Get the dialog player of the project
    public final RunTimePlayer getDefaultDialogPlayer() {
        // Get the player from the map
        final RunTimePlayer player = getPlayer("defaultdialogplayer");
        // Check if the player exists
        if (player != null) {
            // And return it in this case
            return player;
        } else {
            // Or construct a default scene 
            // player without a configuration
            return DefaultDialogPlayer.getInstance();
        }
    }

    // Parse the project data from a directory
    /*public boolean parse(final File file) {
     // Check if the file is null
     if (file == null) {
     // Print an error message
     mLogger.failure("Error: Cannot parse runtime project from a bad file");
     // Return false at error
     return false;
     }
     // Get the absolute file for this directory
     final File base = file.getAbsoluteFile();
     // Check if the project directory does exist
     if (!base.exists()) {
     // Print an error message
     mLogger.failure("Error: Cannot find runtime project directory '" + base + "'");
     // Return false at error
     return false;
     }
     // Parse the project from the base directory
     return (parseProjectConfig(base)
     && parseSceneFlow(base)
     && parseSceneScript(base)
     && parseActiconConfig(base)
     && parseVisiconConfig(base)
     && parseGesticonConfig(base));
     }*/
    public boolean parse(String urlBaseStream) {
        // Check if the file is null
        if (urlBaseStream == null) {
            // Print an error message
            mLogger.failure("Error: Cannot parse runtime project from a bad file");
            // Return false at error
            return false;
        }

        // Parse the project from the base directory
        return (parseProjectConfig(urlBaseStream)
                && parseSceneFlow(urlBaseStream)
                && parseSceneScript(urlBaseStream)
                && parseActiconConfig(urlBaseStream)
                && parseVisiconConfig(urlBaseStream)
                && parseGesticonConfig(urlBaseStream));

    }
    


    // Write the project data to a directory
    public boolean write(final File file) {
        // Check if the file is null
        if (file == null) {
            // Print an error message
            mLogger.failure("Error: Cannot write runtime project into a bad file");
            // Return false at error
            return false;
        }
        // Get the absolute file for the directory
        final File base = file.getAbsoluteFile();
        // Check if the project directory does exist
        if (!base.exists()) {
            // Print a warning message in this case
            mLogger.warning("Warning: Creating a new runtime project directory '" + base + "'");
            // Try to create a project base directory
            if (!base.mkdir()) {
                // Print an error message
                mLogger.failure("Failure: Cannot create a new runtime project directory '" + base + "'");
                // Return false at error
                return false;
            }
        }
        // Save the project to the base directory
        return (writeProjectConfig(base)
                && writeSceneFlow(base)
                && writeSceneScript(base)
                && writeActiconConfig(base)
                && writeVisiconConfig(base)
                && writeGesticonConfig(base));
    }

    // Load the runtime objects of the project
    public final boolean load() {
        return (loadPlayers() && loadPlugins());
    }

    // Launch the runtime objects of the project
    public final boolean launch() {
        return (launchPlayers() && launchPlugins());
    }

    // Unload the runtime objects of the project
    public final boolean unload() {
        return (unloadPlayers() && unloadPlugins());
    }

    private boolean parseProjectConfig(final String path) {
        InputStream inputStream = null;
        final File file = new File(path, "project.xml");
        if (file.exists()) {
             try {
                inputStream = new FileInputStream(file);
            } catch (FileNotFoundException e) {
                mLogger.failure("Error: Cannot find sproject configuration file '" + file + "'");
            }
        }else{
            inputStream = ClassLoader.getSystemResourceAsStream(path + System.getProperty("file.separator")  + "project.xml");
            if (inputStream == null) {
                // Print an error message in this case
                mLogger.failure("Error: Cannot find project configuration file  ");
                // Return failure if it does not exist
                return false;
            }

        }

        if(!XMLUtilities.parseFromXMLStream(mProjectConfig, inputStream)){
               mLogger.failure("Error: Cannot parse project configuration file  in path" + path);
               return false;
        }

        // Print an information message in this case
        mLogger.message("Loaded project configuration file in path'" + path + "':\n" + mProjectConfig);
        // Return success if the project was loaded
        return true;
    }

    private boolean writeProjectConfig(final File base) {
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


    private boolean parseSceneFlow(final String path){
        InputStream inputStream = null;
        final File file = new File(path, "sceneflow.xml");
        // Check if the configuration file does exist
        if (file.exists()) {
           try {
                inputStream = new FileInputStream(file);
           } catch (FileNotFoundException e) {
                mLogger.failure("Error: Cannot find sceneflow configuration file '" + file + "'");
           }
        }

        else{
            inputStream = ClassLoader.getSystemResourceAsStream(path + System.getProperty("file.separator")  + "sceneflow.xml");
            if (inputStream == null) {
                // Print an error message in this case
                mLogger.failure("Error: Cannot find sceneflow configuration file   project ");
                // Return failure if it does not exist
                return false;
            }

        }

        if(!XMLUtilities.parseFromXMLStream(mSceneFlow, inputStream)){
            mLogger.failure("Error: Cannot parse sceneflow configuration file  in path" + path);
            return false;
        }
        // Perform all the postprocessing steps
        mSceneFlow.establishStartNodes();
        mSceneFlow.establishTargetNodes();
        mSceneFlow.establishAltStartNodes();
        // Print an information message in this case
        mLogger.message("Loaded sceneflow configuration file in path '" + path + "':\n" + mSceneFlow);
        // Return success if the project was loaded
        return true;

    }

    private boolean writeSceneFlow(final File base) {
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

    private boolean parseSceneScript(final String path) {
        InputStream inputStream = null;

        final File file = new File(path, "scenescript.xml");
            // Check if the configuration file does exist
        if (file.exists()) {
            try {
                inputStream = new FileInputStream(file);
            } catch (FileNotFoundException e) {
                mLogger.failure("Error: Cannot find scenescript configuration file '" + file + "'");
            }
        }

        else {
            inputStream = ClassLoader.getSystemResourceAsStream(path + System.getProperty("file.separator")  + "scenescript.xml");
            if (inputStream == null) {
                // Print an error message in this case
                mLogger.failure("Error: Cannot find scenescript configuration file  ");
                // Return failure if it does not exist
                return false;
            }

        }
        if(!XMLUtilities.parseFromXMLStream(mSceneScript, inputStream)){
                mLogger.failure("Error: Cannot parse scenescript configuration file  in path" + path);
                return false;
        }

        // Print an information message in this case
        mLogger.message("Loaded scenescript configuration file in path'" + path + "':\n" + mSceneScript);
        // Return success if the project was loaded
        return true;
    }

    private boolean writeSceneScript(final File base) {
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

    private boolean parseActiconConfig(final String path) {

        InputStream inputStream = null;
        final File file = new File(path, "acticon.xml");
            // Check if the configuration file does exist
        if (file.exists()) {
           try {
                inputStream = new FileInputStream(file);
            } catch (FileNotFoundException e) {
                mLogger.failure("Error: Cannot find acticon configuration file '" + file + "'");
            }

        }

        else {
            inputStream = ClassLoader.getSystemResourceAsStream(path + System.getProperty("file.separator")  + "acticon.xml");
            if (inputStream == null) {
                // Print an error message in this case
                mLogger.failure("Error: Cannot find acticon configuration file  ");
                // Return failure if it does not exist
                return false;
            }


        }

        if(!XMLUtilities.parseFromXMLStream(mActiconConfig, inputStream)){
            mLogger.failure("Error: Cannot parse acticon configuration file  in path" + path);
            return false;
        }

        // Print an information message in this case
        mLogger.message("Loaded acticon configuration file in path'" + path + "':\n" + mActiconConfig);
        // Return success if the project was loaded
        return true;
    }

    private boolean writeActiconConfig(final File base) {
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

    private boolean parseGesticonConfig(final String path) {

        InputStream inputStream = null;

        final File file = new File(path, "gesticon.xml");
            // Check if the configuration file does exist
        if (file.exists()) {
            try {
                inputStream = new FileInputStream(file);
            } catch (FileNotFoundException e) {
                mLogger.failure("Error: Cannot find gesticon configuration file '" + file + "'");
            }
        }

        else {
            inputStream = ClassLoader.getSystemResourceAsStream(path + System.getProperty("file.separator")  + "gesticon.xml");
            if (inputStream == null) {
                // Print an error message in this case
                mLogger.failure("Error: Cannot find gesticon configuration file  ");
                // Return failure if it does not exist
                return false;
            }


        }

        if(!XMLUtilities.parseFromXMLStream(mGesticonConfig, inputStream)){
            mLogger.failure("Error: Cannot parse gesticon configuration file  in path" + path);
            return false;
        }

        // Print an information message in this case
        mLogger.message("Loaded gesticon configuration file in path'" + path + "':\n" + mGesticonConfig);
        // Return success if the project was loaded
        return true;
    }

    private boolean writeGesticonConfig(final File base) {
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

    private boolean parseVisiconConfig(final String path) {

        InputStream inputStream = null;

        final File file = new File(path, "visicon.xml");
            // Check if the configuration file does exist
        if (file.exists()) {
            try {
                inputStream = new FileInputStream(file);
            } catch (FileNotFoundException e) {
                mLogger.failure("Error: Cannot find visicon configuration file '" + file + "'");
            }
        }

        else {
            inputStream = ClassLoader.getSystemResourceAsStream(path + System.getProperty("file.separator")  + "visicon.xml");
            if (inputStream == null) {
                // Print an error message in this case
                mLogger.failure("Error: Cannot find visicon configuration file  ");
                // Return failure if it does not exist
                return false;
            }

        }
        if(!XMLUtilities.parseFromXMLStream(mVisiconConfig, inputStream)){
            mLogger.failure("Error: Cannot parse visicon configuration file  in path" + path);
            return false;
        }

        // Print an information message in this case
        mLogger.message("Loaded visicon configuration file in path'" + path + "':\n" + mVisiconConfig);
        // Return success if the project was loaded
        return true;
    }

    private boolean writeVisiconConfig(final File base) {
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

    // Load the plugins of the project
    private boolean loadPlugins() {
        for (final PluginConfig config : mProjectConfig.getPluginConfigList()) {
            // Get the class and plugin name
            final String className = config.getClassName();
            final String pluginName = config.getPluginName();
            try {
                // Find the plugin class by name
                final Class clazz = Class.forName(className);
                // Get the initialization method
                final Method method = clazz.getMethod("getInstance");
                // Call the initialization method
                final RunTimePlugin plugin = (RunTimePlugin) method.invoke(null);
                // Check if plugin has been created
                if (plugin == null) {
                    // Print an error message 
                    mLogger.failure("Failure: Cannot load plugin of class '" + className + "'");
                    // Return false at error
                    return false;
                } else {
                    // Set the default scene player then
                    mPluginMap.put(pluginName, plugin);
                    // Print an information message here
                    mLogger.message("Loading plugin '" + plugin + "' with plugin config:\n" + config);
                }
            } catch (final Exception exc) {
                // Print an error message
                mLogger.failure("Failure: Cannot load plugin of class '" + className + "'");
                // Print an error message
                mLogger.failure(exc.toString());
            }
        }
        // Return true at success
        return true;
    }

    // Load the players of the project
    private boolean loadPlayers() {
        //
        for (final PlayerConfig config : mProjectConfig.getPlayerConfigList()) {
            // Get the class and plugin name
            final String className = config.getClassName();
            final String playerName = config.getPlayerName();
            try {
                // Find the plugin class by name
                final Class clazz = Class.forName(className);
                // Get the initialization method
                final Method method = clazz.getMethod("getInstance");
                // Call the initialization method
                final RunTimePlayer player = (RunTimePlayer) method.invoke(null);
                // Check if plugin has been created
                if (player == null) {
                    // Print an error message 
                    mLogger.failure("Failure: Cannot load player of class '" + className + "'");
                    // Return false at error
                    return false;
                } else {
                    // Set the default scene player then
                    mPlayerMap.put(playerName, player);
                    //new TPLTuple<Player, PlayerConfig>(player, config)
                    // Print an information message here
                    mLogger.message("Loading player '" + player + "' with plugin config:\n" + config);
                }
            } catch (final Exception exc) {
                // Print an error message
                mLogger.failure("Failure: Cannot load player of class '" + className + "'");
                // Print an error message
                mLogger.failure(exc.toString());
            }
        }
        // Return true at success
        return true;
    }

    // Launch the players of the project
    private boolean launchPlayers() {
        // Initialize the result flag
        boolean success = true;
        // Try launching all the players
        for (final RunTimePlayer player : mPlayerMap.values()) {
            if (!player.launch(this)) {
                success = false;
            }
        }
        return success;
    }

    // Launch the plugins of the project
    private boolean launchPlugins() {
        // Initialize the result flag
        boolean success = true;
        // Try launching all the plugins
        for (final RunTimePlugin plugin : mPluginMap.values()) {
            if (!plugin.launch(this)) {
                success = false;
            }
        }
        return success;
    }

    // Unload the players of the project
    private boolean unloadPlayers() {
        // Initialize the result flag
        boolean success = true;
        // Try unloading all the players
        for (final RunTimePlayer player : mPlayerMap.values()) {
            if (!player.unload()) {
                success = false;
            }
        }
        return success;
    }

    // Unload the plugins of the project
    private boolean unloadPlugins() {
        // Initialize the result flag
        boolean success = true;
        // Try unloading all the plugins
        for (final RunTimePlugin plugin : mPluginMap.values()) {
            if (!plugin.unload()) {
                success = false;
            }
        }
        return success;
    }

//        //%%
//        if (mDialogActClassName != null) {
//            try {
//                Class daClass = Class.forName(mDialogActClassName);
//
//                mDialogueAct = (DialogActInterface) daClass.getConstructor().newInstance();
//            } catch (Exception exc) {
//
//                // do nothing
//            }
//        } //else {
//
//        //}
    // Get the hash code of the project
    protected synchronized int getHashCode() {
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
