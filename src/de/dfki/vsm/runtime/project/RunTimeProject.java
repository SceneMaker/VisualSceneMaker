package de.dfki.vsm.runtime.project;

import de.dfki.vsm.model.acticon.ActiconConfig;
import de.dfki.vsm.model.gesticon.GesticonConfig;
import de.dfki.vsm.model.project.ProjectConfig;
import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.model.visicon.VisiconConfig;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.interpreter.Interpreter;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.interpreter.value.BooleanValue;
import de.dfki.vsm.runtime.interpreter.value.FloatValue;
import de.dfki.vsm.runtime.interpreter.value.IntValue;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.player.ReactivePlayer;
import de.dfki.vsm.runtime.player.RunTimePlayer;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.xml.XMLUtilities;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

/**
 * @author Gregor Mehlmann
 */
public class RunTimeProject {

    // The singelton logger instance
    protected final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    // The project Path (added PG 11.4.2016);
    private String mProjectPath = "";
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
    // The default scene player of the project
    private RunTimePlayer mRunTimePlayer;
    // The default interpreter of the project
    private Interpreter mInterpreter;
    // The runtime plugin map of the project
    private final HashMap<String, RunTimePlugin> mPluginMap = new HashMap();

    // Construct an empty runtime project
    public RunTimeProject() {
        // Do nothing
    }

    // Construct a project from a directory
    public RunTimeProject(final File file) {
        // Remember Path
        mProjectPath = file.getPath();
        // Call the local parsing method
        parse(mProjectPath);
    }

    // Get the path of the project (added PG 11.4.2016)
    public final String getProjectPath() {
        return mProjectPath;
    }

    // Get the name of the project's configuration
    public final String getProjectName() {
        return mProjectConfig.getProjectName();
    }

    // Set the name in the project's configuration
    public final void setProjectName(final String name) {
        mProjectConfig.setProjectName(name);
    }

    // Get a specific config from the map of plugins
    public final PluginConfig getPluginConfig(final String name) {
        return mProjectConfig.getPluginConfig(name);
    }

    // Get the list of all configured agents in the project configuation (agged PG 8.4.2016)
    public final ArrayList<String> getAgentNames() {
        return mProjectConfig.getAgentNames();
    }

    // Get a specific config from the map of plugins
    public final AgentConfig getAgentConfig(final String name) {
        return mProjectConfig.getAgentConfig(name);
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

    public final RunTimePlayer getRunTimePlayer() {
        return mRunTimePlayer;
    }

    // Get the project configuration (added PG 15.4.2016)
    public final ProjectConfig getProjectConfig() {
        return mProjectConfig;
    }

    public final ActivityExecutor getAgentDevice(final String agent) {
        // Get the agent config 
        final AgentConfig config = mProjectConfig.getAgentConfig(agent);
        // Check the config
        if (config != null) {
            // Get the plugin now
            final RunTimePlugin plugin = mPluginMap.get(config.getDeviceName());
            // Check the plugin 
            if (plugin instanceof ActivityExecutor) {
                // Return the executor now
                return (ActivityExecutor) plugin;
            }
        }
        // Return NULL at failure
        return null;

    }

    public boolean parse(final String file) {
        // Check if the file is null
        if (file == null) {
            // Print an error message
            mLogger.failure("Error: Cannot parse runtime project from a bad file");
            // Return false at error
            return false;
        }
        // remember Path (e.g. EditorProject calls this without instantiation of
        // the RunTimeProject class, so mProjectPath is (re)set her (PG 11.4.2016)
        mProjectPath = file;

        // Parse the project from file
        if (parseProjectConfig(file)) {
            // Initialize the scene player
            mRunTimePlayer = new ReactivePlayer(
                    mProjectConfig.getPlayerConfig(), this);
            //
            return parseSceneFlow(file)
                    && parseSceneScript(file)
                    && parseActiconConfig(file)
                    && parseVisiconConfig(file)
                    && parseGesticonConfig(file)
                    && loadRunTimePlugins();
        } else {
            return false;
        }

    }

    // First attempt to parse Visual SceneMaker project files for information only
    // added PG 14.4.2016
    public boolean parseForInformation(final String file) {
        // Check if the file is null
        if (file == null) {
            // Print an error message
            mLogger.failure("Error: Cannot parse for information a runtime project from a bad file");
            // Return false at error
            return false;
        }
        // remember Path (e.g. EditorProject calls this without instantiation of
        // the RunTimeProject class, so mProjectPath is (re)set her (PG 11.4.2016)
        mProjectPath = file;

        // Parse the project from file
        return (parseProjectConfig(file)
                && parseSceneFlow(file)
                && parseSceneScript(file)
                && parseActiconConfig(file)
                && parseVisiconConfig(file)
                && parseGesticonConfig(file));
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

    // Load the executors of the project
    public final boolean loadRunTimePlugins() {
        // Get the list of devices
        for (final PluginConfig config : mProjectConfig.getPluginConfigList()) {
            // Get the plugin attributes
            final String type = config.getPluginType();
            final String name = config.getPluginName();
            final String clasn = config.getClassName();

            // check if plugin show be loaded - added PG 19.4.2016
            if (config.isMarkedtoLoad()) {
                // Load the device executor
                try {
                    // Get the class object
                    final Class clazz = Class.forName(clasn);
                    // Get the constructor
                    final Constructor constructor
                            = clazz.getConstructor(PluginConfig.class, RunTimeProject.class);
                    // Call the constructor
                    final RunTimePlugin plugin = (RunTimePlugin) constructor.newInstance(config, this);
                    // Add the executor then
                    mPluginMap.put(name, plugin);
                    // Print some information
                    mLogger.message("Loading plugin object '" + plugin + "' of class '" + plugin.getClass().getName() + "'");
                } catch (final Exception exc) {
                    mLogger.failure(exc.toString());
                    exc.printStackTrace();
                }
            } else {
                mLogger.message("Plugin object '" + name + "' is marked as 'not load' - skipping loading plugin.");
            }
        }
        // Return true at success
        return true;
    }

    // TODO: Load Plugins and call methods on them via the evaluator in the interpreter
    // Make a new command type in the syntax for that purpose
    public final Object call(final String name, final String method) {
        return null;
    }

    // Launch the runtime objects of the project
    public final boolean launch() {
        // Launch the scene player
        createRuntimePlayerIfNeeded();
        mRunTimePlayer.launch();
        // Launch all plugins
        for (final RunTimePlugin plugin : mPluginMap.values()) {
            plugin.launch();
        }
        // Create an interpreter
        mInterpreter = new Interpreter(this);//GM
        // Return true at success
        return true;
    }

    private void createRuntimePlayerIfNeeded() {
        if(mRunTimePlayer == null && mProjectConfig != null && mProjectConfig.getPlayerConfig() != null){
            mRunTimePlayer = new ReactivePlayer(mProjectConfig.getPlayerConfig(), this);
        }
    }

    // Unload the runtime objects of the project
    public final boolean unload() {
        // Unload the scene player
        mRunTimePlayer.unload();
        // Unload all plugins
        for (final RunTimePlugin plugin : mPluginMap.values()) {
            plugin.unload();
        }
        // Remove the interpreter
        mInterpreter = null;//GM
        // Return true at success
        return true;
    }

    //GM
    public final boolean start() {
        if (mInterpreter != null) {
            // Start the interpreter
            return mInterpreter.start();
        }
        //
        return false;
    }

    //GM
    public final boolean abort() {
        if (mInterpreter != null) {
            // Abort the interpreter
            return mInterpreter.abort();
        }
        //
        return false;
    }

    //GM
    public final boolean pause() {
        if (mInterpreter != null) {
            // Abort the interpreter
            return mInterpreter.pause();
        }
        //
        return false;
    }

    //GM
    public final boolean proceed() {
        if (mInterpreter != null) {
            // Abort the interpreter
            return mInterpreter.proceed();
        }
        //
        return false;
    }

    //GM
    public final boolean isRunning() {
        if (mInterpreter != null) {
            // Abort the interpreter
            return mInterpreter.isRunning();
        }
        //
        return false;
    }

    //GM
    public final boolean isPaused() {
        if (mInterpreter != null) {
            // Abort the interpreter
            return mInterpreter.isPaused();
        }
        //
        return false;
    }

    //GM
    public final boolean wasExecuted() {
        if (mInterpreter != null) {
            // Abort the interpreter
            return mInterpreter.wasExecuted();
        }
        //
        return false;
    }

    private boolean parseProjectConfig(final String path) {
        InputStream inputStream = null;
        final File file = new File(path, "project.xml");
        if (file.exists()) {
            try {
                inputStream = new FileInputStream(file);
            } catch (FileNotFoundException e) {
                mLogger.failure("Error: Cannot find project configuration file '" + file + "'");
            }
        } else {
            inputStream = ClassLoader.getSystemResourceAsStream(path + System.getProperty("file.separator") + "project.xml");
            if (inputStream == null) {
                // Print an error message in this case
                mLogger.failure("Error: Cannot find project configuration file  ");
                // Return failure if it does not exist
                return false;
            }

        }

        if (!XMLUtilities.parseFromXMLStream(mProjectConfig, inputStream)) {
            mLogger.failure("Error: Cannot parse project configuration file  in path" + path);
            return false;
        }

        mLogger.message("Loaded project from path '" + path + "':\n" + mProjectConfig);
        // Return success if the project was loaded
        return true;
    }

    public boolean parseProjectConfigFromString(String xml) {
        //Parse the config file for project from a string
        InputStream stream = new ByteArrayInputStream(xml.getBytes());
        if (!XMLUtilities.parseFromXMLStream(mProjectConfig, stream)) {
            mLogger.failure("Error: Cannot parse agent");
            return false;
        }
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

    private boolean parseSceneFlow(final String path) {
        InputStream inputStream = null;
        final File file = new File(path, "sceneflow.xml");
        // Check if the configuration file does exist
        if (file.exists()) {
            try {
                inputStream = new FileInputStream(file);
            } catch (FileNotFoundException e) {
                mLogger.failure("Error: Cannot find sceneflow configuration file '" + file + "'");
            }
        } else {
            inputStream = ClassLoader.getSystemResourceAsStream(path + System.getProperty("file.separator") + "sceneflow.xml");
            if (inputStream == null) {
                // Print an error message in this case
                mLogger.failure("Error: Cannot find sceneflow configuration file   project ");
                // Return failure if it does not exist
                return false;
            }

        }

        if (!XMLUtilities.parseFromXMLStream(mSceneFlow, inputStream)) {
            mLogger.failure("Error: Cannot parse sceneflow file  in path" + path);
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
        } else {
            inputStream = ClassLoader.getSystemResourceAsStream(path + System.getProperty("file.separator") + "scenescript.xml");
            if (inputStream == null) {
                // Print an error message in this case
                mLogger.failure("Error: Cannot find scenescript configuration file  ");
                // Return failure if it does not exist
                return false;
            }

        }
        if (!XMLUtilities.parseFromXMLStream(mSceneScript, inputStream)) {
            mLogger.failure("Error: Cannot parse scenescript configuration file  in path" + path);
            return false;
        }

        // Print an information message in this case
//        mLogger.message("Loaded scenescript configuration file in path'" + path + "':\n" + mSceneScript);
        mLogger.message("Loaded scenescript configuration file in path'" + path + "':\n");

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

        } else {
            inputStream = ClassLoader.getSystemResourceAsStream(path + System.getProperty("file.separator") + "acticon.xml");
            if (inputStream == null) {
                // Print an error message in this case
                mLogger.failure("Error: Cannot find acticon configuration file  ");
                // Return failure if it does not exist
                return false;
            }

        }

        if (!XMLUtilities.parseFromXMLStream(mActiconConfig, inputStream)) {
            mLogger.failure("Error: Cannot parse acticon configuration file  in path" + path);
            return false;
        }

        // Print an information message in this case
//        mLogger.message("Loaded acticon configuration file in path'" + path + "':\n" + mActiconConfig);
        mLogger.message("Loaded acticon configuration file in path'" + path + "':\n");
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
        } else {
            inputStream = ClassLoader.getSystemResourceAsStream(path + System.getProperty("file.separator") + "gesticon.xml");
            if (inputStream == null) {
                // Print an error message in this case
                mLogger.failure("Error: Cannot find gesticon configuration file  ");
                // Return failure if it does not exist
                return false;
            }

        }

        if (!XMLUtilities.parseFromXMLStream(mGesticonConfig, inputStream)) {
            mLogger.failure("Error: Cannot parse gesticon configuration file  in path" + path);
            return false;
        }

        // Print an information message in this case
//        mLogger.message("Loaded gesticon configuration file in path'" + path + "':\n" + mGesticonConfig);
        mLogger.message("Loaded gesticon configuration file in path'" + path + "':\n");
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
        } else {
            inputStream = ClassLoader.getSystemResourceAsStream(path + System.getProperty("file.separator") + "visicon.xml");
            if (inputStream == null) {
                // Print an error message in this case
                mLogger.failure("Error: Cannot find visicon configuration file  ");
                // Return failure if it does not exist
                return false;
            }

        }
        if (!XMLUtilities.parseFromXMLStream(mVisiconConfig, inputStream)) {
            mLogger.failure("Error: Cannot parse visicon configuration file  in path" + path);
            return false;
        }

        mLogger.message("Loaded visicon configuration file in path'" + path + "':\n");
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

    // Get the hash code of the project
    protected synchronized int getHashCode() {
        int hashCode = ((mSceneFlow == null)
                ? 0
                : mSceneFlow.getHashCode());
        // TODO: Why Is The Hash Computed
        // Only Based On The SceneFlow's 
        // Hash And Not Based Also On The 
        // Other Project Data Structures?
        hashCode += mSceneScript.getHashCode();
        return hashCode;
    }

    public final boolean setVariable(final String name, final int value) {
        if (mInterpreter != null) {
            return mInterpreter.setVariable(name, new IntValue(value));
        }
        return false;
    }

    public final boolean setVariable(final String name, final int index, final int value) {
        if (mInterpreter != null) {
            return mInterpreter.setVariable(name, index, new IntValue(value));
        }
        return false;
    }

    public final boolean setVariable(final String name, final String member, final int value) {
        if (mInterpreter != null) {
            return mInterpreter.setVariable(name, member, new IntValue(value));
        }
        return false;
    }

    public final boolean setVariable(final String name, float value) {
        if (mInterpreter != null) {
            return mInterpreter.setVariable(name, new FloatValue(value));
        }
        return false;
    }

    public final boolean setVariable(final String name, final int index, float value) {
        if (mInterpreter != null) {
            return mInterpreter.setVariable(name, index, new FloatValue(value));
        }
        return false;
    }

    public final boolean setVariable(final String name, final String member, float value) {
        if (mInterpreter != null) {
            return mInterpreter.setVariable(name, member, new FloatValue(value));
        }
        return false;
    }

    public final boolean setVariable(final String name, boolean value) {
        if (mInterpreter != null) {
            return mInterpreter.setVariable(name, new BooleanValue(value));
        }
        return false;
    }

    public final boolean setVariable(final String name, final int index, boolean value) {
        if (mInterpreter != null) {
            return mInterpreter.setVariable(name, index, new BooleanValue(value));
        }
        return false;
    }

    public final boolean setVariable(final String name, final String member, boolean value) {
        if (mInterpreter != null) {
            return mInterpreter.setVariable(name, member, new BooleanValue(value));
        }
        return false;
    }

    public final boolean setVariable(final String name, final String value) {
        if (mInterpreter != null) {
            return mInterpreter.setVariable(name, new StringValue(value));
        }
        return false;
    }

    public final boolean setVariable(final String name, final int index, final String value) {
        if (mInterpreter != null) {
            return mInterpreter.setVariable(name, index, new StringValue(value));
        }
        return false;
    }

    public final boolean setVariable(final String name, final String member, final String value) {
        if (mInterpreter != null) {
            return mInterpreter.setVariable(name, member, new StringValue(value));
        }
        return false;
    }

    public final boolean setVariable(final String name, final AbstractValue value) {
        if (mInterpreter != null) {
            return mInterpreter.setVariable(name, value);
        }
        return false;
    }

    public final boolean setVariable(final String name, final int index, final AbstractValue value) {
        if (mInterpreter != null) {
            return mInterpreter.setVariable(name, index, value);
        }
        return false;
    }

    public final boolean setVariable(final String name, final String member, final AbstractValue value) {
        if (mInterpreter != null) {
            return mInterpreter.setVariable(name, member, value);
        }
        return false;
    }

    public final boolean hasVariable(final String name) {
        if (mInterpreter != null) {
            return mInterpreter.hasVariable(name);
        }
        return false;
    }

    public final boolean hasVariable(final String name, final int index) {
        if (mInterpreter != null) {
            return mInterpreter.hasVariable(name, index);
        }
        return false;
    }

    public final boolean hasVariable(final String name, final String member) {
        if (mInterpreter != null) {
            return mInterpreter.hasVariable(name, member);

        }
        return false;
    }

    public final AbstractValue getValueOf(final String name) {
        if (mInterpreter != null) {
            return mInterpreter.getValueOf(name);
        }
        return null;
    }

    public final AbstractValue getValueOf(final String name, final int index) {
        if (mInterpreter != null) {
            return mInterpreter.getValueOf(name, index);
        }
        return null;
    }

    public final AbstractValue getValueOf(final String name, final String member) {
        if (mInterpreter != null) {
            return mInterpreter.getValueOf(name, member);
        }
        return null;
    }

    public void deletePlugin(PluginConfig plugin) {
        deleteRelatedAgents(plugin);
        mProjectConfig.deleteDevice(plugin);
    }

    private void deleteRelatedAgents(PluginConfig plugin) {
        Iterator<AgentConfig> iterator = getProjectConfig().getAgentConfigList().iterator();
        while (iterator.hasNext()) {
            AgentConfig agent = iterator.next();
            if (agent.getDeviceName().equals(plugin.getPluginName())) {
                iterator.remove();
            }
        }
    }

    public void deleteAgent(AgentConfig agent) {
        mProjectConfig.deleteAgent(agent);
    }
}
