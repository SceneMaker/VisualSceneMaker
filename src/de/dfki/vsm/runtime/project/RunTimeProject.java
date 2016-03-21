package de.dfki.vsm.runtime.project;

import de.dfki.vsm.model.acticon.ActiconConfig;
import de.dfki.vsm.model.gesticon.GesticonConfig;
import de.dfki.vsm.model.project.ProjectConfig;
import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.model.project.DeviceConfig;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.scenescript.ActionObject;
import de.dfki.vsm.model.scenescript.SceneEntity;
import de.dfki.vsm.model.scenescript.SceneObject;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.model.scenescript.SceneTurn;
import de.dfki.vsm.model.scenescript.SceneUttr;
import de.dfki.vsm.model.scenescript.UtteranceElement;
import de.dfki.vsm.model.visicon.VisiconConfig;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.dialog.DialogActInterface;
import de.dfki.vsm.runtime.dialog.DummyDialogAct;
import de.dfki.vsm.runtime.player.ScenePlayer;
import de.dfki.vsm.runtime.player.reactive.ReactivePlayer;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.xml.XMLUtilities;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

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

    // The default scene player of the project
    private ScenePlayer mScenePlayer = null;
    // The default scene player of the project
    //private ScenePlayer mDialogPlayer = null;

    // TODO:  Refactor The Dialog Act Stuff
    // Maybe use a configuration file for that
    private final DialogActInterface mDialogueAct = new DummyDialogAct();

    private final HashMap<String, ActivityExecutor> mDevices = new HashMap();

    // Construct an empty runtime project
    public RunTimeProject() {
        // Print some information
        mLogger.message("Creating a new empty runtime project");
        // Initialize the scene players
        mScenePlayer = new ReactivePlayer(this);
    }

    // Construct a project from a directory
    public RunTimeProject(final File file) {
        // Call the local parsing method
        parse(file.getPath());
        // Initialize the scene players
        mScenePlayer = new ReactivePlayer(this);
    }

    // Get the name of the project's configuration
    public final String getProjectName() {
        return mProjectConfig.getProjectName();
    }

    // Set the name in the project's configuration
    public final void setProjectName(final String name) {
        mProjectConfig.setProjectName(name);
    }

    // Get a specific config from the map of devices
    public final DeviceConfig getDeviceConfig(final String name) {
        return mProjectConfig.getDeviceConfig(name);
    }

    // Get a specific config from the map of plugins
    public final PluginConfig getPluginConfig(final String name) {
        return mProjectConfig.getPluginConfig(name);
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

    public final ScenePlayer getScenePlayer() {
        return mScenePlayer;
    }

    // TODO: launch in project
    public final HashMap<String, ActivityExecutor> getExecutorList() {
        return mDevices;
    }

    public final ActivityExecutor getExecutorOf(final String agent) {
        // Get the agent config 
        final AgentConfig config = mProjectConfig.getAgentConfig(agent);
        // Get the decice name
        final String device = config.getDeviceName();
        // Get the executor
        final ActivityExecutor executor = mDevices.get(device);
        // return the executor
        return executor;
    }

    public boolean parse(final String file) {
        // Check if the file is null
        if (file == null) {
            // Print an error message
            mLogger.failure("Error: Cannot parse runtime project from a bad file");
            // Return false at error
            return false;
        }

        // Parse the project from the base directory
        return (parseProjectConfig(file)
                && parseSceneFlow(file)
                && parseSceneScript(file)
                && parseActiconConfig(file)
                && parseVisiconConfig(file)
                && parseGesticonConfig(file)
                && load());

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
    public final boolean load() {
        // Get the list of devices
        for (final DeviceConfig config : mProjectConfig.getDeviceConfigList()) {
            // Load the device executor
            try {
                // Get the class object
                final Class clazz = Class.forName(config.getClassName());
                // Get the constructor
                final Constructor<ActivityExecutor> constructor
                        = clazz.getConstructor(String.class, RunTimeProject.class);
                // Call the constructor
                final ActivityExecutor executor = constructor.newInstance(config.getDeviceName(), this);
                // Add the executor then
                mDevices.put(config.getDeviceName(), executor);
                // Print some information
                mLogger.message("Loading executor object '" + executor + "'");
            } catch (final Exception exc) {
                mLogger.failure(exc.toString());
            }
        }

        // Return true at success
        return true;
    }

    // TODO: Load Plugins and call methods on them via the evaluator in the interpreter
    // Make a new command type in the syntax for that purpose
    public final Object call (final String name, final String method ) {
        return null;
    }
    
    // Launch the runtime objects of the project
    public final boolean launch() {
        mScenePlayer.launch();
        // Return true at success
        return true;
    }

    // Unload the runtime objects of the project
    public final boolean unload() {

        mScenePlayer.unload();
        // Return true at success
        return true;
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

    public void clearPlayersList() {
        mProjectConfig.cleanPlayerList();
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

    // TODO: refactor this
    // Get the default dialog act taxonomy of the project
    public final DialogActInterface getDialogAct() {
        return mDialogueAct;
    }

    private Set<String> getCharacters(SceneScript scenescript) {
        //
        Set<String> speakersSet = new HashSet<>();

        for (SceneEntity scene : scenescript.getEntityList()) {

            LinkedList<SceneTurn> sturns = ((SceneObject) scene).getTurnList();

            for (SceneTurn t : sturns) {
                if (!speakersSet.contains(t.getSpeaker())) {
                    speakersSet.add(t.getSpeaker());
                }

                LinkedList<SceneUttr> suttr = t.getUttrList();

                for (SceneUttr u : suttr) {
                    LinkedList<UtteranceElement> words = u.getWordList();

                    for (UtteranceElement word : words) {
                        if (word instanceof ActionObject) {
                            ActionObject ao = ((ActionObject) word);

                            String agent = ao.getActor();

                            if ((agent != null) && !agent.trim().isEmpty()) {
                                if (!speakersSet.contains(agent)) {
                                    speakersSet.add(agent);
                                }
                            }
                        }
                    }
                }
            }
        }
        return speakersSet;
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
}
