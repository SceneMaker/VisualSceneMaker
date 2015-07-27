package de.dfki.vsm.players;

import de.dfki.vsm.editor.event.SceneExecutedEvent;
import de.dfki.vsm.editor.event.TurnExecutedEvent;
import de.dfki.vsm.editor.event.UtteranceExecutedEvent;
import de.dfki.vsm.model.project.PlayerConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.model.scenescript.AbstractWord;
import de.dfki.vsm.model.scenescript.ActionObject;
import de.dfki.vsm.model.scenescript.SceneAbbrev;
import de.dfki.vsm.model.scenescript.SceneGroup;
import de.dfki.vsm.model.scenescript.SceneObject;
import de.dfki.vsm.model.scenescript.SceneParam;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.model.scenescript.SceneTurn;
import de.dfki.vsm.model.scenescript.SceneUttr;
import de.dfki.vsm.model.scenescript.SceneWord;
import de.dfki.vsm.runtime.interpreter.Process;
import de.dfki.vsm.runtime.players.RunTimePlayer;
import de.dfki.vsm.runtime.values.AbstractValue;
import de.dfki.vsm.runtime.values.AbstractValue.Type;
import de.dfki.vsm.runtime.values.StringValue;
import de.dfki.vsm.runtime.values.StructValue;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map.Entry;

/**
 * @author Gregor Mehlmann
 */
public final class DefaultScenePlayer implements RunTimePlayer {

    // The singelton player instance
    public static DefaultScenePlayer sInstance = null;
    // The singelton logger instance
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();
    // The player's runtime project 
    private RunTimeProject mProject;
    // The project specific config
    private PlayerConfig mPlayerConfig;
    // The project specific name
    private String mPlayerName;

    // Construct the default scene player
    private DefaultScenePlayer() {

    }

    // Get the default scene player instance
    public static synchronized DefaultScenePlayer getInstance() {
        if (sInstance == null) {
            sInstance = new DefaultScenePlayer();
        }
        return sInstance;
    }

    // Launch the default scene player
    @Override
    public final boolean launch(final RunTimeProject project) {
        // Initialize the project
        mProject = project;
        // Initialize the name
        mPlayerName = project.getPlayerName(this);
        // Initialize the config
        mPlayerConfig = project.getPlayerConfig(mPlayerName);
        // Print some information
        mLogger.message("Launching the default scene player '" + this + "' with configuration:\n" + mPlayerConfig);
        // Return true at success
        return true;
    }

    // Unload the default scene player
    @Override
    public final boolean unload() {
        // Print some information
        mLogger.message("Unloading the default scene player '" + this + "' with configuration:\n" + mPlayerConfig);
        // Return true at success
        return true;
    }

    // Play some scene with the player
    @Override
    public final void play(final String name, final LinkedList<AbstractValue> args) {
        // Print some information
        mLogger.message("Playing '" + name + "' with the default scene player '" + this + "'");
        //
        final Process process = ((Process) Thread.currentThread());
        final HashMap<String, String> mSceneParamMap = new HashMap<>();
        // Process The Arguments
        if ((args != null) && !args.isEmpty()) {
            // Get The First Argument
            final AbstractValue value = args.getFirst();
            // Check The Argument Type
            if (value.getType().equals(Type.STRUCT)) {
                // Cast The Argument Type
                final StructValue struct = (StructValue) value;
                // Process Scene Arguments
                for (final Entry<String, AbstractValue> entry : struct.getValueMap().entrySet()) {
                    if (entry.getValue().getType() == Type.STRING) {
                        mSceneParamMap.put(entry.getKey(), ((StringValue) entry.getValue()).getValue());
                    } else {
                        // Process Other Argument Types
                    }
                }
            }
        }

        // Create The Player Task
        Task task = new Task(process.getName() + name) {
            @Override
            public void run() {

                // Select The Scene
                final SceneScript script = mProject.getSceneScript();
                final SceneGroup group = script.getSceneGroup(name);
                final SceneObject scene = group.select();

                // Scene Visualization
                mLogger.message("Executing scene:\r\n" + scene.getText());
                EventDispatcher.getInstance().convey(new SceneExecutedEvent(this, scene));

                // Process The Turns
                for (SceneTurn turn : scene.getTurnList()) {

                    // Turn Visualization
                    mLogger.message("Executing turn:" + turn.getText());
                    EventDispatcher.getInstance().convey(new TurnExecutedEvent(this, turn));

                    // Get The Turn Speaker
                    final String speaker = turn.getSpeaker();

                    if (speaker == null) {

                        // Get The Default Speaker
                    }

                    // Count The Word Number
                    int wordCount = 0;

                    // Process Utterance
                    for (SceneUttr utt : turn.getUttrList()) {

                        // Utterance Visualization
                        mLogger.message("Executing utterance:" + utt.getText());
                        EventDispatcher.getInstance().convey(new UtteranceExecutedEvent(this, utt));

                        // Process the words of this utterance
                        for (AbstractWord word : utt.getWordList()) {
                            if (word instanceof SceneWord) {

                                // Visualization
                                mLogger.message("Executing vocable:" + ((SceneWord) word).getText());
                                wordCount = ((SceneWord) word).getText().length();
                            } else if (word instanceof SceneParam) {

                                // Visualization
                                mLogger.message("Executing param:" + ((SceneParam) word).getText());
                            } else if (word instanceof ActionObject) {

                                // Visualization
                                mLogger.message("Executing action:" + ((ActionObject) word).getText());
                            } else if (word instanceof SceneAbbrev) {

                                // Visualization
                                mLogger.message("Executing abbreviation:" + ((SceneAbbrev) word).getText());
                            }
                        }
                    }

                    // Utterance simulation
                    try {
                        Thread.sleep(wordCount * 100);
                    } catch (InterruptedException exc) {
                        mLogger.warning(exc.toString());
                    }

                    // Exit if interrupted
                    if (isDone()) {
                        return;
                    }
                }
            }
        };

        // Start the player task
        task.start();
        // Wait for termination
        boolean finished = false;
        while (!finished) {
            try {
                // Join the player task
                task.join();
                // Stop waiting for task
                finished = true;
            } catch (final InterruptedException exc) {
                // Print some warning
                mLogger.warning("Warning: Interrupting process '" + process + "'");
                // Terminate the task
                task.abort();
                // Interrupt the task
                task.interrupt();
            }
        }
    }

    /*
     private boolean init() {
     // Initialize the delay
     try {
     // Check if the config is null
     if (mConfig != null) {
     mDelay = Integer.parseInt(
     mConfig.getProperty("delay"));
     // Print an error message in this case
     mLogger.message("Initializing the default scene player delay with '" + mDelay + "'");
     // Return failure if it does not exist
     return true;
     } else {
     // Print an error message in this case
     mLogger.warning("Warning: Cannot read bad default scene player configuration");
     // Return failure if it does not exist
     return false;
     }
     } catch (final NumberFormatException exc) {
     // Print an error message in this case
     mLogger.failure("Error: Cannot initialize the default scene player delay");
     // Return failure if it does not exist
     return false;
     }
     }
     */
}
