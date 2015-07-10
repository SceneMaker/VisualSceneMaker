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
import de.dfki.vsm.runtime.Process;
import de.dfki.vsm.runtime.player.Player;
import de.dfki.vsm.runtime.value.AbstractValue;
import de.dfki.vsm.runtime.value.AbstractValue.Type;
import de.dfki.vsm.runtime.value.StringValue;
import de.dfki.vsm.runtime.value.StructValue;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map.Entry;

/**
 * @author Gregor Mehlmann
 */
public final class DefaultScenePlayer implements Player {

    // The singelton player instance
    public static DefaultScenePlayer sInstance = null;
    // The singelton logger instance
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();
    // The player's runtime project 
    final RunTimeProject mProject;
    // The player's configuration
    final PlayerConfig mConfig;
    // The delay for a single letter
    final long mLetterDelay = 10;

    // Construct the default scene player
    private DefaultScenePlayer(
            final RunTimeProject project,
            final PlayerConfig config) {
        // Initialize the player members
        mProject = project;
        mConfig = config;
        // Initialize the letter delay
        
    }

    // Get the default scene player instance
    public static synchronized DefaultScenePlayer getInstance(
            final RunTimeProject project,
            final PlayerConfig config) {
        if (sInstance == null) {
            sInstance = new DefaultScenePlayer(project, config);
        }
        return sInstance;
    }

    // Launch the default scene player
    @Override
    public final boolean launch() {
        // Print some information
        mLogger.message("Launching the default scene player '" + this + "'");
        // Return true at success
        return true;
    }

    // Unload the default scene player
    @Override
    public final boolean unload() {
        // Print some information
        mLogger.message("Unloading the default scene player '" + this + "'");
        // Return true at success
        return true;
    }

    // Play some scene with the player
    @Override
    public final void play(final String name, final LinkedList<AbstractValue> args) {
        // Print some information
        mLogger.message("Playing '" + name + "' with the default scene player '" + this + "'");
        //
        final Process process = ((Process) java.lang.Thread.currentThread());
        final HashMap<String, String> mSceneParamMap = new HashMap<String, String>();

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

                    // Utterance Simulation
                    try {
                        sleep(wordCount * 100);
                    } catch (InterruptedException exc) {
                        mLogger.warning(exc.toString());
                    }

                    // Exit If Interrupted
                    if (mIsDone) {
                        return;
                    }
                }
            }
        };

        // Start The Player Task
        task.start();

        // Wait For Termination
        boolean finished = false;

        while (!finished) {
            try {

                // Join The Player Task
                task.join();

                // Finish This Execution
                finished = true;
            } catch (Exception e) {

                // Abort The Player Task
                task.mIsDone = true;
            }
        }
    }
}
