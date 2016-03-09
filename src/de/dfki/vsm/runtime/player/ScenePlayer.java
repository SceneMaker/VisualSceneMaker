package de.dfki.vsm.runtime.player;

import de.dfki.vsm.model.scenescript.ActionObject;
import de.dfki.vsm.model.scenescript.SceneGroup;
import de.dfki.vsm.model.scenescript.SceneObject;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.model.scenescript.SceneTurn;
import de.dfki.vsm.model.scenescript.SceneUttr;
import de.dfki.vsm.model.scenescript.UtteranceElement;
import de.dfki.vsm.runtime.interpreter.Process;
import de.dfki.vsm.runtime.player.activity.ActionActivity;
import de.dfki.vsm.runtime.player.activity.VerbalActivity;
import de.dfki.vsm.runtime.player.activity.player.ActivityPlayer;
import de.dfki.vsm.runtime.player.activity.player.ActivityPlayer.SchedulingPolicy;
import de.dfki.vsm.runtime.player.activity.trigger.MarkerTrigger;
import de.dfki.vsm.runtime.player.activity.trigger.TimeoutTrigger;
import de.dfki.vsm.runtime.player.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.runtime.values.AbstractValue;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.xtension.charamel.Charamel;
import de.dfki.vsm.xtension.console.Console;
import de.dfki.vsm.xtension.stickman.Stickman;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.concurrent.AbstractExecutorService;

/**
 * @author Gregor Mehlmann
 */
public final class ScenePlayer implements AbstractPlayer {

    // The defaut system logger
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    // The static marker id
    private static Long sId = 0x0L;

    // Get unique marker id
    private static Long newId() {
        return ++sId;
    }

    // The runtime project data
    private final RunTimeProject mProject;
    // The activity scheduler
    private final ActivityPlayer mPlayer = new ActivityPlayer();
    // The executor mapping
    private final HashMap<String, ActivityExecutor> mDevices
            = new HashMap();

    // Create the scene player
    public ScenePlayer(final RunTimeProject project) {
        // Initialize the project
        mProject = project;

        // Load the executors now
        try {
            // Do that from the config!
            mDevices.put("sticky", de.dfki.vsm.xtension.console.Console.class.getConstructor(RunTimeProject.class).newInstance(project));
            mDevices.put("gloria", de.dfki.vsm.xtension.console.Console.class.getConstructor(RunTimeProject.class).newInstance(project));
        } catch (final Exception exc) {
            mLogger.failure(exc.toString());
        }
        // Print some information
        mLogger.message("Creating scene player '" + this + "' for project '" + project + "'");
    }

    // Launch the scene player
    @Override
    public final void launch() {
        // Print some information
        mLogger.message("Launching scene player '" + this + "'");
        // Launch all executors
        for (final ActivityExecutor executor : mDevices.values()) {
            executor.launch();
        }
    }

    // Unload the scene player
    @Override
    public final void unload() {
        // Print some information
        mLogger.message("Unloading scene player '" + this + "'");
        // Unload all executors
        for (final ActivityExecutor executor : mDevices.values()) {
            executor.unload();
        }
    }

    // Call the playback method
    @Override
    public final void play(final String name, final LinkedList args) {
        // Get the current process
        final Process process = (Process) Thread.currentThread();
        // Print some information
        mLogger.message("Playing '" + name + "' in process '" + process + "' on scene player '" + this + "'");
        // Translate the arguments
        final HashMap map = new HashMap();
        if (args != null && !args.isEmpty()) {
            for (final Object object : args) {
                final AbstractValue value = (AbstractValue) object;
            }
        }
        // Get the scene object
        final SceneScript script = mProject.getSceneScript();
        final SceneGroup group = script.getSceneGroup("en", name);
        final SceneObject scene = group.select();
        // Create playback task
        final Task task = new Task(name) {
            
            @Override
            public void run() {
                for (SceneTurn turn : scene.getTurnList()) {
                    // Get executor for turn
                    final ActivityExecutor turnexec = mDevices.get(turn.getSpeaker());
                    //
                    for (SceneUttr uttr : turn.getUttrList()) {
                        final LinkedList<String> builder = new LinkedList();
                        for (final UtteranceElement element : uttr.getWordList()) {
                            if (element instanceof ActionObject) {
                                final ActionObject action = (ActionObject) element;
                                // Get the actor name of this action
                                final String actor = action.getActorName();
                                // Get the executor for this action
                                final ActivityExecutor actexec
                                        = (actor != null ? mDevices.get(actor) : turnexec);
                                // Create a new marker for the action
                                final String marker = actexec.marker(newId());
                                // Append the marker to the activity
                                builder.add(marker);
                                // Register the activity with marker
                                mPlayer.register(
                                        new MarkerTrigger(marker),
                                        SchedulingPolicy.PARALLEL,
                                        new ActionActivity(action.getText(map)), actexec);
                            } else {
                                // Append the text to the activity
                                builder.add(element.getText(map));
                            }
                        }
                        // 
                        final String punctuation = uttr.getPunctuationMark();
                        // Schedule the activity
                        mPlayer.schedule(
                                new TimeoutTrigger(0),
                                SchedulingPolicy.BLOCKING,
                                new VerbalActivity(turn.getSpeaker(), builder, punctuation), turnexec);
                        // Check for interruption
                        if (isDone()) {
                            return;
                        }
                    }
                }
            }
        };
        // Start the playback task
        task.start();
        // Wait for playback task
        boolean finished = false;
        while (!finished) {
            try {
                // Print some information
                mLogger.warning("Waiting for '" + task + "'");
                // Join the playback task
                task.join();
                // Continue after joining
                finished = true;
            } catch (final InterruptedException exc) {
                // Print some information
                mLogger.warning("Interrupting '" + task + "'");
                // Terminate playback task
                task.abort();
            }
        }
        // Print some information
        mLogger.warning("Continuing '" + process + "'");
    }

    // A player task
    public class Task extends Thread {

        // The termination flag
        private volatile boolean mDone;

        // Abort the execution
        public final void abort() {
            //
            mDone = true;
            //
            interrupt();
        }

        // Check execution status
        public final boolean isDone() {
            return mDone;
        }

        // Construct with a name
        protected Task(final String name) {
            super(name);
            // Initialize the flag
            mDone = false;
        }
    }
}
