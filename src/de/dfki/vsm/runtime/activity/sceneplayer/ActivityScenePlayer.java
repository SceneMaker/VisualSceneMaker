package de.dfki.vsm.runtime.activity.sceneplayer;

import de.dfki.vsm.model.scenescript.ActionObject;
import de.dfki.vsm.model.scenescript.SceneGroup;
import de.dfki.vsm.model.scenescript.SceneObject;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.model.scenescript.SceneTurn;
import de.dfki.vsm.model.scenescript.SceneUttr;
import de.dfki.vsm.model.scenescript.UtteranceElement;
import de.dfki.vsm.runtime.interpreter.Process;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.manager.ActivityManager;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.manager.ActivityWorker;
import de.dfki.vsm.runtime.player.AbstractPlayer;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.runtime.values.AbstractValue;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.util.HashMap;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public final class ActivityScenePlayer implements AbstractPlayer {

    // The defaut system logger
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    // The static marker id
    private static Long sId = 0x0L;

    // Get unique marker id
    private static Long newId() {
        return ++sId;
    }

    // The runtime project
    private final RunTimeProject mProject;
    // The activity scheduler
    private final ActivityManager mManager = new ActivityManager();
    // The activity executor map
    private final HashMap<String, ActivityExecutor> mDevices = new HashMap();

    // Create the scene player
    public ActivityScenePlayer(final RunTimeProject project) {
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
        // Make unique worker name
        final String task = process.getName() + ":" + name + "@";
        // TODO: Append VSM framework time to name
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
        final PlayerWorker worker = new PlayerWorker(task) {

            @Override
            public void run() {
                for (SceneTurn turn : scene.getTurnList()) {
                    // Get executor for turn
                    final ActivityExecutor turnActorExecutor = mDevices.get(turn.getSpeaker());
                    //
                    for (SceneUttr uttr : turn.getUttrList()) {
                        final LinkedList<String> builderlist = new LinkedList();
                        final LinkedList<ActivityWorker> observedWorkerList = new LinkedList();
                        for (final UtteranceElement element : uttr.getWordList()) {
                            if (element instanceof ActionObject) {
                                final ActionObject action = (ActionObject) element;
                                // Get the actor name of this action
                                final String actor = action.getActorName();
                                // Get the executor for this action
                                final ActivityExecutor actionActorExecutor
                                        = (actor != null ? mDevices.get(actor) : turnActorExecutor);
                                // Create a new marker for the action
                                final String marker = actionActorExecutor.marker(newId());
                                // Append the marker to the activity
                                builderlist.add(marker);
                                // Register the activity with marker
                                observedWorkerList.add(
                                        mManager.register(
                                                marker, // Execute at this marker
                                                new ActionActivity(
                                                        null,
                                                        null,
                                                        null,
                                                        action.getText(map)),
                                                actionActorExecutor));
                            } else {
                                // Append the text to the activity
                                builderlist.add(element.getText(map));
                            }
                        }
                        // 
                        final String punctuation = uttr.getPunctuationMark();
                        // Schedule the activity
                        mManager.schedule(
                                0, // Schedule without delay
                                observedWorkerList,
                                new SpeechActivity(
                                        turn.getSpeaker(),
                                        builderlist,
                                        punctuation),
                                turnActorExecutor);
                        // Check for interruption
                        if (isDone()) {
                            return;
                        }
                    }
                }
            }
        };
        // Start the playback task
        worker.start();
        // Wait for playback task
        boolean finished = false;
        while (!finished) {
            try {
                // Print some information
                mLogger.warning("Awaiting player worker '" + worker + "'");
                // Join the playback task
                worker.join();
                // Continue after joining
                finished = true;
                // Print some information
                mLogger.warning("Joining player worker '" + worker + "'");
            } catch (final InterruptedException exc) {
                // Print some information
                mLogger.warning("Aborting player worker '" + worker + "'");
                // Terminate playback task
                worker.abort();
            }
        }
        // Print some information
        mLogger.warning("Continuing '" + process + "'");
    }
}
