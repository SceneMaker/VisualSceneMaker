package de.dfki.vsm.runtime.player.reactive;

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
import de.dfki.vsm.runtime.player.ScenePlayer;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.runtime.values.AbstractValue;
import java.util.HashMap;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public final class ReactivePlayer extends ScenePlayer {

    // The static marker id
    private static Long sId = 0x0L;

    // Get unique marker id
    private static Long newId() {
        return ++sId;
    }

    // The activity scheduler
    private final ActivityManager mManager = new ActivityManager();

    // Create the scene player
    public ReactivePlayer(final RunTimeProject project) {
        // Initialize the project
        super(project);
// Print some information
        mLogger.message("Creating scene player '" + this + "' for project '" + project + "'");
    }

    // Launch the scene player
    @Override
    public final void launch() {
        // Print some information
        mLogger.message("Launching scene player '" + this + "'");
        // Launch all executors
        for (final ActivityExecutor executor : mProject.getExecutorList().values()) {
            executor.launch();
        }
    }

    // Unload the scene player
    @Override
    public final void unload() {
        // Print some information
        mLogger.message("Unloading scene player '" + this + "'");
        // Unload all executors
        for (final ActivityExecutor executor : mProject.getExecutorList().values()) {
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
                    // Get executor for this turn
                    final ActivityExecutor turnActorExecutor = mProject.getExecutorOf(turn.getSpeaker());
                    // Serially play the utterances
                    for (SceneUttr uttr : turn.getUttrList()) {
                        final LinkedList<String> textBuilder = new LinkedList();
                        final LinkedList<ActivityWorker> observedWorkerList = new LinkedList();
                        for (final UtteranceElement element : uttr.getWordList()) {
                            if (element instanceof ActionObject) {
                                final ActionObject action = (ActionObject) element;
                                // Get the actor name of this action
                                final String actor = action.getActor();
                                // Get the executor for this action
                                final ActivityExecutor actionActorExecutor
                                        = (actor != null ? mProject.getExecutorOf(actor) : turnActorExecutor);
                                // Create a new marker for the action
                                final String marker = actionActorExecutor.marker(newId());
                                // Append the marker to the activity
                                textBuilder.add(marker);
                                // Register the activity with marker
                                observedWorkerList.add(
                                        mManager.register(
                                                marker, // Execute at this marker
                                                new ActionActivity(
                                                        action.getActor(),
                                                        action.getMode(),
                                                        action.getName(),
                                                        action.getText(map)),
                                                actionActorExecutor));
                            } else {
                                // Append the text to the activity
                                textBuilder.add(element.getText(map));
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
                                        textBuilder,
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
