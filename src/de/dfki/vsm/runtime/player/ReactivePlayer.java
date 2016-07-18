package de.dfki.vsm.runtime.player;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.model.scenescript.ActionObject;
import de.dfki.vsm.model.scenescript.SceneGroup;
import de.dfki.vsm.model.scenescript.SceneObject;
import de.dfki.vsm.model.scenescript.SceneParam;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.model.scenescript.SceneTurn;
import de.dfki.vsm.model.scenescript.SceneUttr;
import de.dfki.vsm.model.scenescript.UtteranceElement;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.interpreter.Process;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import java.util.HashMap;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public final class ReactivePlayer extends RunTimePlayer {

    // The static marker id
    private static long sId = 0x0L;

    // Get unique marker id
    private synchronized Long newId() {
        return sId++;
    }

    // Create the scene player
    public ReactivePlayer(final PluginConfig config, final RunTimeProject project) {
        // Initialize the player
        super(config, project);
        // Print some information
        mLogger.message("Creating reactive player '" + this + "' for project '" + project + "'");
    }

    // Launch the player
    @Override
    public final void launch() {
        // Print some information
        mLogger.message("Launching reactive player '" + this + "'");
    }

    // Unload the player
    @Override
    public final void unload() {
        // Print some information
        mLogger.message("Unloading reactive player '" + this + "'");
    }

    // Call the play action activity method
    @Override
    public final void playActionActivity(final String name, final LinkedList args) {
        // Get the current process
        final Process process = (Process) Thread.currentThread();
        // Make unique worker name
        final String task = process.getName() + ":" + name + "@";
        // Print some information
        mLogger.message("Playing Action Activity '" + name + "' in process '" + process + "' on reactive player '" + this + "'");

        // Create playback task
        final PlayerWorker worker = new PlayerWorker(task) {
            @Override
            public void run() {
                // parsing actor, action, and features
                String cmdString = name.trim();
                String actor = "";
                String action = "";
                LinkedList<ActionFeature> features = new LinkedList<>();

                if (cmdString.startsWith("[") && cmdString.endsWith("]")) {
                    cmdString = cmdString.substring(1, cmdString.length() - 1);
                    String[] parts = cmdString.split("[ ]+");
                    int cnt = 0;
                    for (String part : parts) {
                        if (cnt == 0) {
                            actor = part;
                        } else if (cnt == 1) {
                            action = part;
                        } else if (part.contains("=")) {
                            String[] pair = part.split("=");
                            features.add(new ActionFeature(ActionFeature.Type.STRING, 0, pair[0].length(), pair[0], pair[1]));
                        }
                        cnt++;
                    }
                }

                // Schedule the activity without delay but blocking
                ActionActivity aa = new ActionActivity(actor, "cmd", action, null, features);
                aa.setTyp(AbstractActivity.Policy.BLOCKING);
                mScheduler.schedule(0, null, aa,  mProject.getAgentDevice(actor));
                // Check for interruption
                if (isDone()) {
                    return;
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
                mLogger.message("Awaiting player worker '" + worker + "'");
                // Join the playback task
                worker.join();
                // Continue after joining
                finished = true;
                // Print some information
                mLogger.message("Joining player worker '" + worker + "'");
            } catch (final InterruptedException exc) {
                // Print some information
                mLogger.warning("Aborting player worker '" + worker + "'");
                // Terminate playback task
                worker.abort();
            }
        }
        // Print some information
        mLogger.message("Continuing '" + process + "'");
    }

    // Call the play scene group method
    @Override
    public final void playSceneGroup(final String name, final LinkedList args) {
        // Get the current process
        final Process process = (Process) Thread.currentThread();
        // Make unique worker name
        final String task = process.getName() + ":" + name + "@";
        // TODO: Append VSM framework time to name
        // Print some information
        mLogger.message("Playing Scenegroup '" + name + "' in process '" + process + "' on reactive player '" + this + "'");
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
                    final ActivityExecutor turnActorExecutor = mProject.getAgentDevice(turn.getSpeaker());
                    // Serially play the utterances
                    for (SceneUttr uttr : turn.getUttrList()) {

                        mLogger.message("Utterance " + uttr.getText().trim());

                        final LinkedList<String> textBuilder = new LinkedList();
                        final LinkedList<ActivityWorker> observedWorkerList = new LinkedList();
                        for (final UtteranceElement element : uttr.getWordList()) {
                            //mLogger.message("element " + element);

                            if (element instanceof ActionObject) {
                                final ActionObject action = (ActionObject) element;
                                // Get the actor name of this action
                                final String actor = action.getActor();
                                // Get the executor for this action
                                final ActivityExecutor actionActorExecutor
                                        = (actor != null ? mProject.getAgentDevice(actor) : turnActorExecutor);
                                // Create a new marker for the action
                                // 19.4.2016 PG: CAREFULL! The marker has to be generated by the turn executor! not by the activity executor of the action!
                                //Old: final String marker = actionActorExecutor.marker(newId());
                                final String marker = turnActorExecutor.marker(newId());
                                // Append the marker to the activity
                                textBuilder.add(marker);
                                // Register the activity with marker
                                observedWorkerList.add(
                                        mScheduler.register(
                                                marker, // Execute at this marker
                                                new ActionActivity(
                                                        (action.getActor() == null) ? turn.getSpeaker() : action.getActor(), // added PG 5.4.2016
                                                        action.getMode(),
                                                        action.getName(),
                                                        action.getText(map),
                                                        action.getFeatureList()),
                                                actionActorExecutor));
                            } else if (element instanceof SceneParam) {
								// append value of variables
								String var = ((SceneParam)element).getName();
								String val = "";
								if (mProject.hasVariable(var)) {
									val = ((StringValue)mProject.getValueOf(var)).getValue();
									textBuilder.add(val);
								}
							} else {
                                // Append the text to the activity
                                textBuilder.add(element.getText(map));
                            }
                        }
                        // 
                        final String punctuation = uttr.getPunctuationMark();
                        // Schedule the activity
                        mScheduler.schedule(
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
                mLogger.message("Awaiting player worker '" + worker + "'");
                // Join the playback task
                worker.join();
                // Continue after joining
                finished = true;
                // Print some information
                mLogger.message("Joining player worker '" + worker + "'");
            } catch (final InterruptedException exc) {
                // Print some information
                mLogger.warning("Aborting player worker '" + worker + "'");
                // Terminate playback task
                worker.abort();
            }
        }
        // Print some information
        mLogger.message("Continuing '" + process + "'");
    }
}
