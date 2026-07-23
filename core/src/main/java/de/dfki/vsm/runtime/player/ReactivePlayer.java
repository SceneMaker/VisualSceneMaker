package de.dfki.vsm.runtime.player;

import de.dfki.vsm.model.project.PlayerConfig;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.PlayActionActivity.PlayMode;
import de.dfki.vsm.model.scenescript.*;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.ActionBlockingUtil;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.interpreter.Process;
import de.dfki.vsm.runtime.interpreter.error.SceneDoesNotExists;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.interpreter.value.StructValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.event.event.SceneExecutedEvent;
import de.dfki.vsm.event.event.SceneDoneEvent;
import de.dfki.vsm.event.event.TurnExecutedEvent;
import de.dfki.vsm.event.event.TurnDoneEvent;
import de.dfki.vsm.runtime.logic.LogicEngines;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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

    /**
     * Embeds one marker for every non-blocking action accumulated in {@code pending} since the
     * last real word (or segment start), registers them as one batch, then clears {@code pending}.
     * Two or more non-blocking actions can appear back-to-back with nothing but whitespace between
     * them — e.g. a turn opening with two bracketed commands for different actors before any
     * spoken word, as in {@code [background ...] [Bob: background ...] Lass mich...}. Embedding
     * each as its own bare marker token left several adjacent marker tokens with no real word
     * between them; the character engine's onMarker only ever echoes back the LAST one, silently
     * dropping every earlier action in the run. Bundling the whole run into a single marker
     * guarantees the engine only ever sees one marker token at that position. Mirrors {@code
     * CharamelEmbedExecutor.previewTurn()}'s identical fix (applied there 2026-07-21) — this is the
     * real-playback counterpart, confirmed missing and reproduced 2026-07-23.
     */
    private void flushPendingActions(
            final List<Map.Entry<AbstractActivity, ActivityExecutor>> pending,
            final LinkedList<String> textBuilder,
            final ActivityExecutor turnActorExecutor) {
        if (pending.isEmpty()) {
            return;
        }
        final List<Map.Entry<AbstractActivity, ActivityExecutor>> batch = new ArrayList<>(pending);
        pending.clear();
        final String marker = turnActorExecutor.marker(newId());
        mScheduler.registerBatch(marker, batch);
        textBuilder.add(marker);
    }

    // Flag if we use the JPL
    private boolean mUseJPL = false;
    // The runtime timer
    //private RunTimeTimer mTimer = null;

    // Create the scene player
    public ReactivePlayer(
            final PlayerConfig config,
            final RunTimeProject project) {
        // Initialize the player
        super(config, project);
        // Print some information
        //mLogger.message("Creating reactive player '" + this + "' for project '" + project + "'");
        // Get the JPL flag value
        mUseJPL = Boolean.parseBoolean(mConfig.getProperty("usejpl"));
    }

    // Launch the player
    @Override
    public final void launch() {
        // Print some information
        mLogger.message("Launching reactive player '" + this + "'");
        // Load the fact base
        if (mUseJPL) {
            LogicEngines.get().load("swi/logic.pl");
        }
    }

    // Unload the player
    @Override
    public final void unload() {
        // Print some information
        mLogger.message("Unloading reactive player '" + this + "'");
    }

    // Call the play action activity method
    @Override
    public final void playAction(final String text, final List<AbstractValue> args, final PlayMode mode) {
        // Get the current process
        final Process process = (Process) Thread.currentThread();
        // Make unique worker name
        final String task = process.getName() + ":" + text + "@";
        // Translate the arguments
        final HashMap<String, String> substitutions = getSubstitutions(args);
        // Print some information
        //mLogger.message("Playing Action '" + text + "' in process '" + process + "' on reactive player '" + this + "' with substitutions '" + substitutions.toString() + "'");
        // Create playback task
        final PlayerWorker worker;
        worker = new PlayerWorker(task) {
            @Override
            public void run() {
                // Parsing command string
                // TODO: Better use our parser for that!!!
                final String syntax = text.trim();
                String actor = "";
                String name = "";
                final LinkedList<ActionFeature> features = new LinkedList<>();

                int cnt = 0;

                if (syntax.startsWith("[") && syntax.endsWith("]")) {
                    // PG: changed action and action feature parser to be more powerful
                    // matching something like: agent Action x=2.5 y=0.0 z=-13.0 w=3.4254345E-5 text='Someone wants a beer!' other=bad some='things' state='Da=fuck.continued and -others']
                    final Pattern pattern = Pattern.compile(
                            "^\\w+|\\w+\\s|\\w+]"
                                    + "|[A-Za-z_][A-Za-z0-9_]*='[^']*'"
                                    + "|[A-Za-z_][A-Za-z0-9_]*=\"[^\"]*\""
                                    + "|[A-Za-z_][A-Za-z0-9_]*=-?[0-9]+(?:\\.[0-9]+)?(?:E-?[0-9]+)?"
                                    + "|[A-Za-z_][A-Za-z0-9_]*=[^\\s\\]]+"
                    );
                    final Matcher matcher = pattern.matcher(syntax);
                    while (matcher.find()) {
                        final String token = matcher.group().trim();
                        if (cnt == 0) {
                            actor = token;
                        } else if (cnt == 1) {
                            name = token;
                            name = (name.contains("]")) ? name.replace("]", "") : name;
                        } else if (token.contains("=")) {
                            String[] pair = token.split("=", 2);
                            features.add(new ActionFeature(0, pair[0].length(), pair[0], pair[1]));
                        }
                        cnt++;
                    }
                }
                // Schedule the activity without delay but blocking
                final ActionActivity activity = new ActionActivity(actor, name, text, features, substitutions);
                final PlayMode effectiveMode = mode != null ? mode : PlayMode.Default;
                activity.setType(effectiveMode == PlayMode.Concurrent
                        ? AbstractActivity.Type.parallel
                        : AbstractActivity.Type.blocking);
                mScheduler.schedule(0, null, activity, mProject.getAgentDevice(actor));
            }
        };
        // Start the playback task
        worker.start();
        // Wait for playback task
        boolean finished = false;
        while (!finished) {
            try {
                // Join the playback task
                worker.join();
                // Continue after joining
                finished = true;
                // Print some information
                //mLogger.message("Joining player worker '" + worker + "'");
            } catch (final InterruptedException exc) {
                // Print some information
                //mLogger.warning("Aborting player worker '" + worker + "'");
                // Terminate playback task
                worker.abort();
            }
        }
        // Print some information
        //mLogger.message("Continuing '" + process + "'");
    }

    // Call the play scene group method
    @Override

    public final void playScene(final String name, final List<AbstractValue> args) throws SceneDoesNotExists {
        // Get the current process
        final Process process = (Process) Thread.currentThread();
        // Make unique worker name
        final String task = process.getName() + ":" + name + "@";
        // Translate the arguments
        final HashMap<String, String> substitutions = getSubstitutions(args);
        // Print some information
        mLogger.message("Playing Scene '" + name + "' in process '" + process + "' on reactive player '" + this + "' with substitutions '" + substitutions.toString() + "'");

        // Get the scene object
        final SceneScript script = mProject.getSceneScript();
        String slang = null;
        // Prefer the project-level language setting when set and the scene exists in it
        final String preferred = mProject.getPreferredLanguage();
        if (preferred != null && script.getSceneGroup(preferred, name) != null) {
            slang = preferred;
        } else {
            // Fall back to first language that has the scene
            for (String str : script.getLangSet()) {
                if (script.getSceneGroup(str, name) != null) {
                    slang = str;
                    break;
                }
            }
        }
        if (slang == null) {
            throw new SceneDoesNotExists(name);
        }
        final SceneGroup group = script.getSceneGroup(slang, name);
        final SceneObject scene = group.select();

        final String sceneNodeId = process.getNode() != null ? process.getNode().getId() : "";
        final String sceneParentId =
                process.getNode() != null && process.getNode().getParentNode() != null
                        ? process.getNode().getParentNode().getId()
                        : "";
        // Fire scene-started event and record history
        mProject.getEventDispatcher().convey(new SceneExecutedEvent(this, scene, sceneNodeId, sceneParentId));
        mProject.recordScenePlay(scene.getName(), scene.getLanguage(), scene.getLower(), scene.getUpper());

        // Create playback task
        final PlayerWorker worker = new PlayerWorker(task) {

            @Override
            public void run() {
                // numerical turn information
                int turn_cnt = 0;
                int turn_number = scene.getTurnList().size();

                for (SceneTurn turn : scene.getTurnList()) {
                    // increment turn_cnt;
                    turn_cnt++;

                    // Fire turn-started event
                    mProject.getEventDispatcher().convey(new TurnExecutedEvent(ReactivePlayer.this, turn));

                    // Get executor for this turn
                    final ActivityExecutor turnActorExecutor = mProject.getAgentDevice(turn.getSpeaker());

                    // numerical utterance information
                    int utterance_cnt = 0;
                    int utterance_number = turn.getUttrList().size();

                    // Serially play the utterances
                    for (SceneUttr uttr : turn.getUttrList()) {
                        // increment utterance_cnt;
                        utterance_cnt++;

                        //mLogger.message("Utterance " + uttr.getText().trim());
                        LinkedList<String> textBuilder = new LinkedList<>();
                        LinkedList<ActivityWorker> observedWorkerList = new LinkedList<>();
                        final String punctuation = uttr.getPunctuationMark();
                        // Non-blocking actions since the last real word / segment start, not yet
                        // embedded as a marker — see flushPendingActions()'s docs for why these are
                        // batched into a single marker instead of one each.
                        final List<Map.Entry<AbstractActivity, ActivityExecutor>> pendingActions = new ArrayList<>();

                        for (final UttrElement element : uttr.getWordList()) {
                            //mLogger.message("element " + element);

                            if (element instanceof ActionObject) {
                                final ActionObject action = (ActionObject) element;
                                // Get the actor name of this action
                                final String actor = action.getActor();
                                // Get the executor for this action
                                final ActivityExecutor actionActorExecutor
                                        = (actor != null && !actor.isEmpty() ? mProject.getAgentDevice(actor) : turnActorExecutor);

                                if (ActionBlockingUtil.requiresUtteranceSplit(action)) {
                                    // A split-point action (2026-07-15: emotion opted in via
                                    // blocking='true'; 2026-07-16: "pause" always) can't just fire
                                    // as an inline marker mid-speech: nothing would actually wait
                                    // for it until the WHOLE utterance's speech already finished,
                                    // so speech itself never pauses. The speech has to genuinely
                                    // stop here instead. Flush whatever's been said so far as its
                                    // own utterance segment (empty at the very start of the
                                    // utterance, so nothing to flush there), run the blocking
                                    // action to completion, then keep collecting a fresh segment
                                    // for whatever follows — flushed either at the next blocking
                                    // action or below, after the word loop, as the trailing
                                    // segment. This is the "implicit punctuation split": a
                                    // mid-utterance blocking action behaves as if the author had
                                    // ended the sentence there and started a new one. Placed at
                                    // the very start or end of the utterance, this degrades to a
                                    // plain "do this, then speak" / "speak, then do this"
                                    // sequencing with no split needed.
                                    flushPendingActions(pendingActions, textBuilder, turnActorExecutor);
                                    if (!textBuilder.isEmpty()) {
                                        mScheduler.schedule(
                                                0,
                                                observedWorkerList,
                                                new SpeechActivity(
                                                        turn.getSpeaker(),
                                                        textBuilder,
                                                        "", // mid-utterance segment — not a real sentence end
                                                        turn,
                                                        turn_cnt,
                                                        turn_number,
                                                        utterance_cnt,
                                                        utterance_number),
                                                turnActorExecutor);
                                        if (isDone()) {
                                            return;
                                        }
                                        textBuilder = new LinkedList<>();
                                        observedWorkerList = new LinkedList<>();
                                    }
                                    if (ActionBlockingUtil.isPause(action)) {
                                        // Pure timing, no visual effect, no target device — a plain
                                        // sleep right here rather than a dispatch to any executor
                                        // (unlike emotion, "pause" isn't tied to any character-
                                        // rendering plugin, so it shouldn't go through one).
                                        try {
                                            Thread.sleep(ActionBlockingUtil.parsePauseDurationMs(action));
                                        } catch (final InterruptedException exc) {
                                            Thread.currentThread().interrupt();
                                            return;
                                        }
                                    } else {
                                        final ActionActivity blockingActivity = new ActionActivity(
                                                (action.getActor() == null) ? turn.getSpeaker() : action.getActor(),
                                                action.getName(),
                                                action.getText(substitutions),
                                                action.getFeatureList(),
                                                substitutions);
                                        blockingActivity.setType(AbstractActivity.Type.blocking);
                                        // No delay, no observed workers — schedule() itself joins on
                                        // this since its type is blocking (ActivityScheduler.schedule()).
                                        mScheduler.schedule(0, null, blockingActivity, actionActorExecutor);
                                    }
                                    if (isDone()) {
                                        return;
                                    }
                                } else {
                                    // Accumulate rather than embed its own marker immediately — see
                                    // flushPendingActions()'s docs for why (adjacent bare markers
                                    // with nothing real between them get collapsed to just the last
                                    // one by the character engine's onMarker).
                                    pendingActions.add(Map.entry(
                                            new ActionActivity(
                                                    (action.getActor() == null) ? turn.getSpeaker() : action.getActor(), // added PG 5.4.2016
                                                    //action.getMode(),
                                                    action.getName(),
                                                    action.getText(substitutions),
                                                    action.getFeatureList(),
                                                    substitutions),
                                            actionActorExecutor));
                                }
                            } //else if (element instanceof SceneParam) {
                            //TODO: Get parameter from list
                            // append value of variables
                            //String var = ((SceneParam) element).getName();
                            //String val = "";
                            //try {
                            //    textBuilder.add(process.getEnvironment().read(var).getValue().toString());
                            //} catch (final InterpreterError exc) {
                            //    exc.printStackTrace();
                            //}
                            //    }
                            else {
                                flushPendingActions(pendingActions, textBuilder, turnActorExecutor);
                                // Append the text to the activity
                                textBuilder.add(element.getText(substitutions));
                            }
                        }
                        flushPendingActions(pendingActions, textBuilder, turnActorExecutor);
                        // mLogger.message("Scheduling Speech Activity:\n" + textBuilder + "");
                        // Schedule the (final, or only) segment — this one carries the utterance's
                        // real punctuation mark regardless of how many blocking-action splits
                        // came before it.
                        mScheduler.schedule(
                                0, // Schedule without delay
                                observedWorkerList,
                                new SpeechActivity(
                                        turn.getSpeaker(),
                                        textBuilder,
                                        punctuation,
                                        turn,
                                        turn_cnt,
                                        turn_number,
                                        utterance_cnt,
                                        utterance_number),
                                turnActorExecutor);
                        // Check for interruption
                        if (isDone()) {
                            return;
                        }
                    }
                    // Fire turn-done event
                    mProject.getEventDispatcher().convey(new TurnDoneEvent(ReactivePlayer.this, turn));
                }
                // Fire scene-done event
                mProject.getEventDispatcher().convey(new SceneDoneEvent(
                        ReactivePlayer.this,
                        scene,
                        sceneNodeId,
                        sceneParentId));
            }
        };
        // Start the playback task
        worker.start();

        // Wait for playback task
        boolean finished = false;
        while (!finished) {
            try {
                // Print some information
                //mLogger.message("Awaiting player worker '" + worker + "'");
                // Join the playback task
                worker.join();
                // Continue after joining
                finished = true;
                // Print some information
                //mLogger.message("Joining player worker '" + worker + "'");
            } catch (final InterruptedException exc) {
                // Print some information
                //mLogger.warning("Aborting player worker '" + worker + "'");
                // Terminate playback task
                worker.abort();
            }
        }

        // Print some information
        //mLogger.message("Continuing '" + process + "'");
    }

    // Translate the arguments
    private HashMap<String, String> getSubstitutions(final List<AbstractValue> args) {
        final HashMap<String, String> substitutions = new HashMap<>();
        if (args != null && !args.isEmpty()) {
            for (final Object object : args) {
                if (object instanceof AbstractValue) {
                    final AbstractValue value = (AbstractValue) object;
                    if (value instanceof StructValue) {
                        final StructValue struct = (StructValue) value;
                        for (Entry<String, AbstractValue> entry : struct.getValueMap().entrySet()) {
                            substitutions.put(entry.getKey(), entry.getValue().getValue().toString());
                            mLogger.message("SUBSITUTION " + entry.getKey() + "->" + entry.getValue().getValue().toString());
                        }
                    }
                }
            }
        }
        return substitutions;
    }
}
