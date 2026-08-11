package de.dfki.vsm.xtension.charamelEmbed;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.model.scenescript.ActionObject;
import de.dfki.vsm.model.scenescript.SceneObject;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.model.scenescript.SceneTurn;
import de.dfki.vsm.model.scenescript.SceneUttr;
import de.dfki.vsm.model.scenescript.ScriptParser;
import de.dfki.vsm.model.scenescript.UttrElement;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.ActionBlockingUtil;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.bootstrap.PlatformBootstrap;
import de.dfki.vsm.runtime.interpreter.value.BooleanValue;
import de.dfki.vsm.runtime.plugin.CharacterPreviewCapable;
import de.dfki.vsm.runtime.plugin.SpeechBreakCapable;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Drives a self-hosted VuppetMaster character page through the engine's JavaScript API
 * (vm.speak(...)), as opposed to {@code charamel-ws} which drives a Charamel-hosted page via the
 * timeline-JSON protocol.
 *
 * VSM → page:  a small JSON command envelope, e.g. {@code {"cmd":"speak", ...}}.
 * page → VSM:  marker strings reconstructed from the engine's onMarker(name,value) callback, in the
 *              exact form this executor's feedback handling parses (identical to charamel-ws).
 *
 * The character page ({@code /renderer/character.html} + {@code /renderer/vm-adapter.js}) is served
 * from the plugin JAR classpath and is transport-agnostic. The actual VSM↔page transport is chosen
 * at {@link #launch()}: {@link JettyTransport} on desktop (embedded server + WebSocket) and
 * {@link AndroidBridgeTransport} on Android (WebView + JS bridge). Confining all Jetty/Javalin
 * references to {@link JettyTransport} keeps this executor {@code androidCompatible}.
 *
 * @author Patrick Gebhard
 */
public class CharamelEmbedExecutor extends ActivityExecutor
        implements CharamelTransport.Listener, CharacterPreviewCapable, SpeechBreakCapable {

    static long sUtteranceId = 0;

    // Blocking emotion actions (2026-07-15, see ReactivePlayer's isBlockingAction/utterance-split
    // logic): VuppetMaster gives no completion callback at all for an emotion transition (nor,
    // later, a gesture) — unlike speech, there is no id:start/id:stop equivalent to wait on. So a
    // blocking emotion's "done" is *estimated* from its own declared envelope duration
    // (attack+hold+decay, the same numbers the authoring UI's ParameterEnvelopeEditor already
    // shows/defaults), padded by a fixed buffer to absorb the gap between this dispatch call and
    // the engine actually starting the transition (JS scheduling + WebSocket transport latency).
    // This is a documented estimate, not a measurement — if VuppetMaster ever exposes a real
    // "transition finished" callback, prefer that over this timer.
    private static final long BLOCKING_DEFAULT_ATTACK_MS = 200;
    private static final long BLOCKING_DEFAULT_HOLD_MS = 20;
    private static final long BLOCKING_DEFAULT_DECAY_MS = 300;
    private static final long BLOCKING_TRANSPORT_BUFFER_MS = 50;

    // Bone-animation envelope defaults, matching vm.animateBone's own engine-side defaults (500/500,
    // confirmed 2026-08-11 against the shipped engine bundle) — deliberately NOT the emotion
    // defaults above, which are a different VuppetMaster call with a different envelope.
    private static final long BONE_DEFAULT_ATTACK_MS = 500;
    private static final long BONE_DEFAULT_DECAY_MS = 500;
    // Procedural-nod defaults — must track vm-adapter.js's 'nod' case, which owns the real values.
    private static final long NOD_DEFAULT_REPEATS = 2;
    private static final long NOD_DEFAULT_PERIOD_MS = 400;

    // Upper bound on how long broadcastSpeakAndAwaitStop() will wait for a "<vmuid>:stop" feedback
    // marker before giving up. Without this, a page reload/relaunch (or transport hiccup) that
    // orphans a pending id leaves the calling thread parked in mPendingSpeechIds.wait() forever.
    // Before WebUiServer started dispatching previewTurn()/previewAction() to a background executor
    // (2026-07-17), that stuck thread *was* the Jetty HTTP-handler thread serving the preview
    // request, and the browser eventually dropped the still-open, silent connection itself,
    // reporting a bare "network connection was lost" with nothing logged server-side. The dispatch
    // fix moved the wait off the request thread, but this timeout stays as the actual fix for the
    // underlying stuck-wait case, wherever it's called from.
    private static final long SPEECH_FEEDBACK_TIMEOUT_MS = 20_000;

    // Utterance ids whose "stop" feedback hasn't arrived yet. Guards a wait/notify — any thread
    // can wait here, not just an ActivityWorker: real playback waits on an ActivityWorker thread
    // (via ActivityScheduler's join-on-blocking contract), and authoring-time preview turns
    // (previewTurn()), dispatched by WebUiServer to a background executor thread, need the exact
    // same "block until this utterance's speech really finishes" wait (bounded by
    // SPEECH_FEEDBACK_TIMEOUT_MS above).
    private final java.util.Set<String> mPendingSpeechIds = new java.util.HashSet<>();
    // Pending actions for markers embedded in a previewTurn() call, keyed by marker string. Resolved
    // directly here rather than via the interpreter's ActivityScheduler, since preview dispatch runs
    // on the caller's thread (e.g. an HTTP handler), not an ActivityWorker.
    private final Map<String, Runnable> mPreviewMarkerMap = new ConcurrentHashMap<>();
    private final AtomicLong mPreviewMarkerId = new AtomicLong();
    protected final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    // Serializes every dispatch to this character's own engine instance — execute() (real
    // playback, an ActivityWorker thread), previewTurn(), and previewAction() (each their own
    // HTTP-handler thread) all acquire this before touching the transport. Without it, two
    // independent callers (a real Play plus a concurrent preview test, or — in a collaborative
    // session — two different people testing the same character at once) can each issue their own
    // speakCommand()/setEmotion() call to the *same* connected browser's VuppetMaster instance at
    // the same time; the engine can't tolerate that overlap and the later call fails outright
    // ("speak ended with error", confirmed 2026-07-16 by firing two previewTurn() calls
    // concurrently). A plain intrinsic lock is safe here (not a dedicated ReentrantLock) because
    // the one path that re-enters — previewTurn() dispatching a same-actor blocking action via
    // runBlockingPreviewAction() -> execute() — does so on the same calling thread, and Java
    // monitors are reentrant per-thread.
    private final Object mDispatchLock = new Object();

    private CharamelTransport mTransport;

    // --- Config (read in launch()). Port/license/appName/engineUrl are transport concerns and are
    // read by JettyTransport itself; the executor keeps only the SceneFlow variable names it writes.
    private String mConnectedVar = "";
    private String mReadyVar = "";
    private String mSpeakingVar = "";
    private String mTurnVar = "turn_utterance";

    public CharamelEmbedExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    @Override
    public synchronized String marker(long id) {
        return "${'" + id + "'}$";
    }

    /** VuppetMaster's TTS backend (Azure) honors standard SSML {@code <break>} tags embedded
     *  directly in speakCommand()'s text — confirmed 2026-07-23 via a live raw-speak test
     *  (requested 3000ms measured as almost exactly 3000ms of silence, vs. the ~1000ms+ extra
     *  tail latency the split-and-sleep fallback incurs per pause). */
    @Override
    public String speechBreakMarkup(long durationMs) {
        return "<break time=\"" + durationMs + "ms\"/>";
    }

    private synchronized Long getVMUtteranceId() {
        return ++sUtteranceId;
    }

    /**
     * The Android bridge transport, when running on Android — the seam the Android host wires to a
     * WebView. Returns {@code null} on desktop (where the Jetty transport is used instead).
     */
    public AndroidBridgeTransport getAndroidBridge() {
        return (mTransport instanceof AndroidBridgeTransport) ? (AndroidBridgeTransport) mTransport : null;
    }

    // ------------------------------------------------------------------ authoring-time preview

    @Override
    public int getPreviewPort() {
        return mTransport != null ? mTransport.getPreviewPort() : -1;
    }

    @Override
    public void setPreviewMuted(boolean muted) {
        if (mTransport != null) mTransport.setPreviewMuted(muted);
    }

    @Override
    public void previewAction(String rawActionBody) {
        final Object parsed = ScriptParser.run(rawActionBody, true, false, true, false, false);
        if (!(parsed instanceof ActionObject)) {
            mLogger.failure("charamel-embed preview: failed to parse action '" + rawActionBody + "'");
            return;
        }
        final ActionObject action = (ActionObject) parsed;
        synchronized (mDispatchLock) {
            parseAction(action.getName(), action.getFeatureList());
        }
    }

    @Override
    public void previewRawText(String rawText) {
        // Deliberately skips SceneScript's grammar (SceneScript.parseTXT), marker embedding, and
        // the actor/word split previewTurn() does — rawText goes to speakCommand() exactly as
        // given, so markup VSM's own grammar can't represent (e.g. SSML) reaches the engine
        // unfiltered. No voice override: this is a one-off diagnostic call, not tied to any
        // SceneFlow agent, so there's no turn speaker to look a configured voice up by.
        synchronized (mDispatchLock) {
            final String vmuid = "preview_raw_" + mPreviewMarkerId.incrementAndGet();
            broadcastSpeakAndAwaitStop(vmuid, rawText, null);
        }
    }

    @Override
    public void previewTurn(String rawTurn) {
        // Synthetic scene header required by the grammar; "preview" (letters only) is a valid
        // identifier — a leading underscore, as in "__preview__", is not (IDENTIFIER must start
        // with a letter per lexxer.jflex).
        final SceneScript script = new SceneScript();
        if (!script.parseTXT("scene de preview\n" + rawTurn) || script.getSceneList().isEmpty()) {
            mLogger.failure("charamel-embed preview: failed to parse turn '" + rawTurn + "'");
            return;
        }
        final SceneObject scene = script.getSceneList().get(0);
        if (scene.getTurnList().isEmpty()) {
            mLogger.failure("charamel-embed preview: no turn found in '" + rawTurn + "'");
            return;
        }
        final SceneTurn turn = scene.getTurnList().get(0);
        final String voice = mProject.getAgentConfig(turn.getSpeaker()) != null
                ? mProject.getAgentConfig(turn.getSpeaker()).getProperty("voice") : null;

        // Holds mDispatchLock for the whole turn — see its declaration for why this is needed
        // (two independent callers, e.g. two collaborators or a real Play overlapping a preview
        // test, dispatching to the same character's engine at once breaks it) and why it's safe
        // (the one internal re-entry, runBlockingPreviewAction -> execute() for a same-actor
        // blocking action, happens on this same thread).
        synchronized (mDispatchLock) {
            // Mirrors ReactivePlayer.playScene()'s real-execution split logic (see
            // ActionBlockingUtil) so testing a turn in the SIA preview panel behaves the same as a
            // real run: a split-point action (pause, or blocking='true') genuinely pauses speech
            // instead of firing as an inline marker while speech keeps playing.
            for (final SceneUttr uttr : turn.getUttrList()) {
                StringBuilder segment = new StringBuilder();
                // Non-blocking actions since the last real word / segment start, not yet embedded as
                // a marker — see flushPendingMarkerActions() for why these are batched into a single
                // marker instead of one each.
                final java.util.List<Runnable> pending = new java.util.ArrayList<>();
                for (final UttrElement element : uttr.getWordList()) {
                    if (element instanceof ActionObject) {
                        final ActionObject action = (ActionObject) element;
                        if (ActionBlockingUtil.requiresUtteranceSplit(action)) {
                            flushPendingMarkerActions(pending, segment);
                            if (segment.length() > 0) {
                                previewSpeakAndAwaitStop(segment.toString().trim(), voice);
                                segment = new StringBuilder();
                            }
                            runBlockingPreviewAction(action, turn.getSpeaker());
                        } else {
                            pending.add(buildPreviewActionRunnable(action));
                        }
                    } else {
                        flushPendingMarkerActions(pending, segment);
                        segment.append(element.getText(new HashMap<>())).append(' ');
                    }
                }
                flushPendingMarkerActions(pending, segment);
                segment.append(uttr.getPunctuationMark());
                if (!segment.toString().isBlank()) {
                    previewSpeakAndAwaitStop(segment.toString().trim(), voice);
                }
            }
        }
    }

    /**
     * Embeds one marker for every action accumulated in {@code pending} since the last real word (or
     * segment start), then clears it. Multiple non-blocking actions can appear back-to-back with
     * nothing but whitespace between them — e.g. a turn opening with two bracketed commands before
     * any spoken word, as in {@code [background ...] [Bob: background ...] Hallo}. Embedding each as
     * its own {@code ${'id'}$} token left two (or more) bare marker tokens adjacent with no real word
     * between them; the engine only ever echoed back the last one via onMarker, silently dropping
     * every earlier action in the run (confirmed 2026-07-21). Bundling the whole run into a single
     * marker — fired as one batch of actions in original order — guarantees the engine only ever
     * sees one marker token at that position, immediately followed by real spoken content.
     */
    private void flushPendingMarkerActions(java.util.List<Runnable> pending, StringBuilder segment) {
        if (pending.isEmpty()) return;
        final java.util.List<Runnable> batch = new java.util.ArrayList<>(pending);
        pending.clear();
        final String markerKey = marker(mPreviewMarkerId.incrementAndGet());
        mPreviewMarkerMap.put(markerKey, () -> batch.forEach(Runnable::run));
        segment.append(markerKey).append(' ');
    }

    private void previewSpeakAndAwaitStop(String rawText, String voice) {
        final String vmuid = "preview_" + mPreviewMarkerId.incrementAndGet();
        broadcastSpeakAndAwaitStop(vmuid, rawText, voice);
    }

    /** Runs a split-point action to completion on the calling thread — either a {@code pause}
     *  (a plain sleep, no target device) or another action that opted into blocking (dispatched to
     *  its actor's own executor, which is responsible for its own wait — see
     *  {@code CharamelEmbedExecutor.execute()}'s {@code Type.blocking} handling). */
    private void runBlockingPreviewAction(ActionObject action, String turnSpeaker) {
        if (ActionBlockingUtil.isPause(action)) {
            try {
                Thread.sleep(ActionBlockingUtil.parsePauseDurationMs(action));
            } catch (InterruptedException exc) {
                Thread.currentThread().interrupt();
            }
            return;
        }
        final String actionActor = (action.getActor() == null || action.getActor().isBlank())
                ? turnSpeaker : action.getActor();
        final ActivityExecutor target = mProject.getAgentDevice(actionActor);
        if (target == null) {
            mLogger.warning("charamel-embed preview: no device for actor '" + actionActor + "', dropping ["
                    + action.getName() + "]");
            return;
        }
        final ActionActivity activity = new ActionActivity(
                actionActor, action.getName(), action.getText(new HashMap<>()),
                action.getFeatureList(), new HashMap<>());
        activity.setType(AbstractActivity.Type.blocking);
        target.execute(activity); // blocks this thread — same Type.blocking contract as ActivityScheduler
    }

    /** Resolves the action's target device (self, or another agent's, for cross-actor commands like
     *  {@code [Bob smile]}) and returns what to run when its marker fires back from the page. */
    private Runnable buildPreviewActionRunnable(ActionObject action) {
        final String actionActor = action.getActor();
        if (actionActor == null || actionActor.isBlank()) {
            // Self-actor: dispatch straight through parseAction(), NOT execute() — this callback
            // runs on processTimeMarkMessage's fresh thread while previewTurn() is still mid-turn,
            // holding mDispatchLock until the WHOLE turn finishes (see its own declaration). Routing
            // through execute() would need that same lock, so a leading marker action (e.g. a turn
            // opening with [background ...]) would queue behind every remaining segment of the turn
            // instead of firing when its marker actually arrives (confirmed 2026-07-18: the color
            // change landed only after both utterances had already finished speaking). Non-split
            // actions reaching this path are always fire-and-forget — a blocking variant is instead
            // routed through runBlockingPreviewAction, which genuinely needs the lock — so calling
            // parseAction() directly here, unsynchronized, is safe.
            return () -> parseAction(action.getName(), action.getFeatureList());
        }
        final ActivityExecutor target = mProject.getAgentDevice(actionActor);
        if (target == null) {
            return () -> mLogger.warning("charamel-embed preview: no device for actor '" + actionActor
                    + "', dropping [" + action.getName() + "]");
        }
        final ActionActivity activity = new ActionActivity(
                actionActor, action.getName(), action.getText(new HashMap<>()), action.getFeatureList(), new HashMap<>());
        return () -> target.execute(activity);
    }

    // ------------------------------------------------------------------ execution

    @Override
    public void execute(AbstractActivity activity) {
        final String actor = activity.getActor();

        if (!(activity instanceof SpeechActivity) && activity.getType() != AbstractActivity.Type.blocking) {
            // Non-blocking, non-speech actions (a marker-triggered inline command like
            // "background" or "emotion" without blocking='true') skip mDispatchLock entirely —
            // see its declaration: that lock exists to stop two INDEPENDENT dispatch flows (a real
            // Play plus a concurrent preview test, say) from issuing overlapping
            // speakCommand()/setEmotion() calls, not to make a same-actor fire-and-forget action
            // wait for whatever the SAME character's OWN in-progress SpeechActivity is still doing.
            // previewTurn()'s own dispatch (buildPreviewActionRunnable, above) already established
            // and validated this exact pattern for the preview path (2026-07-18: routing a
            // non-blocking self-actor action through this synchronized execute() made it wait for
            // the WHOLE enclosing turn to finish before firing — confirmed then via a background
            // color that only visibly changed once both utterances had already finished speaking).
            // This is the real-playback counterpart of that same fix, confirmed missing and
            // reproduced 2026-07-23 with both background and emotion delayed until utterance end.
            parseAction(activity.getName(), activity.getFeatures());
            return;
        }

        // See mDispatchLock's declaration: serializes every dispatch to this character (real
        // playback, previewTurn(), previewAction()) so two independent callers can't issue
        // overlapping speakCommand()/setEmotion() calls to the same connected browser's engine.
        synchronized (mDispatchLock) {
            if (activity instanceof SpeechActivity) {
                SpeechActivity sa = (SpeechActivity) activity;
                String text = sa.getTextOnly("${'").trim();
                LinkedList<String> timemarks = sa.getTimeMarks("${'");

                if (text.isEmpty()) {
                    // No text, but there may be co-located marker activities to fire directly.
                    for (String tm : timemarks) {
                        mLogger.warning("Directly executing activity at timemark " + tm);
                        mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                    }
                    return;
                }

                final String vmuid = actor + "_utterance_" + getVMUtteranceId();
                final String voice = mProject.getAgentConfig(actor) != null
                        ? mProject.getAgentConfig(actor).getProperty("voice") : null;

                activity.setType(AbstractActivity.Type.blocking);

                // Mirror the current turn text into the SceneFlow model (best-effort, as charamel-ws).
                if (mProject.hasVariable(mTurnVar)) {
                    mProject.setVariable(mTurnVar, text);
                }
                if ((sa.getTurnNumber() == 1) && (sa.getUtteranceNumber() == 1)
                        && mProject.hasVariable(mSpeakingVar)) {
                    mProject.setVariable(mSpeakingVar, new BooleanValue(true));
                }

                broadcastSpeakAndAwaitStop(vmuid, sa.getText(), voice);

                if ((sa.getTurnNumber() == sa.getTotalTurns()) && (sa.getUtteranceNumber() == sa.getTotalUtterances())
                        && mProject.hasVariable(mSpeakingVar)) {
                    mProject.setVariable(mSpeakingVar, new BooleanValue(false));
                }
            } else {
                parseAction(activity.getName(), activity.getFeatures());
                if (activity.getType() == AbstractActivity.Type.blocking) {
                    sleepForBlockingEnvelope(activity.getName(), activity.getFeatures());
                }
            }
        }
    }

    /**
     * Non-speech actions, reachable from a SceneFlow PlayAction command ({@code [Xenia happy]}) or an
     * inline scene marker ({@code [Xenia background color='#1a2a6c']}). Each broadcasts a JSON envelope
     * that {@code vm-adapter.js} maps to a VuppetMaster JS call. This method itself is always
     * fire-and-forget; a {@code blocking='true'} emotion additionally pauses the *caller*
     * ({@code execute()}, via {@link #sleepForBlockingEnvelope}) for the envelope's own duration
     * after this returns — see that method's docs for why a sleep rather than a real completion signal.
     */
    private void parseAction(String name, LinkedList<ActionFeature> f) {
        switch (name == null ? "" : name.toLowerCase()) {
            case "stop":
                if (mTransport != null) mTransport.stop();
                break;
            case "background": {
                // Sets the page backdrop shown behind the transparent avatar canvas.
                String color = getActionFeatureValue("color", f);
                broadcast("{\"cmd\":\"background\",\"color\":\"" + escapeJson(color) + "\"}");
                break;
            }
            case "clearemotion":
                broadcast("{\"cmd\":\"clearEmotion\"}");
                break;
            // Bone animation (vm.animateBone). Generic form: [Xenia bone name='Head' x='-12' hold='300']
            case "bone":
                broadcastBone(f);
                break;
            // Procedural head movement built on the same API — see vm-adapter.js's
            // scheduleOscillation() for the per-leg scheduling and the axis each one uses.
            case "nod":
            case "shake":
                broadcastOscillation(name.toLowerCase(), f);
                break;
            // Generic form: [Xenia emotion type='happy' intensity='0.8' ...]
            case "emotion":
                broadcastEmotion(getActionFeatureValue("type", f), f);
                break;
            // Convenience named emotions (IEmotionType) — [Xenia happy intensity='0.8']
            case "happy": case "sad": case "angry": case "tear": case "disgust": case "surprise":
            case "smile": case "excited": case "fear": case "bored": case "relaxed":
                broadcastEmotion(name.toLowerCase(), f);
                break;
            default:
                mLogger.warning("charamel-embed: unknown action '" + name + "'");
        }
    }

    /**
     * Blocks the calling thread (an {@code ActivityWorker}, per {@code ActivityScheduler.schedule()}'s
     * join-on-{@code Type.blocking} contract) for the estimated duration of a blocking command's
     * envelope, plus {@link #BLOCKING_TRANSPORT_BUFFER_MS}. Supported by "emotion" (and its
     * convenience named-emotion aliases, e.g. {@code [Xenia happy blocking='true']}), by "bone", and
     * by "nod" — background/clearEmotion/stop have no meaningful "duration" to wait out, so a
     * blocking flag on those is silently ignored here (nothing to sleep for).
     *
     * <p><b>Why a timer instead of a real completion signal:</b> unlike {@code speak} (which has the
     * engine-native {@code id:start}/{@code id:stop} marker handshake), VuppetMaster gives no callback
     * at all when an emotion transition (or, later, a gesture) actually finishes. So "done" here is an
     * estimate derived from the command's own declared timing — the same attack/hold/decay values the
     * authoring UI's ParameterEnvelopeEditor shows/defaults — not a measurement. The
     * {@value #BLOCKING_TRANSPORT_BUFFER_MS}ms buffer accounts for the gap between this method
     * returning (envelope dispatched) and the engine actually starting the transition: JS event-loop
     * scheduling plus the WebSocket round-trip to the character page. It is a fixed guess, not derived
     * from any measurement of real transport latency — if it proves too short (character still
     * mid-transition when the next scene action fires) or unnecessarily long, adjust the constant.
     */
    private void sleepForBlockingEnvelope(String name, LinkedList<ActionFeature> f) {
        final String lower = name == null ? "" : name.toLowerCase();
        final long durationMs;
        if ("nod".equals(lower) || "shake".equals(lower)) {
            // These run repeats × period ms of scheduled cycles page-side (vm-adapter.js), so their
            // duration comes from those two rather than from an attack/hold/decay envelope. Defaults
            // must track the adapter's own.
            long repeats = Math.max(1, parseMsOrDefault(getActionFeatureValue("repeats", f), NOD_DEFAULT_REPEATS));
            long period = Math.max(1, parseMsOrDefault(getActionFeatureValue("period", f), NOD_DEFAULT_PERIOD_MS));
            durationMs = repeats * period + BLOCKING_TRANSPORT_BUFFER_MS;
        } else if ("bone".equals(lower)) {
            // Engine-accurate defaults for animateBone (500/500), which differ from the emotion
            // envelope defaults below. An omitted hold means "hold indefinitely" engine-side, so
            // there is no finite duration to wait for — treat it as 0 here rather than blocking the
            // scene forever on a pose that is meant to persist.
            long attack = parseMsOrDefault(getActionFeatureValue("attack", f), BONE_DEFAULT_ATTACK_MS);
            long hold = parseMsOrDefault(getActionFeatureValue("hold", f), 0);
            long decay = parseMsOrDefault(getActionFeatureValue("decay", f), BONE_DEFAULT_DECAY_MS);
            durationMs = attack + hold + decay + BLOCKING_TRANSPORT_BUFFER_MS;
        } else if (isEmotionActionName(lower)) {
            long attack = parseMsOrDefault(getActionFeatureValue("attack", f), BLOCKING_DEFAULT_ATTACK_MS);
            long hold = parseMsOrDefault(getActionFeatureValue("hold", f), BLOCKING_DEFAULT_HOLD_MS);
            long decay = parseMsOrDefault(getActionFeatureValue("decay", f), BLOCKING_DEFAULT_DECAY_MS);
            durationMs = attack + hold + decay + BLOCKING_TRANSPORT_BUFFER_MS;
        } else {
            return;
        }
        try {
            Thread.sleep(durationMs);
        } catch (InterruptedException exc) {
            Thread.currentThread().interrupt();
        }
    }

    private static boolean isEmotionActionName(String name) {
        if (name == null) {
            return false;
        }
        switch (name.toLowerCase()) {
            case "emotion": case "happy": case "sad": case "angry": case "tear": case "disgust":
            case "surprise": case "smile": case "excited": case "fear": case "bored": case "relaxed":
                return true;
            default:
                return false;
        }
    }

    private static long parseMsOrDefault(String val, long defaultMs) {
        if (val == null || val.isBlank()) {
            return defaultMs;
        }
        try {
            return Math.round(Double.parseDouble(val.trim()));
        } catch (NumberFormatException exc) {
            return defaultMs;
        }
    }

    /** Broadcasts an emotion envelope; only features actually provided are included (engine defaults apply). */
    private void broadcastEmotion(String type, LinkedList<ActionFeature> f) {
        if (type == null || type.isBlank()) {
            mLogger.warning("charamel-embed: emotion without a type");
            return;
        }
        StringBuilder sb = new StringBuilder("{\"cmd\":\"emotion\",\"type\":\"").append(escapeJson(type)).append("\"");
        appendNumber(sb, "intensity", getActionFeatureValue("intensity", f));
        appendNumber(sb, "attack",    getActionFeatureValue("attack", f));
        appendNumber(sb, "hold",      getActionFeatureValue("hold", f));
        appendNumber(sb, "decay",     getActionFeatureValue("decay", f));
        sb.append("}");
        broadcast(sb.toString());
    }

    /**
     * Rotates a named bone (vm.animateBone) with an attack/hold/decay envelope.
     *
     * <p>Angles are authored in DEGREES and converted to the engine's radians in vm-adapter.js — see
     * its {@code animateBoneDegrees} for why the conversion lives at the engine boundary rather than
     * here.
     *
     * <p>{@code hold} is forwarded only when the author actually set it: the engine reads an absent
     * hold as "hold indefinitely", which is the documented way to strike a sustained pose (and is
     * how {@code emotion} already behaves). To release such a pose, re-issue the command with the
     * neutral angle and an explicit hold, e.g. {@code [Xenia bone name='Head' x='0' hold='0']}.
     */
    private void broadcastBone(LinkedList<ActionFeature> f) {
        String bone = getActionFeatureValue("name", f);
        if (bone == null || bone.isBlank()) {
            // Only "Head" is exposed by the current VuppetMaster rig, so defaulting keeps the
            // common case terse ([Xenia bone x='-12']) without hiding the parameter.
            bone = "Head";
        }
        StringBuilder sb = new StringBuilder("{\"cmd\":\"bone\",\"bone\":\"").append(escapeJson(bone)).append("\"");
        appendNumber(sb, "x",      getActionFeatureValue("x", f));
        appendNumber(sb, "y",      getActionFeatureValue("y", f));
        appendNumber(sb, "z",      getActionFeatureValue("z", f));
        appendNumber(sb, "attack", getActionFeatureValue("attack", f));
        appendNumber(sb, "hold",   getActionFeatureValue("hold", f));
        appendNumber(sb, "decay",  getActionFeatureValue("decay", f));
        appendBoolean(sb, "additive", getActionFeatureValue("additive", f));
        sb.append("}");
        broadcast(sb.toString());
    }

    /**
     * Procedural head oscillation — {@code nod} (pitch) or {@code shake} (yaw): {@code repeats}
     * cycles of {@code amplitude} degrees peak-to-peak, centred on the neutral pose, one every
     * {@code period} ms.
     *
     * <p>Which axis each maps to, the amplitude/repeats/period defaults and the per-leg scheduling
     * all live in vm-adapter.js's {@code scheduleOscillation} — this side only forwards what the
     * author actually typed, so the two cannot drift apart.
     */
    private void broadcastOscillation(String cmd, LinkedList<ActionFeature> f) {
        String bone = getActionFeatureValue("name", f);
        StringBuilder sb = new StringBuilder("{\"cmd\":\"").append(cmd).append("\"");
        if (bone != null && !bone.isBlank()) {
            sb.append(",\"bone\":\"").append(escapeJson(bone)).append("\"");
        }
        appendNumber(sb, "amplitude", getActionFeatureValue("amplitude", f));
        appendNumber(sb, "repeats",   getActionFeatureValue("repeats", f));
        appendNumber(sb, "period",    getActionFeatureValue("period", f));
        sb.append("}");
        broadcast(sb.toString());
    }

    private static void appendNumber(StringBuilder sb, String key, String val) {
        if (val != null && !val.isBlank()) sb.append(",\"").append(key).append("\":").append(val.trim());
    }

    /** Appends a JSON boolean for an author-typed "true"/"false" (anything else is ignored, leaving
     *  the engine's own default in force rather than silently coercing a typo to false). */
    private static void appendBoolean(StringBuilder sb, String key, String val) {
        if (val == null || val.isBlank()) return;
        String v = val.trim().toLowerCase();
        if (v.equals("true") || v.equals("false")) {
            sb.append(",\"").append(key).append("\":").append(v);
        }
    }

    /**
     * Value of a named action feature, quotes stripped ("" if absent). The value is used literally —
     * to pass a SceneFlow variable, build the command by concatenation in the PlayAction expression,
     * e.g. PlayAction("[Xenia emotion type=" + emo_type + "]"), so VSM evaluates it to the final
     * string ([Xenia emotion type=happy]) before this plugin parses it.
     */
    protected static String getActionFeatureValue(String name, LinkedList<ActionFeature> features) {
        if (features == null) return "";
        return features.stream()
                .filter(af -> af.getKey().equalsIgnoreCase(name))
                .findFirst()
                .map(ActionFeature::getVal)
                .orElse("")
                .replace("'", "");
    }

    private void broadcastSpeak(String id, String text, String voice) {
        StringBuilder sb = new StringBuilder("{");
        sb.append("\"cmd\":\"speak\",");
        sb.append("\"id\":\"").append(escapeJson(id)).append("\",");
        sb.append("\"text\":\"").append(escapeJson(text)).append("\"");
        if (voice != null && !voice.isBlank()) {
            sb.append(",\"voice\":\"").append(escapeJson(voice)).append("\"");
        }
        sb.append("}");
        broadcast(sb.toString());
    }

    /**
     * Brackets {@code rawText} with {@code ${'id':'start'}$}/{@code ${'id':'stop'}$} markers (the
     * engine extracts these itself and echoes them via onMarker, which processStatusMessage turns
     * back into a "stop" feedback for {@code id}), sends it, and blocks the calling thread until
     * that feedback arrives (bounded by {@link #SPEECH_FEEDBACK_TIMEOUT_MS}) — or returns immediately
     * if there's no connected page to wait on. Callable from any thread: real playback calls this
     * from an ActivityWorker (already blocked on by ActivityScheduler's join-on-{@code
     * Type.blocking} contract); authoring-time preview turns call it directly from previewTurn(),
     * which WebUiServer dispatches to its own background executor rather than the Jetty request
     * thread.
     */
    private void broadcastSpeakAndAwaitStop(String vmuid, String rawText, String voice) {
        // Only bracket with a leading start marker — NOT a trailing stop marker. The engine
        // extracts/echoes a marker as soon as synthesis reaches that point in the text, which is
        // reliably 1+ second before vm.speakCommand()'s own onEnd callback actually fires (measured
        // 2026-07-16). Waiting on the marker instead of onEnd let this method's caller (a blocking
        // split-point action, or the next speech segment) issue a *new* speakCommand()/setEmotion()
        // call to the same engine instance while the previous call hadn't truly finished — which the
        // engine can't tolerate, causing the next call to silently fail ("speak ended with error").
        // vm-adapter.js's onEnd unconditionally sends "<vmuid>:stop" itself once the call is truly
        // done (success or failure), so waiting for that alone is both sufficient and correct.
        //
        // The space after the start marker is required, not cosmetic: every inline action marker
        // previewTurn() builds into rawText is followed by its own trailing space (see the
        // segment.append(markerKey).append(' ') call), so two adjacent markers are never glued
        // together — except this one junction, where rawText can start with another marker (e.g. a
        // turn opening with an untargeted [background ...] command). Without the space, the engine
        // fuses the two zero-width marker tokens and only reports the second, silently dropping the
        // very first inline action of a turn (confirmed 2026-07-20).
        final String cmd = "${'" + vmuid + "':'start'}$ " + rawText;
        mLogger.message("Utterance with CMD Markers: " + cmd);
        synchronized (mPendingSpeechIds) {
            if (mTransport == null || !mTransport.isConnected()) {
                broadcastSpeak(vmuid, cmd, voice);
                mLogger.warning("Speak command sent to nowhere (no connected page). Not waiting.");
                return;
            }
            mPendingSpeechIds.add(vmuid);
            broadcastSpeak(vmuid, cmd, voice);
            mLogger.message("Waiting for feedback on " + vmuid + " ...");
            final long deadline = System.currentTimeMillis() + SPEECH_FEEDBACK_TIMEOUT_MS;
            boolean timedOut = false;
            while (mPendingSpeechIds.contains(vmuid)) {
                final long remaining = deadline - System.currentTimeMillis();
                if (remaining <= 0) {
                    timedOut = true;
                    mPendingSpeechIds.remove(vmuid);
                    break;
                }
                try {
                    mPendingSpeechIds.wait(remaining);
                } catch (InterruptedException exc) {
                    mLogger.failure(exc.toString());
                    Thread.currentThread().interrupt();
                    mPendingSpeechIds.remove(vmuid);
                    timedOut = true;
                    break;
                }
            }
            if (timedOut) {
                mLogger.warning("Timed out after " + SPEECH_FEEDBACK_TIMEOUT_MS
                        + "ms waiting for feedback on " + vmuid + " - proceeding anyway.");
            } else {
                mLogger.message("Proceed - got feedback on " + vmuid + " ...");
            }
        }
    }

    private void broadcast(String msg) {
        if (mTransport != null) mTransport.send(msg);
    }

    private static String escapeJson(String s) {
        if (s == null) return "";
        return s.replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t");
    }

    // ------------------------------------------------------------------ feedback (page → VSM)

    /** Transport callback: a feedback string arrived from the character page. */
    @Override
    public synchronized void onMessage(String message) {
        mLogger.message("Processing character page message: >" + message + "<");
        // Engine lifecycle events (from the VuppetMaster constructor callbacks).
        if (message.startsWith("vm.")) {
            handleLifecycle(message);
            return;
        }
        // Status messages carry a ":" (id:value); time-mark messages are a bare marker name.
        if (message.contains(":")) {
            processStatusMessage(message);
        } else {
            processTimeMarkMessage(message);
        }
    }

    /** Transport callback: a character page attached. */
    @Override
    public void onConnected() {
        mLogger.message("Character page connected");
        if (mProject.hasVariable(mConnectedVar)) {
            mProject.setVariable(mConnectedVar, true);
        }
    }

    /** Transport callback: the character page detached. */
    @Override
    public void onDisconnected() {
        mLogger.message("Character page disconnected");
        synchronized (mPendingSpeechIds) {
            mPendingSpeechIds.clear();
            mPendingSpeechIds.notifyAll();
        }
    }

    /** Handles engine lifecycle feedback: {@code vm.ready} (model loaded + audio unlocked), etc. */
    private void handleLifecycle(String message) {
        if ("vm.ready".equals(message)) {
            mLogger.message("Character reported ready (model loaded).");
            if (mProject.hasVariable(mReadyVar)) {
                mProject.setVariable(mReadyVar, true);
            }
        }
        // vm.progress:<n> and vm.error are ignored here (surfaced in the browser console).
        // vm.heartbeat is a pure keep-alive (see vm-adapter.js) — receiving it is the whole point
        // (any traffic resets Jetty's WS idle timeout), nothing else to do with it.
    }

    private void processTimeMarkMessage(String message) {
        // vm-adapter.js's onMarker already strips the engine's quoting/bracketing and forwards the
        // bare id (e.g. "42"); reconstruct the exact literal produced by marker(long) above.
        message = message.replace("\"", "").replace("'", "");
        message = "${'" + message + "'}$";
        mLogger.message("Handling time marker >" + message + "<");

        // Preview-dispatched markers (previewTurn()) are resolved here directly, not via the
        // interpreter's ActivityScheduler — there is no ActivityWorker thread in preview mode.
        // Run on a fresh thread, not inline on this synchronized onMessage() call: the Runnable
        // ends in execute(), which now acquires mDispatchLock — if the *originating* previewTurn()
        // call is still holding that lock (waiting on this very character's own speech to finish),
        // running inline here would deadlock (this thread stuck on mDispatchLock while still
        // holding onMessage()'s monitor, blocking the "stop" feedback that previewTurn() is
        // waiting for). Real playback's own marker dispatch already avoids this the same way
        // (ActivityScheduler.handle() -> ActivityWorker.start(), a fresh thread).
        final Runnable previewAction = mPreviewMarkerMap.remove(message);
        if (previewAction != null) {
            new Thread(previewAction, "charamel-embed-preview-marker").start();
            return;
        }

        if (mProject.getRunTimePlayer() != null
                && mProject.getRunTimePlayer().getActivityScheduler().hasMarker(message)) {
            mProject.getRunTimePlayer().getActivityScheduler().handle(message);
        } else {
            mLogger.failure("Marker has already been processed: " + message);
        }
    }

    private void processStatusMessage(String message) {
        message = message.replace("{", "").replace("}", "").replace("'", "").replace("\"", "");
        String[] parts = message.split(":", 2);
        String header = parts[0];
        String content = parts.length > 1 ? parts[1] : "";
        mLogger.message("Status header >" + header + "<, content >" + content + "<");

        if (content.equalsIgnoreCase("stop")) {
            synchronized (mPendingSpeechIds) {
                if (mPendingSpeechIds.remove(header)) {
                    mPendingSpeechIds.notifyAll();
                    mLogger.message("Unblocked waiter for " + header);
                } else {
                    mLogger.warning("No waiter for " + header + " (already stopped?)");
                }
            }
        }
    }

    // ------------------------------------------------------------------ lifecycle

    @Override
    public void launch() {
        mLogger.message("Loading Charamel Embed (VuppetMaster JS-API) Executor ...");
        mConnectedVar = mConfig.getProperty("sceneflowVar", "");
        mReadyVar = mConfig.getProperty("characterReady", "");
        mSpeakingVar = mConfig.getProperty("characterSpeaking", "");
        mTurnVar = mConfig.getProperty("sceneflowTurnUtteranceVar", mTurnVar);

        // launch() runs again on every runtime re-launch (Stop → Start). The transport must
        // survive that: on desktop the Jetty character server + page/WebSocket stay up (avoids
        // re-binding an already-bound port); on Android the host's WebView bridge wiring stays
        // intact (avoids orphaning it and leaving the character unresponsive). Only create and
        // start a transport the first time; unload() tears it down and nulls it.
        if (mTransport != null) {
            mLogger.message("charamel-embed: reusing existing transport across re-launch.");
            return;
        }

        if (PlatformBootstrap.isAndroid()) {
            // Android: the host owns the WebView and drives this bridge (see getAndroidBridge()).
            mTransport = new AndroidBridgeTransport(this);
            mLogger.message("charamel-embed: using Android WebView bridge transport.");
        } else {
            // Desktop: embedded Jetty server hosts the page and speaks WebSocket. Loaded
            // reflectively so this executor holds NO compile-time reference to JettyTransport
            // (and therefore none to Jetty/Javalin/AWT) — the class is only resolved here, on
            // desktop, and never touched on Android where those libraries are absent.
            mTransport = createDesktopTransport();
        }

        if (mTransport != null) {
            try {
                mTransport.start();
            } catch (Exception e) {
                mLogger.failure("charamel-embed: transport failed to start: " + e.getMessage());
            }
        }
    }

    private CharamelTransport createDesktopTransport() {
        try {
            return (CharamelTransport) Class
                    .forName("de.dfki.vsm.xtension.charamelEmbed.JettyTransport")
                    .getConstructor(PluginConfig.class, CharamelTransport.Listener.class)
                    .newInstance(mConfig, this);
        } catch (ReflectiveOperationException e) {
            Throwable cause = (e.getCause() != null) ? e.getCause() : e;
            mLogger.failure("charamel-embed: desktop transport unavailable: " + cause.getMessage());
            return null;
        }
    }

    @Override
    public void unload() {
        if (mTransport != null) {
            mTransport.stop();
            mTransport = null;
        }
    }
}
