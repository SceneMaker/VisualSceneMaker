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
        implements CharamelTransport.Listener, CharacterPreviewCapable {

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

    // Utterance ids whose "stop" feedback hasn't arrived yet. Guards a wait/notify — any thread
    // can wait here, not just an ActivityWorker: real playback waits on an ActivityWorker thread
    // (via ActivityScheduler's join-on-blocking contract), but authoring-time preview turns
    // (previewTurn()) run synchronously on the calling HTTP-handler thread and need the exact same
    // "block until this utterance's speech really finishes" wait.
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
                for (final UttrElement element : uttr.getWordList()) {
                    if (element instanceof ActionObject) {
                        final ActionObject action = (ActionObject) element;
                        if (ActionBlockingUtil.requiresUtteranceSplit(action)) {
                            if (segment.length() > 0) {
                                previewSpeakAndAwaitStop(segment.toString().trim(), voice);
                                segment = new StringBuilder();
                            }
                            runBlockingPreviewAction(action, turn.getSpeaker());
                        } else {
                            final String markerKey = marker(mPreviewMarkerId.incrementAndGet());
                            registerPreviewAction(markerKey, action);
                            segment.append(markerKey).append(' ');
                        }
                    } else {
                        segment.append(element.getText(new HashMap<>())).append(' ');
                    }
                }
                segment.append(uttr.getPunctuationMark());
                if (!segment.toString().isBlank()) {
                    previewSpeakAndAwaitStop(segment.toString().trim(), voice);
                }
            }
        }
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
     *  {@code [Bob smile]}) and records what to run when its marker fires back from the page. */
    private void registerPreviewAction(String markerKey, ActionObject action) {
        final String actionActor = action.getActor();
        final ActivityExecutor target = (actionActor == null || actionActor.isBlank())
                ? this
                : mProject.getAgentDevice(actionActor);
        if (target == null) {
            mPreviewMarkerMap.put(markerKey, () -> mLogger.warning(
                    "charamel-embed preview: no device for actor '" + actionActor + "', dropping ["
                            + action.getName() + "]"));
            return;
        }
        final ActionActivity activity = new ActionActivity(
                (actionActor == null || actionActor.isBlank()) ? "" : actionActor,
                action.getName(), action.getText(new HashMap<>()), action.getFeatureList(), new HashMap<>());
        mPreviewMarkerMap.put(markerKey, () -> target.execute(activity));
    }

    // ------------------------------------------------------------------ execution

    @Override
    public void execute(AbstractActivity activity) {
        final String actor = activity.getActor();

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
     * join-on-{@code Type.blocking} contract) for the estimated duration of a blocking emotion's
     * attack+hold+decay envelope, plus {@link #BLOCKING_TRANSPORT_BUFFER_MS}. Only "emotion" (and its
     * convenience named-emotion aliases, e.g. {@code [Xenia happy blocking='true']}) supports blocking
     * today — background/clearEmotion have no meaningful "duration" to wait out, so a blocking flag on
     * those is silently ignored here (nothing to sleep for).
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
        if (!isEmotionActionName(name)) {
            return;
        }
        long attack = parseMsOrDefault(getActionFeatureValue("attack", f), BLOCKING_DEFAULT_ATTACK_MS);
        long hold = parseMsOrDefault(getActionFeatureValue("hold", f), BLOCKING_DEFAULT_HOLD_MS);
        long decay = parseMsOrDefault(getActionFeatureValue("decay", f), BLOCKING_DEFAULT_DECAY_MS);
        long durationMs = attack + hold + decay + BLOCKING_TRANSPORT_BUFFER_MS;
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

    private static void appendNumber(StringBuilder sb, String key, String val) {
        if (val != null && !val.isBlank()) sb.append(",\"").append(key).append("\":").append(val.trim());
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
     * that feedback arrives — or returns immediately if there's no connected page to wait on.
     * Callable from any thread: real playback calls this from an ActivityWorker (already blocked on
     * by ActivityScheduler's join-on-{@code Type.blocking} contract); authoring-time preview turns
     * call it directly from the HTTP-handler thread that's synchronously running previewTurn().
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
        final String cmd = "${'" + vmuid + "':'start'}$" + rawText;
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
            while (mPendingSpeechIds.contains(vmuid)) {
                try {
                    mPendingSpeechIds.wait();
                } catch (InterruptedException exc) {
                    mLogger.failure(exc.toString());
                }
            }
            mLogger.message("Proceed - got feedback on " + vmuid + " ...");
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
