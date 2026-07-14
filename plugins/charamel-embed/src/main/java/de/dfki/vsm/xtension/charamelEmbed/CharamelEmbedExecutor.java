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
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
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

    // Activity workers waiting for speech-finished feedback, keyed by utterance id.
    private final Map<String, ActivityWorker> mActivityWorkerMap = new HashMap<>();
    // Pending actions for markers embedded in a previewTurn() call, keyed by marker string. Resolved
    // directly here rather than via the interpreter's ActivityScheduler, since preview dispatch runs
    // on the caller's thread (e.g. an HTTP handler), not an ActivityWorker.
    private final Map<String, Runnable> mPreviewMarkerMap = new ConcurrentHashMap<>();
    private final AtomicLong mPreviewMarkerId = new AtomicLong();
    protected final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

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
    public String getPreviewUrl() {
        return mTransport != null ? mTransport.getPreviewUrl() : null;
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
        parseAction(action.getName(), action.getFeatureList());
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

        final StringBuilder text = new StringBuilder();
        for (final SceneUttr uttr : turn.getUttrList()) {
            for (final UttrElement element : uttr.getWordList()) {
                if (element instanceof ActionObject) {
                    final String markerKey = marker(mPreviewMarkerId.incrementAndGet());
                    registerPreviewAction(markerKey, (ActionObject) element);
                    text.append(markerKey);
                } else {
                    text.append(element.getText(new HashMap<>()));
                }
                text.append(' ');
            }
            text.append(uttr.getPunctuationMark()).append(' ');
        }

        final String vmuid = "preview_" + mPreviewMarkerId.incrementAndGet();
        final String voice = mProject.getAgentConfig(turn.getSpeaker()) != null
                ? mProject.getAgentConfig(turn.getSpeaker()).getProperty("voice") : null;
        broadcastSpeak(vmuid, text.toString().trim(), voice);
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

            // Bracket the utterance with start/stop markers (same convention as charamel-ws). The
            // engine extracts ${...}$ markers itself and echoes them via onMarker(name,value); the
            // page adapter forwards them so processStatusMessage can unblock this worker.
            final String vmuid = actor + "_utterance_" + getVMUtteranceId();
            final String cmd = "${'" + vmuid + "':'start'}$" + sa.getText() + "${'" + vmuid + "':'stop'}$";
            mLogger.message("Utterance with CMD Markers: " + cmd);

            final String voice = mProject.getAgentConfig(actor) != null
                    ? mProject.getAgentConfig(actor).getProperty("voice") : null;

            activity.setType(AbstractActivity.Type.blocking);
            broadcastSpeak(vmuid, cmd, voice);
            mLogger.message("Speak command sent to character page ...");

            // Mirror the current turn text into the SceneFlow model (best-effort, as charamel-ws).
            if (mProject.hasVariable(mTurnVar)) {
                mProject.setVariable(mTurnVar, text);
            }
            if ((sa.getTurnNumber() == 1) && (sa.getUtteranceNumber() == 1)
                    && mProject.hasVariable(mSpeakingVar)) {
                mProject.setVariable(mSpeakingVar, new BooleanValue(true));
            }

            synchronized (mActivityWorkerMap) {
                if (mTransport != null && mTransport.isConnected()) {
                    ActivityWorker cAW = (ActivityWorker) Thread.currentThread();
                    mActivityWorkerMap.put(vmuid, cAW);

                    if (activity.getType() == AbstractActivity.Type.blocking) {
                        mLogger.message("ActivityWorker waiting for feedback on action " + vmuid + " ...");
                        while (mActivityWorkerMap.containsValue(cAW)) {
                            try {
                                mActivityWorkerMap.wait();
                            } catch (InterruptedException exc) {
                                mLogger.failure(exc.toString());
                            }
                        }
                        mLogger.message("ActivityWorker proceed - got feedback on " + vmuid + " ...");
                    }
                } else {
                    mLogger.warning("Speak command sent to nowhere (no connected page). Not waiting.");
                }
            }

            if ((sa.getTurnNumber() == sa.getTotalTurns()) && (sa.getUtteranceNumber() == sa.getTotalUtterances())
                    && mProject.hasVariable(mSpeakingVar)) {
                mProject.setVariable(mSpeakingVar, new BooleanValue(false));
            }
        } else {
            parseAction(activity.getName(), activity.getFeatures());
        }
    }

    /**
     * Non-speech actions, reachable from a SceneFlow PlayAction command ({@code [Xenia happy]}) or an
     * inline scene marker ({@code [Xenia background color='#1a2a6c']}). Each broadcasts a JSON envelope
     * that {@code vm-adapter.js} maps to a VuppetMaster JS call. Fire-and-forget (non-blocking).
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
        synchronized (mActivityWorkerMap) {
            mActivityWorkerMap.clear();
            mActivityWorkerMap.notifyAll();
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
        final Runnable previewAction = mPreviewMarkerMap.remove(message);
        if (previewAction != null) {
            previewAction.run();
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
            synchronized (mActivityWorkerMap) {
                if (mActivityWorkerMap.containsKey(header)) {
                    mActivityWorkerMap.remove(header);
                    mActivityWorkerMap.notifyAll();
                    mLogger.message("Unblocked activity worker for " + header);
                } else {
                    mLogger.warning("No waiting worker for " + header + " (already stopped?)");
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
