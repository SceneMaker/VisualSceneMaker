package de.dfki.vsm.xtension.charamelEmbed;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.interpreter.value.BooleanValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import io.javalin.Javalin;
import io.javalin.websocket.WsCloseContext;
import io.javalin.websocket.WsConnectContext;
import io.javalin.websocket.WsContext;
import io.javalin.websocket.WsMessageContext;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.Set;

/**
 * Drives a self-hosted VuppetMaster character page through the engine's JavaScript API
 * (vm.speak(...)), as opposed to {@code charamel-ws} which drives a Charamel-hosted page via the
 * timeline-JSON protocol.
 *
 * VSM → page:  a small JSON command envelope over WebSocket, e.g. {@code {"cmd":"speak", ...}}.
 * page → VSM:  marker strings reconstructed from the engine's onMarker(name,value) callback, in the
 *              exact form this executor's {@link #handleMessage} parses (identical to charamel-ws).
 *
 * The character page ({@code /renderer/character.html} + {@code /renderer/vm-adapter.js}) is served
 * from the plugin JAR classpath and is transport-agnostic: the same page is reused on Android
 * (WebView + JS bridge) in a later phase.
 *
 * @author Patrick Gebhard
 */
public class CharamelEmbedExecutor extends ActivityExecutor {

    static long sUtteranceId = 0;

    // Activity workers waiting for speech-finished feedback, keyed by utterance id.
    private final Map<String, ActivityWorker> mActivityWorkerMap = new HashMap<>();
    protected final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final Set<WsContext> mWebSockets = ConcurrentHashMap.newKeySet();

    private Javalin mApp;
    private Process mBrowserProcess = null;

    // --- Config (read in launch()) ---
    private int mPort;
    private String mLicenseKey = "";
    private String mAppName = "";
    private String mEngineUrl = "https://engine.vuppetmaster.com/api/engine/vuppetmaster.iife.js";
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
                if (!mWebSockets.isEmpty()) {
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
                if (mApp != null) mApp.stop();
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

    // ------------------------------------------------------------------ feedback (page → VSM)

    private synchronized void handleMessage(WsMessageContext ctx) {
        String message = ctx.message();
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
        message = message.replace("\"", "").replace("'", "");
        message = "$" + message + "$"; // bracketing "$" are not sent back from the page
        mLogger.message("Handling time marker >" + message + "<");
        if (mProject.getRunTimePlayer().getActivityScheduler().hasMarker(message)) {
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

    // ------------------------------------------------------------------ server lifecycle

    @Override
    public void launch() {
        mLogger.message("Loading Charamel Embed (VuppetMaster JS-API) Executor ...");
        mPort = Integer.parseInt(Objects.requireNonNull(mConfig.getProperty("port")));
        mLicenseKey = mConfig.getProperty("licenseKey", "");
        mAppName = mConfig.getProperty("appName", "");
        mEngineUrl = mConfig.getProperty("engineUrl", mEngineUrl);
        mConnectedVar = mConfig.getProperty("sceneflowVar", "");
        mReadyVar = mConfig.getProperty("characterReady", "");
        mSpeakingVar = mConfig.getProperty("characterSpeaking", "");
        mTurnVar = mConfig.getProperty("sceneflowTurnUtteranceVar", mTurnVar);

        mApp = Javalin.create(config -> {
            config.jetty.modifyWebSocketServletFactory(f -> f.setIdleTimeout(java.time.Duration.ofMinutes(10)));
            config.jetty.modifyServletContextHandler(handler ->
                handler.addFilter(new org.eclipse.jetty.servlet.FilterHolder(new PnaFilter()), "/*",
                    java.util.EnumSet.of(jakarta.servlet.DispatcherType.REQUEST)));
        }).start(mPort);

        mApp.get("/", ctx -> ctx.redirect("/character.html"));
        mApp.get("/character.html", ctx -> serveResource(ctx, "/renderer/character.html", "text/html"));
        mApp.get("/vm-adapter.js", ctx -> serveResource(ctx, "/renderer/vm-adapter.js", "application/javascript"));

        // Injects license/appName/engine URL into the page without editing the HTML.
        mApp.get("/vsm-config.js", ctx -> ctx.contentType("application/javascript").result(
            "window.VSM_CONFIG=" +
                "{\"licenseKey\":\"" + escapeJson(mLicenseKey) + "\"," +
                 "\"appName\":\"" + escapeJson(mAppName) + "\"," +
                 "\"engineUrl\":\"" + escapeJson(mEngineUrl) + "\"};"));

        mApp.ws("/ws", ws -> {
            ws.onConnect(ctx -> {
                mWebSockets.add(ctx);
                mLogger.message("Character page connected");
                if (mProject.hasVariable(mConnectedVar)) {
                    mProject.setVariable(mConnectedVar, true);
                }
            });
            ws.onMessage(this::handleMessage);
            ws.onClose(this::onWsClose);
            ws.onError(ctx -> {
                Throwable t = ctx.error();
                mLogger.failure("WebSocket error: " + (t != null ? t.getMessage() : "unknown"));
            });
        });

        if ("true".equalsIgnoreCase(mConfig.getProperty("autostart_browser"))) {
            launchBrowser("http://127.0.0.1:" + mPort + "/character.html");
        }
    }

    private void onWsClose(WsCloseContext ctx) {
        mWebSockets.remove(ctx);
        mLogger.message("Character page disconnected");
        synchronized (mActivityWorkerMap) {
            mActivityWorkerMap.clear();
            mActivityWorkerMap.notifyAll();
        }
    }

    private void serveResource(io.javalin.http.Context ctx, String path, String contentType) {
        InputStream stream = getClass().getResourceAsStream(path);
        if (stream != null) ctx.result(stream).contentType(contentType);
        else ctx.status(404);
    }

    private synchronized void broadcast(String msg) {
        for (WsContext ws : mWebSockets) {
            ws.send(msg);
        }
    }

    @Override
    public void unload() {
        mWebSockets.clear();
        if (mApp != null) mApp.stop();
        if (mBrowserProcess != null) {
            mBrowserProcess.destroy();
            try {
                if (!mBrowserProcess.waitFor(2, java.util.concurrent.TimeUnit.SECONDS)) {
                    mBrowserProcess.destroyForcibly();
                }
            } catch (InterruptedException e) {
                mBrowserProcess.destroyForcibly();
            }
            mBrowserProcess = null;
        }
    }

    // ------------------------------------------------------------------ helpers

    /** Opens the character page in a browser. OS default unless a "browser" path is configured. */
    private void launchBrowser(String url) {
        String browserPref = mConfig.getProperty("browser", "");
        boolean fullscreen = "true".equalsIgnoreCase(mConfig.getProperty("browser_fullscreen"));
        if ((browserPref == null || browserPref.isBlank() || "default".equalsIgnoreCase(browserPref)) && !fullscreen) {
            try {
                if (java.awt.Desktop.isDesktopSupported()
                        && java.awt.Desktop.getDesktop().isSupported(java.awt.Desktop.Action.BROWSE)) {
                    java.awt.Desktop.getDesktop().browse(new java.net.URI(url));
                    mLogger.message("Opened default browser: " + url);
                    return;
                }
            } catch (Exception e) {
                mLogger.warning("Default browser open failed: " + e.getMessage());
            }
        }
        String chrome = (browserPref != null && !browserPref.isBlank()
                && !"default".equalsIgnoreCase(browserPref) && !"chrome".equalsIgnoreCase(browserPref))
                ? browserPref : findChrome();
        if (chrome == null) {
            mLogger.warning("Chrome not found; cannot auto-start browser.");
            return;
        }
        List<String> command = new ArrayList<>();
        command.add(chrome);
        if (fullscreen) command.add("--start-fullscreen");
        command.add("--new-window");
        command.add(url);
        try {
            ProcessBuilder pb = new ProcessBuilder(command);
            pb.inheritIO();
            mBrowserProcess = pb.start();
            mLogger.message("Launching browser: " + String.join(" ", command));
        } catch (IOException e) {
            mLogger.failure("Failed to launch browser: " + e.getMessage());
        }
    }

    private String findChrome() {
        String os = System.getProperty("os.name", "").toLowerCase();
        if (os.contains("mac")) {
            String[] candidates = {
                "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
                "/Applications/Chromium.app/Contents/MacOS/Chromium"
            };
            for (String p : candidates) if (new File(p).exists()) return p;
        } else if (os.contains("win")) {
            String[] candidates = {
                System.getenv("ProgramFiles") + "\\Google\\Chrome\\Application\\chrome.exe",
                System.getenv("ProgramFiles(x86)") + "\\Google\\Chrome\\Application\\chrome.exe"
            };
            for (String p : candidates) if (p != null && new File(p).exists()) return p;
        } else if (os.contains("linux")) {
            for (String c : new String[]{"google-chrome", "google-chrome-stable", "chromium-browser", "chromium"}) {
                try {
                    Process p = Runtime.getRuntime().exec(new String[]{"which", c});
                    if (p.waitFor() == 0) return c;
                } catch (Exception ignored) {}
            }
        }
        return null;
    }

    private static String escapeJson(String s) {
        if (s == null) return "";
        return s.replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t");
    }

    // Adds PNA/CORS headers so a page loading the engine from a public origin can reach this
    // localhost WebSocket under Chrome's Private Network Access rules (same approach as charamel-ws).
    private class PnaFilter implements jakarta.servlet.Filter {
        @Override
        public void doFilter(jakarta.servlet.ServletRequest req, jakarta.servlet.ServletResponse res,
                             jakarta.servlet.FilterChain chain)
                throws IOException, jakarta.servlet.ServletException {
            jakarta.servlet.http.HttpServletRequest httpReq = (jakarta.servlet.http.HttpServletRequest) req;
            jakarta.servlet.http.HttpServletResponse httpRes = (jakarta.servlet.http.HttpServletResponse) res;
            httpRes.setHeader("Access-Control-Allow-Origin", "*");
            httpRes.setHeader("Access-Control-Allow-Private-Network", "true");
            if ("OPTIONS".equalsIgnoreCase(httpReq.getMethod())) {
                httpRes.setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
                httpRes.setHeader("Access-Control-Allow-Headers",
                    "Upgrade, Connection, Sec-WebSocket-Key, Sec-WebSocket-Version, Sec-WebSocket-Protocol");
                httpRes.setStatus(jakarta.servlet.http.HttpServletResponse.SC_OK);
                return;
            }
            chain.doFilter(req, res);
        }
        @Override public void init(jakarta.servlet.FilterConfig fc) {}
        @Override public void destroy() {}
    }
}
