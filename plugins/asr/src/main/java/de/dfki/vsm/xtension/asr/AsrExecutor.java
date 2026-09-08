package de.dfki.vsm.xtension.asr;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.runtime.tls.TlsRuntimeContext;

import io.javalin.Javalin;
import io.javalin.websocket.WsCloseContext;
import io.javalin.websocket.WsContext;
import io.javalin.websocket.WsMessageContext;

import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.WebSocket;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.time.Duration;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

import org.json.JSONArray;
import org.json.JSONObject;

public class AsrExecutor extends ActivityExecutor {

    // --- Config ---
    private String wsUrl;
    private String restBaseUrl;
    private int reconnectDelayMs;
    private String defaultGroups;
    private int defaultGroupsTimeout;
    private String recordingDir;
    private String resolvedRecordingDir;

    // --- mode=listen config (VSM listens, fast-asr dials in — see doc/plugin-connectivity.md) ---
    private boolean listenMode;
    private int wsPort;
    private String asrKey;
    private String asrSlug;

    // --- Variable name bindings ---
    private String connectedVar;
    private String vadActiveVar;
    private String vadSpeechRatioVar;
    private String sadActiveVar;
    private String sadLevelDbVar;
    private String asrActiveVar;
    private String asrPartialVar;
    private String asrTextVar;
    private String asrLanguageVar;
    private String asrSpeakerVar;
    private String asrExpectedVar;
    private String asrMatchedLabelVar;
    private String asrConfigLanguageVar;
    private String turnEndProbVar;
    private String diarizationVar;
    private String recordingActiveVar;
    private String lastRecordingFileVar;

    // --- Lifecycle state ---
    private volatile boolean running = false;
    private volatile boolean variableWritesEnabled = false;

    // --- WebSocket state (mode=dial) ---
    private volatile WebSocket wsClient;
    private final AtomicLong reconnectGen = new AtomicLong(0);
    private HttpClient httpClient;
    private ScheduledExecutorService scheduler;
    private ExecutorService messageExecutor;

    // --- WebSocket state (mode=listen) ---
    private Javalin mHttpServer;
    // The single connected-and-authenticated fast-asr instance, if any. Replaced (old socket
    // closed) on a fresh successful auth, never rejected — same "reconnect replaces" design as
    // studymaster's wizard socket.
    private volatile WsContext mFastAsrSocket;

    public AsrExecutor(final PluginConfig config, final RunTimeProject project) {
        super(config, project);
    }

    @Override
    public String marker(final long id) {
        return "$(" + id + ")";
    }

    // -------------------------------------------------------------------------
    // Lifecycle
    // -------------------------------------------------------------------------

    @Override
    public void launch() {
        variableWritesEnabled = true;
        running = true;

        listenMode           = "listen".equalsIgnoreCase(configOrDefault("mode", "dial"));
        wsUrl                = configOrDefault("ws_url",                "ws://localhost:8765/stream");
        restBaseUrl          = restBaseUrlFrom(wsUrl);
        wsPort               = parseIntOrDefault(configOrDefault("ws_port", "9092"), 9092);
        asrKey               = configOrDefault("asr_key", "");
        asrSlug              = configOrDefault("asr_slug", "");
        reconnectDelayMs     = parseIntOrDefault(configOrDefault("reconnect_delay_ms",   "2000"), 2000);
        defaultGroups        = configOrDefault("defaultGroups",        "");
        defaultGroupsTimeout = parseIntOrDefault(configOrDefault("defaultGroupsTimeout", "3600"), 3600);
        recordingDir         = configOrDefault("recording_dir",        "./audio-recording");
        resolvedRecordingDir = Paths.get(mProject.getProjectPath()).resolve(recordingDir).normalize().toString();

        connectedVar      = configOrDefault("connectedVar",      "asr_connected");
        vadActiveVar      = configOrDefault("vadActiveVar",      "asr_vad_active");
        vadSpeechRatioVar = configOrDefault("vadSpeechRatioVar", "asr_vad_ratio");
        sadActiveVar      = configOrDefault("sadActiveVar",      "asr_sad_active");
        sadLevelDbVar     = configOrDefault("sadLevelDbVar",     "asr_sad_level_db");
        asrActiveVar      = configOrDefault("asrActiveVar",      "asr_active");
        asrPartialVar     = configOrDefault("asrPartialVar",     "asr_partial");
        asrTextVar        = configOrDefault("asrTextVar",        "asr_text");
        asrLanguageVar    = configOrDefault("asrLanguageVar",    "asr_language");
        asrSpeakerVar     = configOrDefault("asrSpeakerVar",     "asr_speaker");
        asrExpectedVar        = configOrDefault("asrExpectedVar",        "asr_expected");
        asrMatchedLabelVar    = configOrDefault("asrMatchedLabelVar",    "asr_matched_label");
        asrConfigLanguageVar  = configOrDefault("asrConfigLanguageVar",  "asr_config_language");
        turnEndProbVar        = configOrDefault("turnEndProbVar",        "asr_turn_end_prob");
        diarizationVar      = configOrDefault("diarizationVar",      "asr_diarization");
        recordingActiveVar   = configOrDefault("recordingActiveVar",   "asr_recording_active");
        lastRecordingFileVar = configOrDefault("lastRecordingFileVar", "asr_last_recording_file");

        // Safe defaults
        setBoolVar(connectedVar,       false);
        setBoolVar(vadActiveVar,       false);
        setFloatVar(vadSpeechRatioVar, 0.0f);
        setBoolVar(sadActiveVar,       false);
        setFloatVar(sadLevelDbVar,     -100.0f);
        setBoolVar(asrActiveVar,       false);
        setStringVar(asrPartialVar,    "");
        setStringVar(asrTextVar,       "");
        setStringVar(asrLanguageVar,   "");
        setStringVar(asrSpeakerVar,    "");
        setBoolVar(asrExpectedVar,          false);
        setStringVar(asrConfigLanguageVar,  "");
        // asrMatchedLabelVar is an Event(String) — do NOT initialize to "" at launch,
        // since writing to an Event variable enqueues (not replaces), and an empty
        // string event would cause spurious sceneflow edge triggers.
        setFloatVar(turnEndProbVar,    0.0f);
        setStringVar(diarizationVar,   "");
        setBoolVar(recordingActiveVar,       false);
        setStringVar(lastRecordingFileVar,   "");

        messageExecutor = Executors.newSingleThreadExecutor(r -> {
            Thread t = new Thread(r, "asr-message-handler");
            t.setDaemon(true);
            return t;
        });
        scheduler = Executors.newSingleThreadScheduledExecutor(r -> {
            Thread t = new Thread(r, "asr-scheduler");
            t.setDaemon(true);
            return t;
        });
        httpClient = HttpClient.newBuilder()
                .executor(Executors.newCachedThreadPool(r -> {
                    Thread t = new Thread(r, "asr-http");
                    t.setDaemon(true);
                    return t;
                }))
                .build();

        if (listenMode) {
            if (asrKey.isEmpty()) {
                mLogger.failure("[asr] mode=listen but no asr_key configured, refusing to start "
                        + "(would otherwise accept any fast-asr instance with no authentication).");
                return;
            }
            startListenServer();
            mLogger.message("[asr] launched in listen mode on port " + wsPort
                    + (asrSlug.isEmpty() ? "" : " (slug=" + asrSlug + ")"));
        } else {
            connect();
            mLogger.message("[asr] launched, ws_url=" + wsUrl);
        }
    }

    @Override
    public void unload() {
        running = false;
        variableWritesEnabled = false;
        reconnectGen.incrementAndGet();
        WebSocket ws = wsClient;
        wsClient = null;
        if (ws != null) {
            try { ws.abort(); } catch (Exception ignore) {}
        }
        if (mFastAsrSocket != null) {
            if (mFastAsrSocket.session.isOpen()) {
                mFastAsrSocket.session.close();
            }
            mFastAsrSocket = null;
        }
        if (mHttpServer != null) {
            mHttpServer.stop();
            mHttpServer = null;
        }
        if (messageExecutor != null) {
            messageExecutor.shutdownNow();
        }
        if (scheduler != null) {
            scheduler.shutdownNow();
        }
        mLogger.message("[asr] unloaded");
    }

    // -------------------------------------------------------------------------
    // mode=listen: embedded server fast-asr dials into (mirrors charamel-embed's
    // JettyTransport and studymaster's wizard socket — see doc/plugin-connectivity.md)
    // -------------------------------------------------------------------------

    private void startListenServer() {
        if (TlsRuntimeContext.isEnabled()) {
            mLogger.message("--secure: serving asr listen socket over wss on port " + wsPort);
            mHttpServer = Javalin.create(config -> {
                config.jetty.modifyWebSocketServletFactory(f -> f.setIdleTimeout(Duration.ofMinutes(10)));
                config.jetty.modifyServer(server -> server.setConnectors(
                        new org.eclipse.jetty.server.Connector[]{ sharedTlsConnector(server, wsPort) }));
            }).start();
        } else {
            mHttpServer = Javalin.create(config ->
                    config.jetty.modifyWebSocketServletFactory(f -> f.setIdleTimeout(Duration.ofMinutes(10)))
            ).start(wsPort);
        }
        mHttpServer.ws("/ws", ws -> {
            ws.onMessage(this::onListenMessage);
            ws.onClose(this::onListenClose);
            ws.onError(ctx -> mLogger.failure("[asr] listen-mode WS error: " + ctx.error()));
        });
    }

    private void onListenMessage(WsMessageContext ctx) {
        final JSONObject msg;
        try {
            msg = new JSONObject(ctx.message());
        } catch (Exception e) {
            mLogger.warning("[asr] listen mode: malformed frame ignored: " + e);
            return;
        }
        final String type = msg.optString("type", "");

        if ("auth".equals(type)) {
            handleListenAuth(ctx, msg);
            return;
        }

        final boolean authenticated = mFastAsrSocket != null && mFastAsrSocket.session.equals(ctx.session);
        if (!authenticated) {
            ctx.send(new JSONObject().put("type", "error").put("code", "not_authenticated")
                    .put("message", "Send 'auth' first").toString());
            return;
        }
        if ("ping".equals(type)) {
            // Pure keep-alive (see the setIdleTimeout override above) — any traffic resets
            // Jetty's idle timer, receiving it is the whole point.
            return;
        }
        // Every other message is the same wire protocol AsrWsListener already parses in dial
        // mode (ready/config/sad/vad/asr/turn_end/diarization/recording) — reuse handleMessage
        // unchanged rather than duplicating the protocol.
        final long gen = reconnectGen.get();
        final String raw = ctx.message();
        messageExecutor.execute(() -> handleMessage(gen, raw));
    }

    private void handleListenAuth(WsMessageContext ctx, JSONObject msg) {
        final String key = msg.optString("key", "");
        if (asrKey.isEmpty() || !asrKey.equals(key)) {
            ctx.send(new JSONObject().put("type", "authResult").put("ok", false)
                    .put("reason", "invalid_key").toString());
            ctx.session.close();
            return;
        }
        if (mFastAsrSocket != null && mFastAsrSocket.session.isOpen() && !mFastAsrSocket.session.equals(ctx.session)) {
            mLogger.message("[asr] new fast-asr connection replaces the existing one");
            mFastAsrSocket.session.close();
        }
        mFastAsrSocket = ctx;
        ctx.send(new JSONObject().put("type", "authResult").put("ok", true).toString());
        setBoolVar(connectedVar, true);
        sendSetRecordingDir();
        if (!defaultGroups.isBlank()) {
            sendExpect(defaultGroups, String.valueOf(defaultGroupsTimeout));
        }
    }

    private void onListenClose(WsCloseContext ctx) {
        if (mFastAsrSocket != null && mFastAsrSocket.session.equals(ctx.session)) {
            mFastAsrSocket = null;
            mLogger.message("[asr] listen mode: fast-asr disconnected");
            setBoolVar(connectedVar, false);
            setBoolVar(asrActiveVar, false);
        }
    }

    /** TLS connector on the given port using the shared mkcert host keystore (--secure mode).
     * Copied from JettyTransport/StudyMasterExecutor's identical helper — see TlsRuntimeContext's
     * javadoc for why every plugin embedding its own Jetty/Javalin server builds this the same way. */
    private static org.eclipse.jetty.server.ServerConnector sharedTlsConnector(
            org.eclipse.jetty.server.Server server, int port) {
        org.eclipse.jetty.util.ssl.SslContextFactory.Server factory =
                new org.eclipse.jetty.util.ssl.SslContextFactory.Server();
        factory.setKeyStorePath(TlsRuntimeContext.getKeyStorePath());
        factory.setKeyStorePassword(TlsRuntimeContext.getKeyStorePassword());
        factory.setKeyStoreType("PKCS12");
        org.eclipse.jetty.server.ServerConnector connector = new org.eclipse.jetty.server.ServerConnector(server,
                new org.eclipse.jetty.server.SslConnectionFactory(factory, "http/1.1"),
                new org.eclipse.jetty.server.HttpConnectionFactory());
        connector.setPort(port);
        return connector;
    }

    // -------------------------------------------------------------------------
    // Connection management
    // -------------------------------------------------------------------------

    private void connect() {
        final long gen = reconnectGen.get();
        if (!running) return;
        try {
            URI uri = URI.create(wsUrl);
            httpClient.newWebSocketBuilder()
                    .buildAsync(uri, new AsrWsListener(gen))
                    .whenComplete((ws, ex) -> {
                        if (ex != null) {
                            mLogger.warning("[asr] connect failed: " + ex.getMessage());
                            scheduleReconnect(gen);
                        }
                        // Success: onOpen() stores wsClient
                    });
        } catch (Exception ex) {
            mLogger.warning("[asr] connect error: " + ex.getMessage());
            scheduleReconnect(gen);
        }
    }

    private void scheduleReconnect(final long gen) {
        if (!running || gen != reconnectGen.get()) return;
        try {
            scheduler.schedule(() -> {
                if (running && gen == reconnectGen.get() && wsClient == null) {
                    mLogger.message("[asr] reconnecting to " + wsUrl + "...");
                    connect();
                }
            }, reconnectDelayMs, TimeUnit.MILLISECONDS);
        } catch (Exception ignore) {}
    }

    // -------------------------------------------------------------------------
    // Message handling
    // -------------------------------------------------------------------------

    private void handleMessage(final long gen, final String raw) {
        if (gen != reconnectGen.get() || !running) return;
        try {
            JSONObject msg = new JSONObject(raw);
            String type = msg.optString("type", "");
            switch (type) {
                case "ready":
                    setBoolVar(connectedVar, true);
                    setStringVar(asrConfigLanguageVar, msg.optString("language", ""));
                    mLogger.message("[asr] server ready, models=" + msg.optString("models", ""));
                    sendSetRecordingDir();
                    if (!defaultGroups.isBlank()) {
                        sendExpect(defaultGroups, String.valueOf(defaultGroupsTimeout));
                    }
                    break;
                case "config":
                    setStringVar(asrConfigLanguageVar, msg.optString("language", ""));
                    break;
                case "sad":
                    setBoolVar(sadActiveVar,  msg.optBoolean("active",   false));
                    setFloatVar(sadLevelDbVar, (float) msg.optDouble("level_db", -100.0));
                    break;
                case "vad":
                    setBoolVar(vadActiveVar,       msg.optBoolean("active",       false));
                    setFloatVar(vadSpeechRatioVar,  (float) msg.optDouble("speech_ratio", 0.0));
                    break;
                case "asr":
                    handleAsrMessage(msg);
                    break;
                case "turn_end":
                    setFloatVar(turnEndProbVar, (float) msg.optDouble("probability", 0.0));
                    break;
                case "diarization":
                    setStringVar(diarizationVar, raw);
                    break;
                case "recording":
                    setBoolVar(recordingActiveVar, msg.optBoolean("active", false));
                    final String recordedFile = msg.optString("file", "");
                    if (!recordedFile.isBlank()) {
                        setStringVar(lastRecordingFileVar,
                                Paths.get(resolvedRecordingDir, recordedFile).toString());
                    }
                    break;
                default:
                    break;
            }
        } catch (Exception ex) {
            mLogger.warning("[asr] parse error: " + ex.getMessage() + " raw=" + raw);
        }
    }

    private void handleAsrMessage(final JSONObject msg) {
        String event = msg.optString("event", "");

        if ("start".equals(event)) {
            setBoolVar(asrActiveVar,    true);
            setStringVar(asrPartialVar, "");
            setBoolVar(asrExpectedVar,  false);
            // asrMatchedLabelVar intentionally NOT reset here — it persists until
            // the next committed final so the sceneflow can read it after the
            // new utterance start resets asrExpectedVar.
            return;
        }

        if ("cancel".equals(event) || "error".equals(event)) {
            setBoolVar(asrActiveVar,    false);
            setStringVar(asrPartialVar, "");
            // asrMatchedLabelVar preserved — no new utterance was committed.
            return;
        }

        boolean isFinal     = msg.optBoolean("is_final",    false);
        boolean provisional = msg.optBoolean("provisional", false);
        boolean isExpected  = msg.optBoolean("expected",    false);
        String accumulated  = msg.optString("accumulated", msg.optString("text", ""));

        if (!isFinal) {
            // Streaming partial token
            setStringVar(asrPartialVar, accumulated);
            return;
        }

        if (provisional && !isExpected) {
            // Live preview: show but don't close utterance.
            // Exception: if this provisional token matched an expected word,
            // we commit it immediately so the sceneflow receives the match.
            setStringVar(asrPartialVar, accumulated);
            return;
        }

        // Non-provisional final (or expected provisional): commit utterance
        setBoolVar(asrActiveVar,     false);
        setStringVar(asrPartialVar,  "");
        setStringVar(asrTextVar,     accumulated);
        setStringVar(asrLanguageVar, msg.optString("language", ""));
        setStringVar(asrSpeakerVar,  msg.optString("speaker",  ""));
        setBoolVar(asrExpectedVar,   isExpected);
        // asrMatchedLabelVar is Event(String): only enqueue when there is an actual match.
        // The sceneflow guard  asr_matched_label == "CONFIRM"  peeks, compares and dequeues
        // only when the front element matches, so multiple groups work correctly without
        // consuming the wrong event. Writing on every final (including non-matches) would
        // enqueue spurious empty-string events.
        if (isExpected) {
            setStringVar(asrMatchedLabelVar, msg.optString("expected_label", ""));
        }
    }

    // -------------------------------------------------------------------------
    // execute() — sceneflow actions
    // -------------------------------------------------------------------------

    @Override
    public void execute(final AbstractActivity activity) {
        final String name = activity.getName() == null ? "" : activity.getName().toLowerCase().trim();
        switch (name) {
            case "expect":
                sendExpect(
                        getActionFeatureValue("words",   activity.getFeatures()),
                        getActionFeatureValue("timeout", activity.getFeatures())
                );
                break;
            case "clear_expect":
                sendClearExpect();
                break;
            case "set_language":
                sendSetLanguage(getActionFeatureValue("language", activity.getFeatures()));
                break;
            case "mute":
                sendMute();
                break;
            case "unmute":
                sendUnmute();
                break;
            case "start_recording":
                sendStartRecording();
                break;
            case "stop_recording":
                sendStopRecording();
                break;
            case "get_last_recorded_audio":
                sendGetLastRecordedAudio();
                break;
            default:
                mLogger.warning("[asr] unknown action: " + name);
                break;
        }
    }

    // -------------------------------------------------------------------------
    // Outbound actions
    // -------------------------------------------------------------------------

    /**
     * Send an expect action to the server.
     *
     * <p>The {@code words} param supports two formats:
     * <ul>
     *   <li><b>Flat</b>: {@code "ja, nein, doch"} — words sent as a plain list</li>
     *   <li><b>Grouped</b>: {@code "CONFIRM:ja,schon,doch | DISCONFIRM:nein,nee"} —
     *       pipe-separated groups, each with an optional LABEL prefix.
     *       Sends a {@code groups} JSON array; the server returns
     *       {@code expected_label} on match, which is written to
     *       {@code asrMatchedLabelVar}.</li>
     * </ul>
     */
    private void sendExpect(final String words, final String timeout) {
        if (words == null || words.isBlank()) {
            mLogger.warning("[asr] expect: 'words' param is required");
            return;
        }
        try {
            JSONObject msg = new JSONObject();
            msg.put("action",  "expect");
            msg.put("timeout", parseIntOrDefault(stripQuotes(timeout), 30));

            final String w = stripQuotes(words);
            if (w.contains("|")) {
                // Grouped format: "CONFIRM:ja,schon | DISCONFIRM:nein,nee"
                JSONArray groups = new JSONArray();
                for (String part : w.split("\\|")) {
                    part = part.trim();
                    if (part.isEmpty()) continue;
                    JSONObject group = new JSONObject();
                    int colon = part.indexOf(':');
                    if (colon > 0) {
                        group.put("label", part.substring(0, colon).trim());
                        group.put("words", splitWords(part.substring(colon + 1)));
                    } else {
                        // No colon: bare label with no words → catch-all group (e.g. "OTHER").
                        // The server fires this label when the expectation is active but no
                        // specific word matched and the utterance passed all filters.
                        group.put("label", part.trim());
                        group.put("words", new JSONArray());
                    }
                    groups.put(group);
                }
                msg.put("groups", groups);
            } else {
                // Flat format: "ja, nein, doch"
                msg.put("words", splitWords(w));
            }
            sendText(msg.toString());
        } catch (Exception ex) {
            mLogger.warning("[asr] sendExpect error: " + ex.getMessage());
        }
    }

    /** Remove a matching pair of outer single or double quotes that VSM may include in feature values. */
    private static String stripQuotes(final String raw) {
        if (raw == null) return "";
        String s = raw.trim();
        if (s.length() >= 2) {
            char first = s.charAt(0), last = s.charAt(s.length() - 1);
            if ((first == '\'' && last == '\'') || (first == '"' && last == '"')) {
                s = s.substring(1, s.length() - 1);
            }
        }
        return s;
    }

    private JSONArray splitWords(final String raw) {
        JSONArray arr = new JSONArray();
        for (String w : raw.split(",")) {
            String trimmed = w.trim();
            if (!trimmed.isEmpty()) arr.put(trimmed);
        }
        return arr;
    }

    private void sendMute() {
        try {
            JSONObject msg = new JSONObject();
            msg.put("action", "mute");
            sendText(msg.toString());
        } catch (Exception ex) {
            mLogger.warning("[asr] sendMute error: " + ex.getMessage());
        }
    }

    private void sendUnmute() {
        try {
            JSONObject msg = new JSONObject();
            msg.put("action", "unmute");
            sendText(msg.toString());
        } catch (Exception ex) {
            mLogger.warning("[asr] sendUnmute error: " + ex.getMessage());
        }
    }

    private void sendClearExpect() {
        try {
            JSONObject msg = new JSONObject();
            msg.put("action", "clear_expect");
            sendText(msg.toString());
        } catch (Exception ex) {
            mLogger.warning("[asr] sendClearExpect error: " + ex.getMessage());
        }
    }

    private void sendSetLanguage(final String language) {
        if (language == null || stripQuotes(language).isBlank()) {
            mLogger.warning("[asr] set_language: 'language' param is required");
            return;
        }
        try {
            JSONObject msg = new JSONObject();
            msg.put("action",   "set_language");
            msg.put("language", stripQuotes(language));
            sendText(msg.toString());
        } catch (Exception ex) {
            mLogger.warning("[asr] sendSetLanguage error: " + ex.getMessage());
        }
    }

    // -------------------------------------------------------------------------
    // Recording (fast-asr REST endpoints, alongside the WebSocket stream)
    // -------------------------------------------------------------------------

    /** Derive the fast-asr server's HTTP base URL (scheme+authority) from its WebSocket URL. */
    private static String restBaseUrlFrom(final String wsUrl) {
        URI uri = URI.create(wsUrl);
        String scheme = "wss".equalsIgnoreCase(uri.getScheme()) ? "https" : "http";
        return scheme + "://" + uri.getAuthority();
    }

    /** Tell the fast-asr server where to save recordings. Sent on every connect/reconnect.
     * No-op in listen mode: there is no known REST base URL for a fast-asr instance that dialed
     * in to us (recording control over a reverse connection is out of scope for now). */
    private void sendSetRecordingDir() {
        if (listenMode) {
            return;
        }
        try {
            String encodedDir = URLEncoder.encode(resolvedRecordingDir, StandardCharsets.UTF_8);
            HttpRequest req = HttpRequest.newBuilder(
                            URI.create(restBaseUrl + "/set_recording_dir?directory=" + encodedDir))
                    .timeout(Duration.ofSeconds(5))
                    .POST(HttpRequest.BodyPublishers.noBody())
                    .build();
            httpClient.sendAsync(req, HttpResponse.BodyHandlers.ofString())
                    .thenAccept(resp -> {
                        if (resp.statusCode() != 200) {
                            mLogger.warning("[asr] set_recording_dir failed: status=" + resp.statusCode()
                                    + " body=" + resp.body());
                        }
                    })
                    .exceptionally(ex -> {
                        mLogger.warning("[asr] set_recording_dir error: " + ex.getMessage());
                        return null;
                    });
        } catch (Exception ex) {
            mLogger.warning("[asr] set_recording_dir error: " + ex.getMessage());
        }
    }

    private void sendStartRecording() {
        if (listenMode) {
            mLogger.warning("[asr] start_recording is not supported in listen mode");
            return;
        }
        try {
            HttpRequest req = HttpRequest.newBuilder(URI.create(restBaseUrl + "/start_recording"))
                    .timeout(Duration.ofSeconds(5))
                    .POST(HttpRequest.BodyPublishers.noBody())
                    .build();
            httpClient.sendAsync(req, HttpResponse.BodyHandlers.ofString())
                    .thenAccept(resp -> {
                        if (resp.statusCode() == 200) {
                            setBoolVar(recordingActiveVar, true);
                        } else {
                            mLogger.warning("[asr] start_recording failed: status=" + resp.statusCode()
                                    + " body=" + resp.body());
                        }
                    })
                    .exceptionally(ex -> {
                        mLogger.warning("[asr] start_recording error: " + ex.getMessage());
                        return null;
                    });
        } catch (Exception ex) {
            mLogger.warning("[asr] start_recording error: " + ex.getMessage());
        }
    }

    private void sendStopRecording() {
        if (listenMode) {
            mLogger.warning("[asr] stop_recording is not supported in listen mode");
            return;
        }
        try {
            HttpRequest req = HttpRequest.newBuilder(URI.create(restBaseUrl + "/stop_recording"))
                    .timeout(Duration.ofSeconds(10))
                    .POST(HttpRequest.BodyPublishers.noBody())
                    .build();
            httpClient.sendAsync(req, HttpResponse.BodyHandlers.ofString())
                    .thenAccept(resp -> {
                        setBoolVar(recordingActiveVar, false);
                        if (resp.statusCode() == 200) {
                            try {
                                JSONObject obj = new JSONObject(resp.body());
                                setStringVar(lastRecordingFileVar, obj.optString("file", ""));
                            } catch (Exception ex) {
                                mLogger.warning("[asr] stop_recording parse error: " + ex.getMessage());
                            }
                        } else {
                            mLogger.warning("[asr] stop_recording failed: status=" + resp.statusCode()
                                    + " body=" + resp.body());
                        }
                    })
                    .exceptionally(ex -> {
                        mLogger.warning("[asr] stop_recording error: " + ex.getMessage());
                        return null;
                    });
        } catch (Exception ex) {
            mLogger.warning("[asr] stop_recording error: " + ex.getMessage());
        }
    }

    private void sendGetLastRecordedAudio() {
        if (listenMode) {
            mLogger.warning("[asr] get_last_recorded_audio is not supported in listen mode");
            return;
        }
        try {
            HttpRequest req = HttpRequest.newBuilder(URI.create(restBaseUrl + "/get_last_recorded_audio"))
                    .timeout(Duration.ofSeconds(5))
                    .GET()
                    .build();
            httpClient.sendAsync(req, HttpResponse.BodyHandlers.ofString())
                    .thenAccept(resp -> {
                        if (resp.statusCode() == 200) {
                            try {
                                JSONObject obj = new JSONObject(resp.body());
                                setStringVar(lastRecordingFileVar, obj.optString("path", ""));
                            } catch (Exception ex) {
                                mLogger.warning("[asr] get_last_recorded_audio parse error: " + ex.getMessage());
                            }
                        } else if (resp.statusCode() == 404) {
                            mLogger.message("[asr] get_last_recorded_audio: no recording saved yet");
                        } else {
                            mLogger.warning("[asr] get_last_recorded_audio failed: status=" + resp.statusCode()
                                    + " body=" + resp.body());
                        }
                    })
                    .exceptionally(ex -> {
                        mLogger.warning("[asr] get_last_recorded_audio error: " + ex.getMessage());
                        return null;
                    });
        } catch (Exception ex) {
            mLogger.warning("[asr] get_last_recorded_audio error: " + ex.getMessage());
        }
    }

    private void sendText(final String text) {
        if (listenMode) {
            WsContext ctx = mFastAsrSocket;
            if (ctx == null || !ctx.session.isOpen()) {
                mLogger.warning("[asr] sendText: no fast-asr instance connected, message dropped");
                return;
            }
            try {
                ctx.send(text);
            } catch (Exception ex) {
                mLogger.warning("[asr] sendText error: " + ex.getMessage());
            }
            return;
        }
        WebSocket ws = wsClient;
        if (ws == null) {
            mLogger.warning("[asr] sendText: not connected, message dropped");
            return;
        }
        try {
            ws.sendText(text, true);
        } catch (Exception ex) {
            mLogger.warning("[asr] sendText error: " + ex.getMessage());
        }
    }

    // -------------------------------------------------------------------------
    // Variable helpers
    // -------------------------------------------------------------------------

    private void setStringVar(final String varName, final String value) {
        setStringVar(varName, value, 20);
    }

    private void setStringVar(final String varName, final String value, final int retriesLeft) {
        if (!variableWritesEnabled) return;
        if (varName == null || varName.isBlank()) return;
        final String v = value == null ? "" : value;
        try {
            if (!mProject.setVariable(varName, v) && running && retriesLeft > 0 && scheduler != null) {
                scheduler.schedule(() -> setStringVar(varName, v, retriesLeft - 1),
                        250, TimeUnit.MILLISECONDS);
            }
        } catch (Throwable ignore) {}
    }

    private void setBoolVar(final String varName, final boolean value) {
        setBoolVar(varName, value, 20);
    }

    private void setBoolVar(final String varName, final boolean value, final int retriesLeft) {
        if (!variableWritesEnabled) return;
        if (varName == null || varName.isBlank()) return;
        try {
            if (!mProject.setVariable(varName, value) && running && retriesLeft > 0 && scheduler != null) {
                scheduler.schedule(() -> setBoolVar(varName, value, retriesLeft - 1),
                        250, TimeUnit.MILLISECONDS);
            }
        } catch (Throwable ignore) {}
    }

    private void setFloatVar(final String varName, final float value) {
        setFloatVar(varName, value, 20);
    }

    private void setFloatVar(final String varName, final float value, final int retriesLeft) {
        if (!variableWritesEnabled) return;
        if (varName == null || varName.isBlank()) return;
        try {
            if (!mProject.setVariable(varName, value) && running && retriesLeft > 0 && scheduler != null) {
                scheduler.schedule(() -> setFloatVar(varName, value, retriesLeft - 1),
                        250, TimeUnit.MILLISECONDS);
            }
        } catch (Throwable ignore) {}
    }

    // -------------------------------------------------------------------------
    // Config helpers
    // -------------------------------------------------------------------------

    private String configOrDefault(final String key, final String fallback) {
        String value = mConfig.getProperty(key);
        if (value == null || value.isBlank()) return fallback;
        return value;
    }

    private int parseIntOrDefault(final String value, final int fallback) {
        if (value == null || value.isBlank()) return fallback;
        try {
            return Integer.parseInt(value.trim());
        } catch (Exception ignore) {
            return fallback;
        }
    }

    // -------------------------------------------------------------------------
    // Inner class: WebSocket listener
    // -------------------------------------------------------------------------

    private class AsrWsListener implements WebSocket.Listener {

        private final long gen;
        private final StringBuilder buf = new StringBuilder();

        AsrWsListener(final long gen) {
            this.gen = gen;
        }

        @Override
        public void onOpen(final WebSocket ws) {
            wsClient = ws;
            setBoolVar(connectedVar, true);
            mLogger.message("[asr] connected to " + wsUrl);
            ws.request(1);
        }

        @Override
        public CompletionStage<?> onText(final WebSocket ws, final CharSequence data, final boolean last) {
            buf.append(data);
            if (last) {
                final String text = buf.toString();
                buf.setLength(0);
                messageExecutor.execute(() -> handleMessage(gen, text));
            }
            ws.request(1);
            return null;
        }

        @Override
        public CompletionStage<?> onClose(final WebSocket ws, final int statusCode, final String reason) {
            wsClient = null;
            mLogger.message("[asr] disconnected status=" + statusCode + " reason=" + reason);
            messageExecutor.execute(() -> {
                setBoolVar(connectedVar, false);
                setBoolVar(asrActiveVar, false);
            });
            scheduleReconnect(gen);
            return null;
        }

        @Override
        public void onError(final WebSocket ws, final Throwable error) {
            wsClient = null;
            mLogger.warning("[asr] WebSocket error: " + error.getMessage());
            messageExecutor.execute(() -> {
                setBoolVar(connectedVar, false);
                setBoolVar(asrActiveVar, false);
            });
            scheduleReconnect(gen);
        }
    }
}
