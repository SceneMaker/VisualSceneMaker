package de.dfki.vsm.xtension.asr;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.WebSocket;
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
    private int reconnectDelayMs;
    private String defaultGroups;
    private int defaultGroupsTimeout;

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
    private String turnEndProbVar;
    private String diarizationVar;

    // --- Lifecycle state ---
    private volatile boolean running = false;
    private volatile boolean variableWritesEnabled = false;

    // --- WebSocket state ---
    private volatile WebSocket wsClient;
    private final AtomicLong reconnectGen = new AtomicLong(0);
    private HttpClient httpClient;
    private ScheduledExecutorService scheduler;
    private ExecutorService messageExecutor;

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

        wsUrl                = configOrDefault("ws_url",                "ws://localhost:8765/stream");
        reconnectDelayMs     = parseIntOrDefault(configOrDefault("reconnect_delay_ms",   "2000"), 2000);
        defaultGroups        = configOrDefault("defaultGroups",        "");
        defaultGroupsTimeout = parseIntOrDefault(configOrDefault("defaultGroupsTimeout", "3600"), 3600);

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
        asrExpectedVar      = configOrDefault("asrExpectedVar",      "asr_expected");
        asrMatchedLabelVar  = configOrDefault("asrMatchedLabelVar",  "asr_matched_label");
        turnEndProbVar      = configOrDefault("turnEndProbVar",      "asr_turn_end_prob");
        diarizationVar      = configOrDefault("diarizationVar",      "asr_diarization");

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
        setBoolVar(asrExpectedVar,     false);
        // asrMatchedLabelVar is an Event(String) — do NOT initialize to "" at launch,
        // since writing to an Event variable enqueues (not replaces), and an empty
        // string event would cause spurious sceneflow edge triggers.
        setFloatVar(turnEndProbVar,    0.0f);
        setStringVar(diarizationVar,   "");

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

        connect();
        mLogger.message("[asr] launched, ws_url=" + wsUrl);
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
        if (messageExecutor != null) {
            messageExecutor.shutdownNow();
        }
        if (scheduler != null) {
            scheduler.shutdownNow();
        }
        mLogger.message("[asr] unloaded");
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
                    mLogger.message("[asr] server ready, models=" + msg.optString("models", ""));
                    if (!defaultGroups.isBlank()) {
                        sendExpect(defaultGroups, String.valueOf(defaultGroupsTimeout));
                    }
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

    private void sendText(final String text) {
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
        if (!variableWritesEnabled) return;
        if (varName == null || varName.isBlank()) return;
        try {
            mProject.setVariable(varName, value == null ? "" : value);
        } catch (Throwable ignore) {}
    }

    private void setBoolVar(final String varName, final boolean value) {
        if (!variableWritesEnabled) return;
        if (varName == null || varName.isBlank()) return;
        try {
            mProject.setVariable(varName, value);
        } catch (Throwable ignore) {}
    }

    private void setFloatVar(final String varName, final float value) {
        if (!variableWritesEnabled) return;
        if (varName == null || varName.isBlank()) return;
        try {
            mProject.setVariable(varName, value);
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
