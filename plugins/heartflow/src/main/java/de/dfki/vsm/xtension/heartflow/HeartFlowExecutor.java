package de.dfki.vsm.xtension.heartflow;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.json.JSONObject;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.WebSocket;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Heart Flow VSM plugin.
 *
 * Connects to the Heart Flow WebSocket server (ws://localhost:7878) and maps
 * incoming biosignal messages to SceneFlow variables:
 *
 *   hf_bpm              Float   — heart rate in BPM
 *   hf_rmssd            Float   — RMSSD in ms
 *   hf_sdnn             Float   — SDNN in ms
 *   hf_pnn50            Float   — pNN50 in %
 *   hf_sd1              Float   — SD1 in ms
 *   hf_sd2              Float   — SD2 in ms
 *   hf_median_rr        Float   — median RR in ms
 *   hf_motion           String  — Still / Slight / Strong
 *   hf_motion_score     Float   — normalised motion score 0–1
 *   hf_breathing_phase  String  — inhale / exhale
 *   hf_breathing_rate   Float   — breaths per minute
 *   hf_heartbeat        Event(String) — pre-emptive beat event ("beat")
 *   hf_battery          Float   — battery level in %
 *   hf_connected        Bool    — true while streaming
 *
 * Heartbeat synchronisation:
 *   The Python backend sends a beat_epoch_ms absolute timestamp (ms since Unix
 *   epoch) predicting when the NEXT R-peak will occur.  Both Python
 *   (time.time_ns()//1_000_000) and Java (System.currentTimeMillis()) use the
 *   same OS wall clock on localhost, so no clock sync is needed.  The plugin
 *   schedules the hf_heartbeat event for exactly (beat_epoch_ms − now) ms,
 *   plus an optional beat_offset_ms tuning parameter.
 *
 * Auto-connect mode (auto_connect = true):
 *   On launch the plugin sends {"action":"auto_connect"} to Heart Flow, which
 *   scans for the first available Polar H10 and connects automatically —
 *   no browser or manual interaction required.
 */
public class HeartFlowExecutor extends ActivityExecutor {

    // ── Config ────────────────────────────────────────────────────────────────
    private String  wsUrl;
    private boolean autoConnect;
    private long    reconnectDelayMs;
    private long    beatOffsetMs;
    private double  minConfidence;

    // Variable names (all overridable via plugin config)
    private String varBpm, varRmssd, varSdnn, varPnn50, varSd1, varSd2,
                   varMedianRr, varMotion, varMotionScore,
                   varBreathPhase, varBreathRate,
                   varHeartbeat, varBattery, varConnected;

    // ── Runtime state ─────────────────────────────────────────────────────────
    private volatile WebSocket            webSocket       = null;
    private final AtomicLong              reconnectGen    = new AtomicLong(0);
    private final ExecutorService         messageExecutor = Executors.newSingleThreadExecutor();
    private final ScheduledExecutorService scheduler      = Executors.newScheduledThreadPool(2);

    // ── Constructor ───────────────────────────────────────────────────────────
    public HeartFlowExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    // ── Lifecycle ─────────────────────────────────────────────────────────────

    @Override
    public void launch() {
        wsUrl            = mConfig.getProperty("ws_url",             "ws://localhost:7878");
        autoConnect      = Boolean.parseBoolean(mConfig.getProperty("auto_connect",       "false"));
        reconnectDelayMs = Long.parseLong(    mConfig.getProperty("reconnect_delay_ms", "2000"));
        beatOffsetMs     = Long.parseLong(    mConfig.getProperty("beat_offset_ms",     "0"));
        minConfidence    = Double.parseDouble(mConfig.getProperty("min_confidence",     "0.3"));

        varBpm          = mConfig.getProperty("hf_bpm",             "hf_bpm");
        varRmssd        = mConfig.getProperty("hf_rmssd",           "hf_rmssd");
        varSdnn         = mConfig.getProperty("hf_sdnn",            "hf_sdnn");
        varPnn50        = mConfig.getProperty("hf_pnn50",           "hf_pnn50");
        varSd1          = mConfig.getProperty("hf_sd1",             "hf_sd1");
        varSd2          = mConfig.getProperty("hf_sd2",             "hf_sd2");
        varMedianRr     = mConfig.getProperty("hf_median_rr",       "hf_median_rr");
        varMotion       = mConfig.getProperty("hf_motion",          "hf_motion");
        varMotionScore  = mConfig.getProperty("hf_motion_score",    "hf_motion_score");
        varBreathPhase  = mConfig.getProperty("hf_breathing_phase", "hf_breathing_phase");
        varBreathRate   = mConfig.getProperty("hf_breathing_rate",  "hf_breathing_rate");
        varHeartbeat    = mConfig.getProperty("hf_heartbeat",       "hf_heartbeat");
        varBattery      = mConfig.getProperty("hf_battery",         "hf_battery");
        varConnected    = mConfig.getProperty("hf_connected",       "hf_connected");

        connect(reconnectGen.get());
    }

    @Override
    public void unload() {
        reconnectGen.incrementAndGet();       // invalidate any pending reconnects
        WebSocket ws = webSocket;
        if (ws != null) ws.abort();
        messageExecutor.shutdownNow();
        scheduler.shutdownNow();
        setBoolVar(varConnected, false);
    }

    @Override public String marker(long id) { return "$(" + id + ")"; }
    @Override public void execute(AbstractActivity a) { /* no outbound sceneflow actions */ }

    // ── Connection management ─────────────────────────────────────────────────

    private void connect(long gen) {
        HttpClient.newHttpClient()
                  .newWebSocketBuilder()
                  .buildAsync(URI.create(wsUrl), new HfListener(gen))
                  .exceptionally(ex -> {
                      mLogger.warning("HeartFlow: connect failed — " + ex.getMessage());
                      scheduleReconnect(gen);
                      return null;
                  });
    }

    private void scheduleReconnect(long gen) {
        scheduler.schedule(() -> {
            if (reconnectGen.get() == gen) connect(gen);
        }, reconnectDelayMs, TimeUnit.MILLISECONDS);
    }

    private void sendText(String json) {
        WebSocket ws = webSocket;
        if (ws != null) ws.sendText(json, true);
    }

    // ── Message dispatch ──────────────────────────────────────────────────────

    private void handleMessage(String raw) {
        try {
            JSONObject msg  = new JSONObject(raw);
            String     type = msg.optString("type", "");
            switch (type) {
                case "hr"                -> setFloatVar(varBpm,     msg.getDouble("value"));
                case "bat"               -> setFloatVar(varBattery, msg.getDouble("value"));
                case "hrv"               -> handleHrv(msg);
                case "motion"            -> handleMotion(msg);
                case "breathing"         -> handleBreathing(msg);
                case "heartbeat_predict" -> handleHeartbeatPredict(msg);
                case "status"            -> handleStatus(msg.optString("message", ""));
                default                  -> { /* ecg, ecg_sr, devices — ignored */ }
            }
        } catch (Exception e) {
            mLogger.warning("HeartFlow: parse error — " + e.getMessage());
        }
    }

    private void handleHrv(JSONObject m) {
        setFloatVar(varRmssd,    m.optDouble("rmssd",     -1));
        setFloatVar(varSdnn,     m.optDouble("sdnn",      -1));
        setFloatVar(varPnn50,    m.optDouble("pnn50",     -1));
        setFloatVar(varSd1,      m.optDouble("sd1",       -1));
        setFloatVar(varSd2,      m.optDouble("sd2",       -1));
        setFloatVar(varMedianRr, m.optDouble("median_rr", -1));
    }

    private void handleMotion(JSONObject m) {
        setStringVar(varMotion,     m.optString("category", ""));
        setFloatVar(varMotionScore, m.optDouble("score",    0.0));
    }

    private void handleBreathing(JSONObject m) {
        setStringVar(varBreathPhase, m.optString("phase", ""));
        if (m.optInt("rate", 0) > 0)
            setFloatVar(varBreathRate, m.getDouble("rate"));
    }

    /**
     * Predictive heartbeat scheduling.
     *
     * beat_epoch_ms is the absolute wall-clock time (ms since Unix epoch) at
     * which the next R-peak is predicted to occur, computed by Python as:
     *
     *   actual_beat_ms  = time.time_ns()//1_000_000 − pt_delay_ms
     *   next_beat_epoch = actual_beat_ms + median_rr
     *
     * Python time.time_ns()//1_000_000 and Java System.currentTimeMillis() are
     * both sourced from the same OS clock on localhost, so fireInMs is the
     * exact remaining time until the predicted beat — no latency correction needed.
     */
    private void handleHeartbeatPredict(JSONObject m) {
        long   epochMs    = m.getLong("beat_epoch_ms");
        double confidence = m.optDouble("confidence", 1.0);

        if (confidence < minConfidence) {
            mLogger.message("HeartFlow: heartbeat skipped (confidence " + confidence + " < " + minConfidence + ")");
            return;
        }

        long fireInMs = epochMs - System.currentTimeMillis() + beatOffsetMs;
        if (fireInMs < 0) fireInMs = 0;   // already past — fire immediately

        final long delay = fireInMs;
        scheduler.schedule(
            () -> setStringVar(varHeartbeat, "beat"),
            delay,
            TimeUnit.MILLISECONDS
        );
    }

    private void handleStatus(String message) {
        boolean streaming = message.toLowerCase().contains("streaming");
        setBoolVar(varConnected, streaming);
    }

    // ── Variable helpers (with retry, pattern from ASR plugin) ───────────────

    private void setFloatVar(String name, double value) {
        messageExecutor.submit(() -> {
            for (int i = 0; i < 20; i++) {
                if (mProject.setVariable(name, (float) value)) return;
                try { Thread.sleep(250); } catch (InterruptedException ignored) {}
            }
            mLogger.warning("HeartFlow: variable not found: " + name);
        });
    }

    private void setStringVar(String name, String value) {
        messageExecutor.submit(() -> {
            for (int i = 0; i < 20; i++) {
                if (mProject.setVariable(name, value)) return;
                try { Thread.sleep(250); } catch (InterruptedException ignored) {}
            }
            mLogger.warning("HeartFlow: variable not found: " + name);
        });
    }

    private void setBoolVar(String name, boolean value) {
        for (int i = 0; i < 20; i++) {
            if (mProject.setVariable(name, value)) return;
            try { Thread.sleep(250); } catch (InterruptedException ignored) {}
        }
    }

    // ── WebSocket listener ────────────────────────────────────────────────────

    private class HfListener implements WebSocket.Listener {
        private final long          gen;
        private final StringBuilder buf = new StringBuilder();

        HfListener(long gen) { this.gen = gen; }

        @Override
        public void onOpen(WebSocket ws) {
            if (reconnectGen.get() != gen) { ws.abort(); return; }
            webSocket = ws;
            mLogger.message("HeartFlow: connected to " + wsUrl);
            setBoolVar(varConnected, false);   // true only once "Streaming" status arrives

            if (autoConnect) {
                // Lazy mode: tell Heart Flow to scan and connect to the first Polar H10.
                sendText("{\"action\":\"auto_connect\"}");
                mLogger.message("HeartFlow: sent auto_connect");
            }

            ws.request(1);
        }

        @Override
        public CompletionStage<?> onText(WebSocket ws, CharSequence data, boolean last) {
            buf.append(data);
            if (last) {
                final String raw = buf.toString();
                buf.setLength(0);
                if (reconnectGen.get() == gen)
                    messageExecutor.submit(() -> handleMessage(raw));
            }
            ws.request(1);
            return null;
        }

        @Override
        public CompletionStage<?> onClose(WebSocket ws, int code, String reason) {
            webSocket = null;
            setBoolVar(varConnected, false);
            mLogger.warning("HeartFlow: disconnected (" + code + " " + reason + ")");
            scheduleReconnect(gen);
            return null;
        }

        @Override
        public void onError(WebSocket ws, Throwable err) {
            webSocket = null;
            setBoolVar(varConnected, false);
            mLogger.warning("HeartFlow: WS error — " + err.getMessage());
            scheduleReconnect(gen);
        }
    }
}
