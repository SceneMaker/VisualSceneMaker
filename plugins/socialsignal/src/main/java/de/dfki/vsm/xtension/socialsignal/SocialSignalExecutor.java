package de.dfki.vsm.xtension.socialsignal;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.json.JSONArray;
import org.json.JSONObject;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.WebSocket;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Consumer;

/**
 * SocialSignalStream VSM plugin.
 *
 * Connects to the SocialSignalStream WebSocket server (ws://localhost:7070/ws) and maps the
 * incoming FeatureFrame JSON — head pose, mouth, expression, gaze, blink, nod/shake, body lean,
 * smile sub-classification, eyebrow AUs, hand-face self-touch and turn context — to SceneFlow
 * variables.
 *
 * <h3>Rate handling</h3>
 * SocialSignalStream broadcasts one frame per camera frame (30 FPS) with ~50 fields. Writing all
 * of them straight through would call {@code Interpreter.setVariable} ~1500×/s, and every such
 * call takes the interpreter lock and re-evaluates all conditional edges. The plugin therefore:
 *
 * <ul>
 *   <li>writes a variable only when its value actually changed (floats: only when the change
 *       exceeds a deadband — {@code angle_deadband} in degrees, {@code score_deadband} for
 *       normalised 0–1 values),</li>
 *   <li>rate-limits all continuous signals to one pass per {@code update_interval_ms}
 *       (default 100 ms = 10 Hz), and</li>
 *   <li>exempts the edge-triggered events (nod, shake, blink) from that gate, so a gesture
 *       that is flagged for a single frame is never dropped.</li>
 * </ul>
 *
 * <h3>Unused signals</h3>
 * Every signal is optional. A signal is skipped when its SceneFlow variable does not exist (the
 * plugin probes with {@code hasVariable} for {@code variable_timeout_ms} after launch, then logs
 * once and disables it), or when its name is blanked in the plugin configuration. So deleting the
 * variables you do not need from the SceneFlow is the intended way to trim the plugin down.
 *
 * <h3>No image data</h3>
 * The feature WebSocket carries the FeatureFrame JSON and nothing else, so no pixels ever reach
 * VSM. SocialSignalStream's only image endpoint is {@code GET /preview/{index}}, a JPEG for the
 * device picker in its own browser UI, which this plugin never calls. Recordings are written on
 * the SocialSignalStream host; VSM only ever learns the session id.
 *
 * <h3>Starting the camera pipeline</h3>
 * SocialSignalStream idles until a capture device is chosen (normally in its browser UI at
 * http://localhost:7070). With {@code auto_start = true} the plugin issues that POST /start
 * itself on connect, so no browser interaction is needed. The same is available as the
 * {@code start} action.
 *
 * All variable bookkeeping runs on a single message thread, so the maps below need no locking.
 */
public class SocialSignalExecutor extends ActivityExecutor {

    // ── Variable configuration keys ───────────────────────────────────────────
    // Connection / face presence
    private static final String V_CONNECTED    = "sss_connected";
    private static final String V_STREAMING    = "sss_streaming";
    private static final String V_FACE_VISIBLE = "sss_face_visible";
    // Head pose
    private static final String V_HEAD_PITCH   = "sss_head_pitch";
    private static final String V_HEAD_YAW     = "sss_head_yaw";
    private static final String V_HEAD_ROLL    = "sss_head_roll";
    // Mouth
    private static final String V_MOUTH_STATE  = "sss_mouth_state";
    private static final String V_MOUTH_OPEN   = "sss_mouth_openness";
    // Expression
    private static final String V_EXPRESSION   = "sss_expression";
    private static final String V_EXPR_CONF    = "sss_expression_confidence";
    // Head gestures (edge-triggered)
    private static final String V_NOD          = "sss_nod";
    private static final String V_SHAKE        = "sss_shake";
    // Gaze
    private static final String V_GAZE_ZONE    = "sss_gaze_zone";
    private static final String V_GAZE_X       = "sss_gaze_x";
    private static final String V_GAZE_Y       = "sss_gaze_y";
    // Blink
    private static final String V_BLINK        = "sss_blink";
    private static final String V_BLINK_RATE   = "sss_blink_rate";
    // Body lean
    private static final String V_LEAN_STATE   = "sss_lean_state";
    private static final String V_LEAN_ANGLE   = "sss_lean_angle";
    // Smile sub-classification
    private static final String V_SMILE_TYPE   = "sss_smile_type";
    private static final String V_SMILE_CAT    = "sss_smile_category";
    private static final String V_SMILE_ASYM   = "sss_smile_asymmetry";
    private static final String V_SMILE_CONT   = "sss_smile_contempt";
    private static final String V_SMILE_ONSET  = "sss_smile_onset_side";
    private static final String V_SMILE_DELTA  = "sss_smile_onset_delta";
    // Eyebrows
    private static final String V_BROW_INNER   = "sss_brow_inner_raise";
    private static final String V_BROW_OUT_L   = "sss_brow_outer_left";
    private static final String V_BROW_OUT_R   = "sss_brow_outer_right";
    private static final String V_BROW_FUR_L   = "sss_brow_furrow_left";
    private static final String V_BROW_FUR_R   = "sss_brow_furrow_right";
    private static final String V_BROW_OUT_AS  = "sss_brow_outer_asym";
    private static final String V_BROW_IN_AS   = "sss_brow_inner_asym";
    // Hand-face self-touch
    private static final String V_HF_TOUCH     = "sss_hand_face_touch";
    private static final String V_HF_GESTURE   = "sss_hand_face_gesture";
    private static final String V_HF_HAND      = "sss_hand_face_hand";
    private static final String V_HF_CONF      = "sss_hand_face_confidence";
    // Turn context
    private static final String V_TURN_STATE   = "sss_turn_state";
    private static final String V_SPEECH_RATIO = "sss_speech_ratio";
    private static final String V_VAD_ACTIVE   = "sss_vad_active";
    private static final String V_SILENCE_S    = "sss_time_since_utterance";
    // Recording (written from the REST responses, not from the frame stream)
    private static final String V_RECORDING    = "sss_recording";
    private static final String V_REC_SESSION  = "sss_recording_session";

    private static final String[] ALL_VARS = {
        V_CONNECTED, V_STREAMING, V_FACE_VISIBLE,
        V_HEAD_PITCH, V_HEAD_YAW, V_HEAD_ROLL,
        V_MOUTH_STATE, V_MOUTH_OPEN,
        V_EXPRESSION, V_EXPR_CONF,
        V_NOD, V_SHAKE,
        V_GAZE_ZONE, V_GAZE_X, V_GAZE_Y,
        V_BLINK, V_BLINK_RATE,
        V_LEAN_STATE, V_LEAN_ANGLE,
        V_SMILE_TYPE, V_SMILE_CAT, V_SMILE_ASYM, V_SMILE_CONT, V_SMILE_ONSET, V_SMILE_DELTA,
        V_BROW_INNER, V_BROW_OUT_L, V_BROW_OUT_R, V_BROW_FUR_L, V_BROW_FUR_R,
        V_BROW_OUT_AS, V_BROW_IN_AS,
        V_HF_TOUCH, V_HF_GESTURE, V_HF_HAND, V_HF_CONF,
        V_TURN_STATE, V_SPEECH_RATIO, V_VAD_ACTIVE, V_SILENCE_S,
        V_RECORDING, V_REC_SESSION
    };

    // ── Config ────────────────────────────────────────────────────────────────
    private String  mWsUrl;
    private String  mHttpUrl;
    private boolean mAutoStart;
    private int     mDeviceIndex;
    private int     mCaptureWidth;
    private int     mCaptureHeight;
    private long    mReconnectDelayMs;
    private long    mUpdateIntervalMs;
    private double  mAngleDeadband;
    private double  mScoreDeadband;
    private boolean mWriteWhenInvalid;
    private long    mVariableTimeoutMs;
    private long    mStreamTimeoutMs;

    /** Config key → configured SceneFlow variable name (empty = signal switched off). */
    private final Map<String, String> mVarName = new HashMap<>();

    // ── Runtime state (message thread only, except where noted) ───────────────
    private final Map<String, Object> mLastValue = new HashMap<>();
    private final Map<String, Long>   mNextProbe = new HashMap<>();
    private final Set<String>         mResolved  = new HashSet<>();
    private final Set<String>         mDisabled  = new HashSet<>();
    private long mLaunchMs        = 0L;
    private long mLastFrameMs     = 0L;
    private long mLastContinuous  = 0L;

    private volatile WebSocket       mWebSocket      = null;
    private volatile HttpClient      mHttpClient     = null;
    private ExecutorService          mHttpExecutor   = null;
    private ExecutorService          mMessageExecutor = null;
    private ScheduledExecutorService mScheduler      = null;
    private final AtomicLong         mReconnectGen   = new AtomicLong(0);

    // ── Constructor ───────────────────────────────────────────────────────────
    public SocialSignalExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    // ── Lifecycle ─────────────────────────────────────────────────────────────

    @Override
    public void launch() {
        mWsUrl             = mConfig.getProperty("ws_url", "ws://localhost:7070/ws").trim();
        mHttpUrl           = mConfig.getProperty("http_url", "").trim();
        if (mHttpUrl.isEmpty()) {
            mHttpUrl = deriveHttpUrl(mWsUrl);
        }
        mAutoStart         = Boolean.parseBoolean(mConfig.getProperty("auto_start", "false"));
        mDeviceIndex       = parseInt(mConfig.getProperty("device_index",    "0"),   0);
        mCaptureWidth      = parseInt(mConfig.getProperty("capture_width",   "640"), 640);
        mCaptureHeight     = parseInt(mConfig.getProperty("capture_height",  "480"), 480);
        mReconnectDelayMs  = parseLong(mConfig.getProperty("reconnect_delay_ms",  "2000"),  2000L);
        mUpdateIntervalMs  = parseLong(mConfig.getProperty("update_interval_ms",  "100"),   100L);
        mAngleDeadband     = parseDouble(mConfig.getProperty("angle_deadband",    "0.5"),   0.5);
        mScoreDeadband     = parseDouble(mConfig.getProperty("score_deadband",    "0.02"),  0.02);
        mWriteWhenInvalid  = Boolean.parseBoolean(mConfig.getProperty("write_when_invalid", "false"));
        mVariableTimeoutMs = parseLong(mConfig.getProperty("variable_timeout_ms", "20000"), 20000L);
        mStreamTimeoutMs   = parseLong(mConfig.getProperty("stream_timeout_ms",   "1500"),  1500L);

        mVarName.clear();
        for (String key : ALL_VARS) {
            mVarName.put(key, mConfig.getProperty(key, key).trim());
        }

        // Recreate per-run state — the same plugin instance is reused across stop/start cycles.
        mLastValue.clear();
        mNextProbe.clear();
        mResolved.clear();
        mDisabled.clear();
        mLaunchMs       = System.currentTimeMillis();
        mLastFrameMs    = 0L;
        mLastContinuous = 0L;

        mMessageExecutor = Executors.newSingleThreadExecutor();
        mScheduler       = Executors.newScheduledThreadPool(2);

        // Owned executor so the HttpClient's threads can be shut down on unload
        // (HttpClient.close() is Java 21+; explicit executor shutdown works on Java 17).
        mHttpExecutor = Executors.newCachedThreadPool();
        mHttpClient   = HttpClient.newBuilder().executor(mHttpExecutor).build();

        mScheduler.scheduleWithFixedDelay(
            () -> submit(this::checkStreamAlive), 500, 500, TimeUnit.MILLISECONDS);

        connect(mReconnectGen.get());
    }

    @Override
    public void unload() {
        mReconnectGen.incrementAndGet();   // stop the reconnect loop

        WebSocket ws = mWebSocket;
        mWebSocket = null;
        if (ws != null) {
            // Close frame first so the server drops the client cleanly; abort() as fallback.
            ws.sendClose(WebSocket.NORMAL_CLOSURE, "").whenComplete((v, ex) -> ws.abort());
        }

        if (mMessageExecutor != null) mMessageExecutor.shutdownNow();
        if (mScheduler != null)       mScheduler.shutdownNow();

        mHttpClient = null;
        if (mHttpExecutor != null) {
            mHttpExecutor.shutdownNow();
            mHttpExecutor = null;
        }

        // Best-effort — the interpreter may already be stopped.
        writeDirect(V_CONNECTED, false);
        writeDirect(V_STREAMING, false);
    }

    @Override
    public String marker(long id) {
        return "$(" + id + ")";
    }

    // ── Outbound actions ──────────────────────────────────────────────────────

    @Override
    public void execute(final AbstractActivity activity) {
        final String name = activity.getName() == null
                ? "" : activity.getName().toLowerCase(Locale.ROOT).trim();
        switch (name) {
            case "start" -> {
                int device = parseInt(getActionFeatureValue("device_index", activity.getFeatures()), mDeviceIndex);
                int width  = parseInt(getActionFeatureValue("width",        activity.getFeatures()), mCaptureWidth);
                int height = parseInt(getActionFeatureValue("height",       activity.getFeatures()), mCaptureHeight);
                sendStart(device, width, height);
            }
            case "calibrate_gaze" -> post("/gaze/calibrate", "{}");
            case "patch" -> sendPatch(
                    getActionFeatureValue("key",   activity.getFeatures()),
                    getActionFeatureValue("value", activity.getFeatures()));
            case "recording_start" -> startRecording(
                    getActionFeatureValue("marks",     activity.getFeatures()),
                    getActionFeatureValue("scenarios", activity.getFeatures()),
                    getActionFeatureValue("lang",      activity.getFeatures()));
            case "recording_stop" -> stopRecording();
            case "mark" -> markPrompt(
                    getActionFeatureValue("id",    activity.getFeatures()),
                    getActionFeatureValue("type",  activity.getFeatures()),
                    getActionFeatureValue("label", activity.getFeatures()),
                    getActionFeatureValue("text",  activity.getFeatures()));
            default -> mLogger.warning("SocialSignalStream: unknown action: " + name);
        }
    }

    private void sendStart(int device, int width, int height) {
        JSONObject body = new JSONObject();
        body.put("device_index", device);
        body.put("width",  width);
        body.put("height", height);
        post("/start", body.toString());
    }

    /**
     * Hot-patch a SocialSignalStream config value, e.g. key={@code mouth.mar_open_threshold},
     * value={@code 0.35}. The patch travels on the same WebSocket the frames arrive on.
     */
    private void sendPatch(String key, String value) {
        if (key == null || key.isBlank()) {
            mLogger.warning("SocialSignalStream: patch action without a key");
            return;
        }
        JSONObject patch = new JSONObject();
        patch.put(key.trim(), coerce(value));
        sendText(patch.toString());
    }

    // ── Recording ─────────────────────────────────────────────────────────────

    /**
     * Start a recording on the SocialSignalStream host. Video and features.csv are written there;
     * VSM only learns the session id.
     *
     * <p>A "marked" session (the default) uses the prompted recorder, which additionally writes a
     * prompts.csv on the recording clock. That is what makes the {@code mark} action usable, so a
     * flow can stamp what the agent was doing into the same timeline as the measured signals.
     * Pass {@code marks='false'} for a plain recording without that file.
     */
    private void startRecording(String marks, String scenarios, String lang) {
        boolean marked = marks == null || marks.isBlank() || Boolean.parseBoolean(marks.trim());
        if (!marked) {
            post("/recording/start", "{}", this::onRecordingStarted);
            return;
        }
        JSONArray ids = new JSONArray();
        if (scenarios != null && !scenarios.isBlank()) {
            for (String id : scenarios.split(",")) {
                if (!id.isBlank()) ids.put(id.trim());
            }
        }
        JSONObject body = new JSONObject();
        body.put("scenario_ids", ids);
        body.put("lang", lang == null || lang.isBlank() ? "en" : lang.trim());
        post("/recording/start/prompted", body.toString(), this::onRecordingStarted);
    }

    private void onRecordingStarted(JSONObject response) {
        setString(V_REC_SESSION, response.optString("session_id", ""));
        setBool(V_RECORDING, true);
    }

    private void stopRecording() {
        post("/recording/stop", "{}", response -> {
            setBool(V_RECORDING, false);
            String session = response.optString("session_id", "");
            if (!session.isEmpty()) setString(V_REC_SESSION, session);
        });
    }

    /**
     * Stamp a moment into the running recording's prompts.csv, with the start and end times
     * measured on the recording clock. Requires a session started with {@code marks} enabled.
     */
    private void markPrompt(String id, String type, String label, String text) {
        JSONObject body = new JSONObject();
        body.put("scenario_id",    id    == null ? "" : id.trim());
        body.put("prompt_type",    type  == null ? "" : type.trim());
        body.put("expected_label", label == null || label.isBlank() ? "none" : label.trim());
        body.put("prompt_text",    text  == null ? "" : text);
        post("/recording/prompt", body.toString(), null);
    }

    /** Numbers and booleans are patched as such; anything else stays a string. */
    private static Object coerce(String raw) {
        if (raw == null) return "";
        String v = raw.trim();
        if (v.equalsIgnoreCase("true"))  return Boolean.TRUE;
        if (v.equalsIgnoreCase("false")) return Boolean.FALSE;
        try {
            return Double.valueOf(v);
        } catch (NumberFormatException ignored) {
            return v;
        }
    }

    // ── Connection management ─────────────────────────────────────────────────

    private void connect(long gen) {
        HttpClient client = mHttpClient;
        if (mReconnectGen.get() != gen || client == null) return;
        try {
            client.newWebSocketBuilder()
                  .buildAsync(URI.create(mWsUrl), new SssListener(gen))
                  .exceptionally(ex -> {
                      mLogger.warning("SocialSignalStream: connect failed — " + ex.getMessage());
                      scheduleReconnect(gen);
                      return null;
                  });
        } catch (Exception e) {   // malformed ws_url — retrying will not help
            mLogger.failure("SocialSignalStream: cannot connect to '" + mWsUrl + "' — " + e.getMessage());
        }
    }

    private void scheduleReconnect(long gen) {
        ScheduledExecutorService scheduler = mScheduler;
        if (scheduler == null) return;
        try {
            scheduler.schedule(() -> {
                if (mReconnectGen.get() == gen) connect(gen);
            }, mReconnectDelayMs, TimeUnit.MILLISECONDS);
        } catch (RejectedExecutionException ignored) {
            // shutting down
        }
    }

    private void sendText(String json) {
        WebSocket ws = mWebSocket;
        if (ws != null) {
            ws.sendText(json, true);
        } else {
            mLogger.warning("SocialSignalStream: not connected — dropped " + json);
        }
    }

    private void post(String path, String jsonBody) {
        post(path, jsonBody, null);
    }

    /**
     * Fire-and-forget POST. When {@code onResponse} is given it runs on the message thread with
     * the parsed body, but only for a successful call — SocialSignalStream reports refusals as
     * {@code {"error": ...}}, which are logged instead.
     */
    private void post(String path, String jsonBody, Consumer<JSONObject> onResponse) {
        HttpClient client = mHttpClient;
        if (client == null) return;
        try {
            HttpRequest req = HttpRequest.newBuilder(URI.create(mHttpUrl + path))
                    .header("Content-Type", "application/json")
                    .timeout(Duration.ofSeconds(5))
                    .POST(HttpRequest.BodyPublishers.ofString(
                            jsonBody == null ? "{}" : jsonBody, StandardCharsets.UTF_8))
                    .build();
            client.sendAsync(req, HttpResponse.BodyHandlers.ofString())
                  .thenAccept(r -> handleResponse(path, r.statusCode(), r.body(), onResponse))
                  .exceptionally(ex -> {
                      mLogger.warning("SocialSignalStream: POST " + path + " failed — " + ex.getMessage());
                      return null;
                  });
        } catch (Exception e) {
            mLogger.warning("SocialSignalStream: POST " + path + " failed — " + e.getMessage());
        }
    }

    private void handleResponse(String path, int status, String body, Consumer<JSONObject> onResponse) {
        JSONObject json = null;
        try {
            json = new JSONObject(body == null ? "" : body);
        } catch (Exception ignored) {
            // not JSON — the status code below is all we report
        }
        String error = json == null ? "" : json.optString("error", "");
        if (status >= 400 || !error.isEmpty()) {
            mLogger.warning("SocialSignalStream: " + path + " → " + status
                    + (error.isEmpty() ? "" : " — " + error));
            return;
        }
        mLogger.message("SocialSignalStream: POST " + path + " → " + status);
        if (onResponse != null && json != null) {
            final JSONObject payload = json;
            submit(() -> onResponse.accept(payload));
        }
    }

    /** http://host:port from ws://host:port/ws — used for the REST control endpoints. */
    private static String deriveHttpUrl(String wsUrl) {
        String url = wsUrl;
        if (url.startsWith("ws://"))       url = "http://"  + url.substring(5);
        else if (url.startsWith("wss://")) url = "https://" + url.substring(6);
        int slash = url.indexOf('/', url.indexOf("//") + 2);
        if (slash > 0) url = url.substring(0, slash);
        return url;
    }

    // ── Frame handling ────────────────────────────────────────────────────────

    private void handleMessage(String raw) {
        final JSONObject f;
        try {
            f = new JSONObject(raw);
        } catch (Exception e) {
            mLogger.warning("SocialSignalStream: parse error — " + e.getMessage());
            return;
        }
        if (!f.has("frame_id") && !f.has("valid")) return;   // not a FeatureFrame

        final long now = System.currentTimeMillis();
        mLastFrameMs = now;
        setBool(V_STREAMING, true);

        final boolean valid = f.optBoolean("valid", true);
        setBool(V_FACE_VISIBLE, valid);
        if (!valid && !mWriteWhenInvalid) return;   // no face — the feature fields are defaults

        // Edge-triggered gestures are flagged for a single frame, so they bypass the rate gate.
        if (f.optBoolean("nod_detected",   false)) fireEvent(V_NOD,   "nod");
        if (f.optBoolean("shake_detected", false)) fireEvent(V_SHAKE, "shake");
        if (f.optBoolean("blink_detected", false)) fireEvent(V_BLINK, "blink");

        if (now - mLastContinuous < mUpdateIntervalMs) return;
        mLastContinuous = now;

        // Head pose (degrees)
        setFloat(V_HEAD_PITCH, f.optDouble("head_pitch", 0.0), mAngleDeadband);
        setFloat(V_HEAD_YAW,   f.optDouble("head_yaw",   0.0), mAngleDeadband);
        setFloat(V_HEAD_ROLL,  f.optDouble("head_roll",  0.0), mAngleDeadband);

        // Mouth
        setString(V_MOUTH_STATE, f.optString("mouth_state", ""));
        setFloat(V_MOUTH_OPEN,   f.optDouble("mouth_openness", 0.0), mScoreDeadband);

        // Expression
        setString(V_EXPRESSION, f.optString("expression", ""));
        setFloat(V_EXPR_CONF,   f.optDouble("expression_confidence", 0.0), mScoreDeadband);

        // Gaze
        setString(V_GAZE_ZONE, f.optString("gaze_zone", ""));
        setFloat(V_GAZE_X,     f.optDouble("gaze_x", 0.0), mScoreDeadband);
        setFloat(V_GAZE_Y,     f.optDouble("gaze_y", 0.0), mScoreDeadband);

        // Blink rate (blinks per minute, 60 s rolling window)
        setFloat(V_BLINK_RATE, f.optDouble("blink_rate_bpm", 0.0), mAngleDeadband);

        // Body lean
        setString(V_LEAN_STATE, f.optString("lean_state", ""));
        setFloat(V_LEAN_ANGLE,  f.optDouble("lean_angle_deg", 0.0), mAngleDeadband);

        // Smile sub-classification
        setString(V_SMILE_TYPE,  f.optString("smile_type", ""));
        setString(V_SMILE_CAT,   f.optString("smile_category", ""));
        setFloat(V_SMILE_ASYM,   f.optDouble("smile_asymmetry", 0.0), mScoreDeadband);
        setBool(V_SMILE_CONT,    f.optBoolean("smile_contempt", false));
        setString(V_SMILE_ONSET, f.optString("smile_onset_side", ""));
        setFloat(V_SMILE_DELTA,  f.optDouble("smile_onset_delta_ms", 0.0), 1.0);

        // Eyebrows (blendshape action units, 0–1)
        setFloat(V_BROW_INNER,  f.optDouble("brow_inner_raise",     0.0), mScoreDeadband);
        setFloat(V_BROW_OUT_L,  f.optDouble("brow_outer_up_left",   0.0), mScoreDeadband);
        setFloat(V_BROW_OUT_R,  f.optDouble("brow_outer_up_right",  0.0), mScoreDeadband);
        setFloat(V_BROW_FUR_L,  f.optDouble("brow_furrow_left",     0.0), mScoreDeadband);
        setFloat(V_BROW_FUR_R,  f.optDouble("brow_furrow_right",    0.0), mScoreDeadband);
        setFloat(V_BROW_OUT_AS, f.optDouble("brow_outer_asymmetry", 0.0), mScoreDeadband);
        setFloat(V_BROW_IN_AS,  f.optDouble("brow_inner_asym",      0.0), mScoreDeadband);

        // Hand-face self-touch
        setBool(V_HF_TOUCH,     f.optBoolean("hand_face_touch", false));
        setString(V_HF_GESTURE, f.optString("hand_face_gesture", ""));
        setString(V_HF_HAND,    f.optString("hand_face_which_hand", ""));
        setFloat(V_HF_CONF,     f.optDouble("hand_face_confidence", 0.0), mScoreDeadband);

        // Turn context
        setString(V_TURN_STATE,  f.optString("turn_state", ""));
        setFloat(V_SPEECH_RATIO, f.optDouble("speech_ratio", 0.0), mScoreDeadband);
        setBool(V_VAD_ACTIVE,    f.optBoolean("vad_active", false));
        // A running clock: the 0.5 s deadband keeps it at roughly two writes per second.
        setFloat(V_SILENCE_S,    f.optDouble("time_since_last_utterance_s", -1.0), 0.5);
    }

    /** Frames stop arriving whenever the SocialSignalStream pipeline is not capturing. */
    private void checkStreamAlive() {
        if (mLastFrameMs == 0L) return;
        if (System.currentTimeMillis() - mLastFrameMs > mStreamTimeoutMs) {
            setBool(V_STREAMING, false);
        }
    }

    // ── Variable writing ──────────────────────────────────────────────────────

    /**
     * A signal is written only once its SceneFlow variable exists. The probe is retried at 1 Hz
     * for {@code variable_timeout_ms} after launch (the interpreter needs a moment to build the
     * environment), then the signal is logged once and switched off for the rest of the run.
     */
    private boolean resolve(String key) {
        if (mDisabled.contains(key)) return false;
        if (mResolved.contains(key)) return true;

        String name = mVarName.get(key);
        if (name == null || name.isEmpty()) {   // blanked in the plugin configuration
            mDisabled.add(key);
            return false;
        }

        long now  = System.currentTimeMillis();
        Long next = mNextProbe.get(key);
        if (next != null && now < next) return false;

        boolean exists;
        try {
            exists = mProject.hasVariable(name);
        } catch (Exception e) {
            exists = false;
        }
        if (exists) {
            mResolved.add(key);
            return true;
        }
        if (now - mLaunchMs > mVariableTimeoutMs) {
            mDisabled.add(key);
            mLogger.message("SocialSignalStream: no SceneFlow variable '" + name + "' — signal off");
            return false;
        }
        mNextProbe.put(key, now + 1000);
        return false;
    }

    private void disable(String key, String reason) {
        mDisabled.add(key);
        mLogger.warning("SocialSignalStream: write to '" + mVarName.get(key) + "' failed (" + reason
                + ") — signal off");
    }

    private void setFloat(String key, double value, double deadband) {
        if (!resolve(key)) return;
        Object prev = mLastValue.get(key);
        if (prev instanceof Float p && Math.abs(p - value) < deadband) return;
        float v = (float) value;
        boolean ok;
        try {
            ok = mProject.setVariable(mVarName.get(key), v);
        } catch (Exception e) {
            disable(key, e.getMessage());
            return;
        }
        if (ok) mLastValue.put(key, v); else disable(key, "rejected");
    }

    private void setString(String key, String value) {
        if (!resolve(key)) return;
        if (Objects.equals(mLastValue.get(key), value)) return;
        boolean ok;
        try {
            ok = mProject.setVariable(mVarName.get(key), value);
        } catch (Exception e) {
            disable(key, e.getMessage());
            return;
        }
        if (ok) mLastValue.put(key, value); else disable(key, "rejected");
    }

    private void setBool(String key, boolean value) {
        if (!resolve(key)) return;
        if (Objects.equals(mLastValue.get(key), value)) return;
        boolean ok;
        try {
            ok = mProject.setVariable(mVarName.get(key), value);
        } catch (Exception e) {
            disable(key, e.getMessage());
            return;
        }
        if (ok) mLastValue.put(key, value); else disable(key, "rejected");
    }

    /** Event variables re-fire on every occurrence, so they are never deduplicated. */
    private void fireEvent(String key, String value) {
        if (!resolve(key)) return;
        boolean ok;
        try {
            ok = mProject.setVariable(mVarName.get(key), value);
        } catch (Exception e) {
            disable(key, e.getMessage());
            return;
        }
        if (!ok) disable(key, "rejected");
    }

    /** Bypasses the message thread — for unload, when the executors are already gone. */
    private void writeDirect(String key, boolean value) {
        String name = mVarName.get(key);
        if (name == null || name.isEmpty()) return;
        try {
            mProject.setVariable(name, value);
        } catch (Exception ignored) {
            // interpreter already stopped
        }
    }

    private void submit(Runnable task) {
        ExecutorService exec = mMessageExecutor;
        if (exec == null) return;
        try {
            exec.submit(task);
        } catch (RejectedExecutionException ignored) {
            // shutting down
        }
    }

    // ── Parsing helpers ───────────────────────────────────────────────────────

    private static int parseInt(String raw, int fallback) {
        if (raw == null || raw.isBlank()) return fallback;
        try { return Integer.parseInt(raw.trim()); } catch (NumberFormatException e) { return fallback; }
    }

    private static long parseLong(String raw, long fallback) {
        if (raw == null || raw.isBlank()) return fallback;
        try { return Long.parseLong(raw.trim()); } catch (NumberFormatException e) { return fallback; }
    }

    private static double parseDouble(String raw, double fallback) {
        if (raw == null || raw.isBlank()) return fallback;
        try { return Double.parseDouble(raw.trim()); } catch (NumberFormatException e) { return fallback; }
    }

    // ── WebSocket listener ────────────────────────────────────────────────────

    private class SssListener implements WebSocket.Listener {
        private final long          gen;
        private final StringBuilder buf = new StringBuilder();

        SssListener(long gen) { this.gen = gen; }

        @Override
        public void onOpen(WebSocket ws) {
            if (mReconnectGen.get() != gen) { ws.abort(); return; }
            mWebSocket = ws;
            mLogger.message("SocialSignalStream: connected to " + mWsUrl);
            submit(() -> setBool(V_CONNECTED, true));

            if (mAutoStart) {
                // The pipeline idles until a capture device is chosen; do it without the browser UI.
                sendStart(mDeviceIndex, mCaptureWidth, mCaptureHeight);
                mLogger.message("SocialSignalStream: sent auto start (device " + mDeviceIndex + ")");
            }
            ws.request(1);
        }

        @Override
        public CompletionStage<?> onText(WebSocket ws, CharSequence data, boolean last) {
            buf.append(data);
            if (last) {
                final String raw = buf.toString();
                buf.setLength(0);
                if (mReconnectGen.get() == gen) submit(() -> handleMessage(raw));
            }
            ws.request(1);
            return null;
        }

        @Override
        public CompletionStage<?> onClose(WebSocket ws, int code, String reason) {
            mWebSocket = null;
            submit(() -> { setBool(V_CONNECTED, false); setBool(V_STREAMING, false); });
            mLogger.warning("SocialSignalStream: disconnected (" + code + " " + reason + ")");
            scheduleReconnect(gen);
            return null;
        }

        @Override
        public void onError(WebSocket ws, Throwable err) {
            mWebSocket = null;
            submit(() -> { setBool(V_CONNECTED, false); setBool(V_STREAMING, false); });
            mLogger.warning("SocialSignalStream: WS error — " + err.getMessage());
            scheduleReconnect(gen);
        }
    }
}
