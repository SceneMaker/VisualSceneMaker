package de.dfki.vsm.xtension.alma;

import de.dfki.vsm.util.log.LOGConsoleLogger;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.IOException;
import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.WebSocket;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * WebSocket client for the standalone ALMA affect server ({@code de.affect.frontend.Server} in
 * ALMA2025). Replaces the old in-process {@code AffectManager} embedding: appraisal signals and
 * affect updates travel as JSON envelopes {@code {"type","id","payload"}} over {@code /ws}
 * instead of direct Java calls. The AffectInput XML fragment format itself is unchanged from the
 * old {@code de.affect.util.AppraisalTag}-built XML, just carried as a string inside the payload.
 */
public class AlmaWsClient {

    public interface Listener {
        void onAuthResult(boolean ok);

        void onAffectInfo(String character, String dominantEmotionType, double dominantEmotionIntensity,
                           String moodName, String moodTendencyName);

        void onEmotionVector(String character, List<String> activeEmotions);

        void onError(String message);

        /** Fires when the socket closes, including an unexpected drop after a successful connect. */
        void onClose(int statusCode, String reason);
    }

    /** Mirrors de.affect.util.AppraisalTag.EventTags/ActionTags/ObjectTags (ALMA2025) — public wire vocabulary. */
    private static final Set<String> EVENT_TAGS = Set.of(
            "GoodEvent", "BadEvent", "GoodEventForGoodOther", "GoodEventForBadOther",
            "BadEventForBadOther", "BadEventForGoodOther", "GoodLikelyFutureEvent",
            "BadLikelyFutureEvent", "GoodUnlikelyFutureEvent", "BadUnlikelyFutureEvent",
            "EventConfirmed", "EventDisconfirmed");
    private static final Set<String> ACTION_TAGS = Set.of(
            "GoodActSelf", "BadActSelf", "GoodActOther", "BadActOther");
    private static final Set<String> OBJECT_TAGS = Set.of("NiceThing", "NastyThing");

    private static final double EMOTION_ACTIVE_THRESHOLD = 0.25;
    private static final Duration AUTH_TIMEOUT = Duration.ofSeconds(15);
    private static final Duration REPLY_TIMEOUT = Duration.ofSeconds(15);

    private final String mWsUrl;
    private final String mTokenUrl;
    private final String mClientId;
    private final String mClientSecret;
    private final Listener mListener;
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    private final HttpClient mHttp = HttpClient.newHttpClient();
    private final StringBuilder mIncoming = new StringBuilder();
    private final ConcurrentHashMap<String, CompletableFuture<JSONObject>> mPendingReplies = new ConcurrentHashMap<>();

    private volatile WebSocket mSocket;
    private volatile CompletableFuture<Boolean> mPendingAuth;

    public AlmaWsClient(String wsUrl, String tokenUrl, String clientId, String clientSecret, Listener listener) {
        mWsUrl = wsUrl;
        mTokenUrl = tokenUrl;
        mClientId = clientId;
        mClientSecret = clientSecret;
        mListener = listener;
    }

    public static boolean isAppraisalTag(String tag) {
        return EVENT_TAGS.contains(tag) || ACTION_TAGS.contains(tag) || OBJECT_TAGS.contains(tag);
    }

    /**
     * Blocking: opens the WS, authenticates if configured, and uploads the project. Call off the
     * runtime thread.
     *
     * Authentication is skipped entirely when {@code client_id}/{@code client_secret}/
     * {@code keycloak_token_url} are left blank — that's the local-ALMA2025 case, where the server
     * runs without {@code OIDC_ISSUER_URL} and treats {@code /ws} as open. A server with auth
     * disabled still replies to an unsolicited {@code auth} message, but with a plain {@code "ok"}
     * rather than {@code "authResult"} (see {@code Server.java}'s {@code case "auth"} vs the
     * pre-userId WS-level gate), so sending it unconditionally would just hang this client waiting
     * for a reply that never comes — skipping the message outright is simpler than special-casing
     * that reply.
     */
    public void connectAndInit(String projectXml, String fileName) throws IOException, InterruptedException {
        boolean authConfigured = hasText(mClientId) && hasText(mClientSecret) && hasText(mTokenUrl);
        String token = authConfigured ? fetchToken() : null;

        WebSocket.Listener wsListener = new WebSocket.Listener() {
            @Override
            public CompletionStage<?> onText(WebSocket webSocket, CharSequence data, boolean last) {
                mIncoming.append(data);
                webSocket.request(1);
                if (last) {
                    String message = mIncoming.toString();
                    mIncoming.setLength(0);
                    handleMessage(message);
                }
                return null;
            }

            @Override
            public void onError(WebSocket webSocket, Throwable error) {
                mListener.onError(error.getMessage());
            }

            @Override
            public CompletionStage<?> onClose(WebSocket webSocket, int statusCode, String reason) {
                mSocket = null;
                mListener.onClose(statusCode, reason);
                return null;
            }
        };

        try {
            mSocket = mHttp.newWebSocketBuilder().buildAsync(URI.create(mWsUrl), wsListener).join();
        } catch (Exception ex) {
            throw new IOException("could not open WebSocket to " + mWsUrl, ex);
        }

        if (authConfigured) {
            CompletableFuture<Boolean> authFuture = new CompletableFuture<>();
            mPendingAuth = authFuture;
            send(envelope("auth", new JSONObject().put("token", token)));

            Boolean authOk;
            try {
                authOk = authFuture.get(AUTH_TIMEOUT.toSeconds(), TimeUnit.SECONDS);
            } catch (Exception ex) {
                throw new IOException("ALMA authentication timed out", ex);
            }
            if (!Boolean.TRUE.equals(authOk)) {
                throw new IOException("ALMA authentication rejected");
            }
        }

        initSessionAndRoles(projectXml, fileName);
    }

    /**
     * Uploads the project, then sets a placeholder context/role — each step awaited in order, not
     * fired fire-and-forget. AppraisalServer.handleAffectItem rejects ANY affectItem — including
     * plain XML, non-Raw signals that never touch context/roles — unless setContext/setRole were
     * sent first for this session (confirmed 2026-08-27 against the live server: "Context/roles
     * missing" even for a well-formed <aml:Event> item); the gate only checks the maps are
     * non-empty, not that they name a specific character, so one placeholder pair satisfies it for
     * all sends. initAacsSession's success path also wipes any existing sessionContext/sessionRoles
     * as part of loading the project, so firing all three as unawaited sends raced on a fast (e.g.
     * localhost) connection: setContext/setRole could land at the server before initAacsSession's
     * own wipe ran, and got silently discarded (confirmed 2026-08-30 against a local ALMA2025).
     * Awaiting each reply before sending the next removes that ordering dependency on timing.
     */
    private void initSessionAndRoles(String projectXml, String fileName) throws IOException, InterruptedException {
        sendAndAwait(envelope("initAacsSession", new JSONObject().put("xml", projectXml).put("fileName", fileName)));
        sendAndAwait(envelope("setContext", new JSONObject().put("context", "VisualSceneMaker SceneFlow session")));
        sendAndAwait(envelope("setRole", new JSONObject().put("name", "_vsm").put("role", "drives appraisal input from a VSM SceneFlow")));
    }

    public void reset(String projectXml, String fileName) throws IOException, InterruptedException {
        initSessionAndRoles(projectXml, fileName);
    }

    public void sendAppraisal(String character, String tag, String intensity, String elicitor) {
        sendAppraisal(character, tag, intensity, elicitor, null);
    }

    /**
     * @param hearer who this act is addressed to/heard by (dialogue-act invocations only; blank/null
     *               omits the attribute entirely rather than sending an empty one).
     */
    public void sendAppraisal(String character, String tag, String intensity, String elicitor, String hearer) {
        String category = EVENT_TAGS.contains(tag) ? "Event"
                : ACTION_TAGS.contains(tag) ? "Action"
                : OBJECT_TAGS.contains(tag) ? "Object" : null;
        if (category == null) {
            mLogger.warning("[alma] unknown appraisal tag: " + tag);
            return;
        }
        String hearerAttr = (hearer != null && !hearer.isBlank()) ? " hearer=\"" + esc(hearer) + "\"" : "";
        String xml = "<aml:Item xmlns:aml=\"xml.affect.de\"><aml:AffectInput>"
                + "<aml:Character name=\"" + esc(character) + "\"/>"
                + "<aml:" + category + " type=\"" + esc(tag) + "\" intensity=\"" + esc(intensity)
                + "\" elicitor=\"" + esc(elicitor) + "\"" + hearerAttr + "/>"
                + "</aml:AffectInput></aml:Item>";
        send(envelope("affectItem", new JSONObject().put("xml", xml)));
    }

    public void close() {
        WebSocket socket = mSocket;
        if (socket != null) {
            socket.sendClose(WebSocket.NORMAL_CLOSURE, "");
        }
    }

    private void handleMessage(String raw) {
        JSONObject envelope;
        try {
            envelope = new JSONObject(raw);
        } catch (Exception ex) {
            mLogger.warning("[alma] malformed message: " + raw);
            return;
        }
        String type = envelope.optString("type", "");
        JSONObject payload = envelope.optJSONObject("payload");

        switch (type) {
            case "authResult": {
                boolean ok = payload != null && payload.optBoolean("ok", false);
                CompletableFuture<Boolean> pending = mPendingAuth;
                if (pending != null) {
                    pending.complete(ok);
                }
                mListener.onAuthResult(ok);
                break;
            }
            case "affectInfo": {
                if (payload == null) break;
                JSONObject dominant = payload.optJSONObject("dominantEmotion");
                JSONObject mood = payload.optJSONObject("currentMood");
                JSONObject tendency = payload.optJSONObject("moodTendency");
                mListener.onAffectInfo(
                        payload.optString("character", ""),
                        dominant != null ? dominant.optString("type", "") : "",
                        dominant != null ? dominant.optDouble("intensity", 0.0) : 0.0,
                        mood != null ? mood.optString("name", "") : "",
                        tendency != null ? tendency.optString("name", "") : "");
                break;
            }
            case "emotionVector": {
                if (payload == null) break;
                String character = payload.optString("character", "");
                JSONArray emotions = payload.optJSONArray("emotions");
                List<String> active = new ArrayList<>();
                if (emotions != null) {
                    for (int i = 0; i < emotions.length(); i++) {
                        JSONObject e = emotions.getJSONObject(i);
                        if (e.optDouble("intensity", 0.0) > EMOTION_ACTIVE_THRESHOLD) {
                            active.add(e.optString("type", ""));
                        }
                    }
                }
                mListener.onEmotionVector(character, active);
                break;
            }
            case "ok":
            case "error": {
                String id = envelope.optString("id", null);
                CompletableFuture<JSONObject> pending = id != null ? mPendingReplies.get(id) : null;
                if (pending != null) {
                    pending.complete(envelope);
                } else if ("error".equals(type)) {
                    mListener.onError(payload != null ? payload.optString("msg", raw) : raw);
                }
                break;
            }
            default:
                break;
        }
    }

    /**
     * Sends a request and blocks until its correlated ok/error reply arrives (matched by the
     * envelope's "id"). See {@link #initSessionAndRoles} for why this ordering matters.
     */
    private JSONObject sendAndAwait(JSONObject request) throws IOException, InterruptedException {
        String id = request.getString("id");
        CompletableFuture<JSONObject> future = new CompletableFuture<>();
        mPendingReplies.put(id, future);
        try {
            send(request);
            JSONObject reply;
            try {
                reply = future.get(REPLY_TIMEOUT.toSeconds(), TimeUnit.SECONDS);
            } catch (TimeoutException | ExecutionException ex) {
                throw new IOException("Timed out waiting for reply to '" + request.optString("type") + "'", ex);
            }
            if ("error".equals(reply.optString("type"))) {
                JSONObject replyPayload = reply.optJSONObject("payload");
                String msg = replyPayload != null ? replyPayload.optString("msg", "unknown error") : "unknown error";
                throw new IOException("ALMA rejected '" + request.optString("type") + "': " + msg);
            }
            return reply;
        } finally {
            mPendingReplies.remove(id);
        }
    }

    private String fetchToken() throws IOException, InterruptedException {
        String form = "grant_type=client_credentials"
                + "&client_id=" + urlEncode(mClientId)
                + "&client_secret=" + urlEncode(mClientSecret);
        HttpRequest request = HttpRequest.newBuilder(URI.create(mTokenUrl))
                .header("Content-Type", "application/x-www-form-urlencoded")
                .POST(HttpRequest.BodyPublishers.ofString(form))
                .timeout(Duration.ofSeconds(10))
                .build();
        HttpResponse<String> response = mHttp.send(request, HttpResponse.BodyHandlers.ofString());
        if (response.statusCode() != 200) {
            throw new IOException("Keycloak token request failed: HTTP " + response.statusCode());
        }
        return new JSONObject(response.body()).getString("access_token");
    }

    private void send(JSONObject envelope) {
        WebSocket socket = mSocket;
        if (socket == null) {
            mLogger.warning("[alma] cannot send, not connected: " + envelope);
            return;
        }
        socket.sendText(envelope.toString(), true);
    }

    private static JSONObject envelope(String type, JSONObject payload) {
        return new JSONObject().put("type", type).put("id", UUID.randomUUID().toString()).put("payload", payload);
    }

    private static String urlEncode(String s) {
        return URLEncoder.encode(s == null ? "" : s, StandardCharsets.UTF_8);
    }

    private static String esc(String s) {
        return s == null ? "" : s.replace("&", "&amp;").replace("\"", "&quot;")
                .replace("<", "&lt;").replace(">", "&gt;");
    }

    private static boolean hasText(String s) {
        return s != null && !s.isBlank();
    }
}
