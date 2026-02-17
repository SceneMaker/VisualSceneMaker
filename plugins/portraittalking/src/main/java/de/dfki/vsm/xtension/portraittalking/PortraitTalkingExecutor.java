package de.dfki.vsm.xtension.portraittalking;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.voicetts.VoiceTtsExecutor;
import dev.dfki.affective.tts.client.*;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.WebSocket;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

/**
 * Extends VoiceTtsExecutor to forward viseme events to a portrait WebSocket relay,
 * driving a browser-based talking portrait renderer.
 */
public class PortraitTalkingExecutor extends VoiceTtsExecutor {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private String portraitWsUrl;
    private volatile WebSocket portraitWs;

    public PortraitTalkingExecutor(final PluginConfig config, final RunTimeProject project) {
        super(config, project);
    }

    @Override
    public void launch() {
        super.launch();
        portraitWsUrl = mConfig.getProperty("portrait_ws_url");
        if (portraitWsUrl == null || portraitWsUrl.isBlank()) {
            portraitWsUrl = "ws://127.0.0.1:8000/v1/portrait/stream";
        }
        connectPortrait();
    }

    @Override
    public void unload() {
        disconnectPortrait();
        super.unload();
    }

    @Override
    protected TtsStreamEventListener createSessionListener(long generation) {
        TtsStreamEventListener base = super.createSessionListener(generation);
        return new PortraitForwardingListener(base, generation);
    }

    private void connectPortrait() {
        try {
            HttpClient client = HttpClient.newHttpClient();
            CompletableFuture<WebSocket> future = client.newWebSocketBuilder()
                    .buildAsync(URI.create(portraitWsUrl), new WebSocket.Listener() {
                        @Override
                        public void onOpen(WebSocket webSocket) {
                            WebSocket.Listener.super.onOpen(webSocket);
                        }

                        @Override
                        public CompletionStage<?> onText(WebSocket webSocket, CharSequence data, boolean last) {
                            return WebSocket.Listener.super.onText(webSocket, data, last);
                        }

                        @Override
                        public void onError(WebSocket webSocket, Throwable error) {
                            mLogger.warning("[portrait] WebSocket error: " + error.getMessage());
                        }
                    });
            portraitWs = future.join();
            // Register as controller
            String register = MAPPER.writeValueAsString(Map.of(
                    "type", "portrait.register",
                    "role", "controller"
            ));
            portraitWs.sendText(register, true);
            mLogger.message("[portrait] connected to " + portraitWsUrl);
        } catch (Exception ex) {
            mLogger.warning("[portrait] connect failed: " + ex.getMessage());
            portraitWs = null;
        }
    }

    private void disconnectPortrait() {
        WebSocket ws = portraitWs;
        portraitWs = null;
        if (ws != null) {
            try {
                ws.sendClose(WebSocket.NORMAL_CLOSURE, "unload").join();
            } catch (Exception ignore) {
            }
        }
    }

    private void sendPortraitJson(Map<String, Object> msg) {
        WebSocket ws = portraitWs;
        if (ws == null) return;
        try {
            String json = MAPPER.writeValueAsString(msg);
            ws.sendText(json, true);
        } catch (Exception ex) {
            mLogger.warning("[portrait] send failed: " + ex.getMessage());
        }
    }

    /**
     * Wrapping listener that delegates all events to the base listener
     * and additionally forwards viseme + session events to the portrait relay.
     */
    private class PortraitForwardingListener implements TtsStreamEventListener {
        private final TtsStreamEventListener base;
        private final long generation;

        PortraitForwardingListener(TtsStreamEventListener base, long generation) {
            this.base = base;
            this.generation = generation;
        }

        @Override
        public void onSessionStarted(SessionStartedEvent event) {
            base.onSessionStarted(event);
            sendPortraitJson(Map.of("type", "portrait.session_start"));
        }

        @Override
        public void onAudioChunk(AudioChunkEvent event) {
            base.onAudioChunk(event);
        }

        @Override
        public void onViseme(VisemeEvent event) {
            base.onViseme(event);
            // Forward viseme to portrait relay
            Map<String, Object> msg = new HashMap<>();
            msg.put("type", "viseme.frame");
            msg.put("viseme", event.viseme());
            msg.put("confidence", event.confidence());
            msg.put("weights", event.weights());
            if (event.clock() != null) {
                msg.put("clock_ms", event.clock().startMs());
            }
            sendPortraitJson(msg);
        }

        @Override
        public void onWordProvisional(WordTimingEvent event) {
            base.onWordProvisional(event);
        }

        @Override
        public void onWordFinal(WordTimingEvent event) {
            base.onWordFinal(event);
        }

        @Override
        public void onSessionCompleted(SessionCompletedEvent event) {
            base.onSessionCompleted(event);
            sendPortraitJson(Map.of("type", "portrait.session_end"));
        }

        @Override
        public void onSessionError(SessionErrorEvent event) {
            base.onSessionError(event);
            sendPortraitJson(Map.of("type", "portrait.session_end"));
        }

        @Override
        public void onTransportError(Throwable error) {
            base.onTransportError(error);
        }
    }
}
