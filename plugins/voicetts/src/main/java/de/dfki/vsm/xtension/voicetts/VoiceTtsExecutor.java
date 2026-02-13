package de.dfki.vsm.xtension.voicetts;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import dev.dfki.affective.tts.client.*;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.SourceDataLine;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Base64;
import java.util.LinkedList;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class VoiceTtsExecutor extends ActivityExecutor {
    private ExecutorService worker = Executors.newSingleThreadExecutor();
    private final AtomicReference<TtsStreamClient> activeClient = new AtomicReference<>();
    private final Object audioLock = new Object();
    private final AtomicLong streamGeneration = new AtomicLong(0);

    private volatile String activeRequestId;
    private volatile SourceDataLine audioLine;
    private volatile int audioSampleRate = -1;
    private volatile int audioChannels = -1;
    private volatile boolean variableWritesEnabled = true;

    private static final Pattern NEXT_KV_PATTERN = Pattern.compile("\\s+[A-Za-z_][A-Za-z0-9_]*=");

    private String wsUrl;
    private String defaultMode;
    private String defaultCustomVoiceId;
    private String defaultVoice;
    private String defaultInstruct;
    private int defaultChunkMs;
    private int defaultVisemeHopMs;
    private int defaultGenerationStreamingIntervalMs;
    private String defaultCloneRefAudioPath;
    private String defaultCloneRefText;

    private String connectedVar;
    private String speakingVar;
    private String visemeVar;
    private String wordVar;
    private String wordFinalVar;
    private String errorVar;

    public VoiceTtsExecutor(final PluginConfig config, final RunTimeProject project) {
        super(config, project);
    }

    @Override
    public synchronized String marker(final long id) {
        return "$(" + id + ")";
    }

    @Override
    public void launch() {
        ensureWorker();
        variableWritesEnabled = true;
        wsUrl = configOrDefault("ws_url", "ws://127.0.0.1:8000/v1/tts/stream");
        defaultMode = configOrDefault("mode", "custom");
        defaultCustomVoiceId = configOrDefault("custom_voice_id", "cv1");
        defaultVoice = configOrDefault("voice", "Vivian");
        defaultInstruct = configOrDefault("instruct", "Calm and friendly");
        defaultChunkMs = parseIntOrDefault(configOrDefault("chunk_ms", "100"), 100);
        defaultVisemeHopMs = parseIntOrDefault(configOrDefault("viseme_hop_ms", "10"), 10);
        defaultGenerationStreamingIntervalMs = parseIntOrDefault(
                configOrDefault("generation_streaming_interval_ms", "350"), 350
        );
        defaultCloneRefAudioPath = configOrDefault("clone_ref_audio_path", "");
        defaultCloneRefText = configOrDefault("clone_ref_text", ".");

        connectedVar = configOrDefault("connectedVar", "tts_connected");
        speakingVar = configOrDefault("speakingVar", "tts_speaking");
        visemeVar = configOrDefault("visemeVar", "tts_viseme");
        wordVar = configOrDefault("wordVar", "tts_word");
        wordFinalVar = configOrDefault("wordFinalVar", "tts_word_final");
        errorVar = configOrDefault("errorVar", "tts_error");

        setBoolVar(connectedVar, false);
        setBoolVar(speakingVar, false);
        setBoolVar(wordFinalVar, false);
        setStringVar(errorVar, "");

        connectOnly();
        mLogger.message("VoiceTtsExecutor launched, ws_url=" + wsUrl);
    }

    @Override
    public void unload() {
        variableWritesEnabled = false;
        stopActiveSession("plugin_unload");
        disconnectOnly();
        closeAudioLine();
        shutdownWorker();
    }

    @Override
    public void execute(final AbstractActivity activity) {
        if (activity instanceof SpeechActivity) {
            handleSpeechActivity((SpeechActivity) activity);
            return;
        }

        final String actionName = safe(activity.getName()).toLowerCase();

        if ("stop".equals(actionName)) {
            stopActiveSession("stop_command");
            return;
        }

        if ("speak".equals(actionName) || !safe(activity.getText()).isBlank() || getFeature(activity, "text") != null) {
            speak(activity);
        }
    }

    private void handleSpeechActivity(final SpeechActivity speech) {
        final String textOnly = safe(speech.getTextOnly("${'")).trim();
        final LinkedList<String> timemarks = speech.getTimeMarks("${'");

        if (textOnly.isEmpty()) {
            for (String tm : timemarks) {
                mLogger.warning("Directly executing activity at timemark " + tm);
                mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
            }
            return;
        }

        speak(speech, textOnly, true);
    }

    private void speak(final AbstractActivity activity) {
        final String text = resolveText(activity);
        speak(activity, text, false);
    }

    private void speak(final AbstractActivity activity, final String resolvedText, final boolean blocking) {
        final String text = safe(resolvedText).trim();
        if (text.isBlank()) {
            setStringVar(errorVar, "No text provided for speak");
            return;
        }

        String normalizedMode = firstNonEmpty(getFeature(activity, "mode"), defaultMode).toLowerCase();
        if (!"custom".equals(normalizedMode) && !"design".equals(normalizedMode) && !"clone".equals(normalizedMode)) {
            normalizedMode = "custom";
            setStringVar(errorVar, "unsupported mode requested; falling back to custom");
        }
        final String mode = normalizedMode;
        final String customVoiceId = firstNonEmpty(getFeature(activity, "custom_voice_id"), defaultCustomVoiceId);
        final String voice = firstNonEmpty(getFeature(activity, "voice"), defaultVoice);
        final String instruct = firstNonEmpty(getFeature(activity, "instruct"), defaultInstruct);
        final double speed = parseDoubleOrDefault(getFeature(activity, "speed"), 1.0);
        final int chunkMs = parseIntOrDefault(getFeature(activity, "chunk_ms"), defaultChunkMs);
        final int visemeHopMs = parseIntOrDefault(getFeature(activity, "viseme_hop_ms"), defaultVisemeHopMs);
        final int generationStreamingIntervalMs = parseIntOrDefault(
                getFeature(activity, "generation_streaming_interval_ms"),
                defaultGenerationStreamingIntervalMs
        );
        final String refAudioB64Inline = firstNonEmpty(getFeature(activity, "ref_audio_b64"), "");
        final String refAudioPath = firstNonEmpty(getFeature(activity, "ref_audio_path"), defaultCloneRefAudioPath);
        final String refText = firstNonEmpty(getFeature(activity, "ref_text"), defaultCloneRefText);

        // Interrupt any running session immediately before queueing the next one.
        stopActiveSession("interrupt_previous");

        Runnable runSpeak = () -> runSpeakSession(
                mode, text, customVoiceId, voice, instruct, speed,
                chunkMs, visemeHopMs, generationStreamingIntervalMs,
                refAudioB64Inline, refAudioPath, refText
        );

        if (blocking) {
            runSpeak.run();
        } else {
            submitWorker(runSpeak);
        }
    }

    private void runSpeakSession(
            final String mode,
            final String text,
            final String customVoiceId,
            final String voice,
            final String instruct,
            final double speed,
            final int chunkMs,
            final int visemeHopMs,
            final int generationStreamingIntervalMs,
            final String refAudioB64Inline,
            final String refAudioPath,
            final String refText
    ) {
        setBoolVar(speakingVar, true);
        setStringVar(errorVar, "");

        final long generation = streamGeneration.get();
        final TtsStreamEventListener listener = new ListenerImpl(generation);
        final TtsStreamClient client = new TtsStreamClient(wsUrl, listener);
        activeClient.set(client);

        try {
            client.connect();
            setBoolVar(connectedVar, true);

            String effectiveRefAudioB64 = null;
            if ("clone".equals(mode)) {
                effectiveRefAudioB64 = resolveRefAudioB64(refAudioB64Inline, refAudioPath);
                if (effectiveRefAudioB64 == null || effectiveRefAudioB64.isBlank()) {
                    setStringVar(errorVar, "clone mode requires ref_audio_b64 or ref_audio_path");
                    setBoolVar(speakingVar, false);
                    return;
                }
            }

            final TtsStreamRequest request = TtsStreamRequest.builder()
                    .mode(mode)
                    .text(text)
                    .customVoiceId(customVoiceId)
                    .voice(voice)
                    .instruct(instruct)
                    .speed(speed)
                    .chunkMs(chunkMs)
                    .visemeHopMs(visemeHopMs)
                    .generationStreamingIntervalMs(generationStreamingIntervalMs)
                    .refAudioB64(effectiveRefAudioB64)
                    .refText("clone".equals(mode) ? refText : null)
                    .build();

            activeRequestId = request.requestId();
            client.startSession(request);

            boolean completed = client.awaitCompletion(Duration.ofMinutes(5));
            if (!completed) {
                setStringVar(errorVar, "TTS session timeout");
            }
        } catch (Exception ex) {
            setStringVar(errorVar, "TTS exception: " + ex.getMessage());
        } finally {
            try {
                client.close();
            } catch (Exception ignore) {
            }
            activeClient.compareAndSet(client, null);
            activeRequestId = null;
            setBoolVar(speakingVar, false);
        }
    }

    private void connectOnly() {
        submitWorker(() -> {
            try {
                TtsStreamClient existing = activeClient.get();
                if (existing != null) {
                    return;
                }
                long generation = streamGeneration.get();
                TtsStreamClient client = new TtsStreamClient(wsUrl, new ListenerImpl(generation));
                client.connect();
                activeClient.set(client);
                setBoolVar(connectedVar, true);
            } catch (Exception ex) {
                setStringVar(errorVar, "connect failed: " + ex.getMessage());
            }
        });
    }

    private void disconnectOnly() {
        try {
            TtsStreamClient client = activeClient.getAndSet(null);
            if (client != null) {
                try {
                    String req = activeRequestId;
                    if (req != null && !req.isBlank()) {
                        client.cancel(req, "disconnect");
                    }
                } catch (Exception ignore) {
                }
                try {
                    client.close();
                } catch (Exception ignore) {
                }
            }
            activeRequestId = null;
            setBoolVar(connectedVar, false);
            setBoolVar(speakingVar, false);
        } catch (Exception ex) {
            setStringVar(errorVar, "disconnect failed: " + ex.getMessage());
        }
    }

    private void stopActiveSession(final String reason) {
        streamGeneration.incrementAndGet();

        // Stop local playback first so audio halts immediately.
        closeAudioLineImmediate();

        TtsStreamClient client = activeClient.getAndSet(null);
        String req = activeRequestId;
        if (client != null && req != null && !req.isBlank()) {
            try {
                client.cancel(req, reason);
            } catch (Exception ignore) {
            }
            try {
                client.close();
            } catch (Exception ignore) {
            }
        }
        activeRequestId = null;
        setBoolVar(speakingVar, false);
    }

    private String configOrDefault(final String key, final String fallback) {
        String value = mConfig.getProperty(key);
        if (value == null || value.isBlank()) {
            return fallback;
        }
        return value;
    }

    private String safe(final String value) {
        return value == null ? "" : value;
    }

    private synchronized void ensureWorker() {
        if (worker == null || worker.isShutdown() || worker.isTerminated()) {
            worker = Executors.newSingleThreadExecutor();
        }
    }

    private synchronized void shutdownWorker() {
        if (worker != null && !worker.isShutdown()) {
            worker.shutdownNow();
        }
    }

    private void submitWorker(final Runnable job) {
        try {
            ensureWorker();
            worker.submit(job);
        } catch (RejectedExecutionException ex) {
            setStringVar(errorVar, "executor rejected task during lifecycle transition");
        }
    }

    private String resolveText(final AbstractActivity activity) {
        final String textParam = normalizeQuoted(firstNonEmpty(getFeature(activity, "text"), ""));
        final String raw = safe(activity.getText());
        final String fromRaw = extractTextFromRaw(raw);

        if (!fromRaw.isBlank()) {
            if (textParam.isBlank()) {
                return fromRaw;
            }
            // VSM may truncate unquoted key-values to one token ("Ich").
            if (!textParam.contains(" ") && fromRaw.length() > textParam.length()) {
                return fromRaw;
            }
            return textParam;
        }
        if (!textParam.isBlank()) {
            return textParam;
        }
        return normalizeQuoted(raw).trim();
    }

    private String extractTextFromRaw(final String rawInput) {
        final String raw = safe(rawInput).trim();
        if (raw.isBlank()) {
            return "";
        }

        final int idx = raw.toLowerCase().indexOf("text=");
        if (idx < 0) {
            return "";
        }

        String tail = raw.substring(idx + "text=".length()).trim();
        if (tail.isBlank()) {
            return "";
        }

        if (tail.startsWith("\"")) {
            int end = findClosingQuote(tail, '"');
            if (end > 0) {
                return tail.substring(1, end).trim();
            }
            return normalizeQuoted(tail);
        }

        if (tail.startsWith("'")) {
            int end = findClosingQuote(tail, '\'');
            if (end > 0) {
                return tail.substring(1, end).trim();
            }
            return normalizeQuoted(tail);
        }

        Matcher m = NEXT_KV_PATTERN.matcher(tail);
        if (m.find()) {
            tail = tail.substring(0, m.start()).trim();
        }
        return normalizeQuoted(tail);
    }

    private int findClosingQuote(final String text, final char quote) {
        for (int i = 1; i < text.length(); i++) {
            if (text.charAt(i) == quote && text.charAt(i - 1) != '\\') {
                return i;
            }
        }
        return -1;
    }

    private String normalizeQuoted(final String value) {
        String v = safe(value).trim();
        if ((v.startsWith("\"") && v.endsWith("\"")) || (v.startsWith("'") && v.endsWith("'"))) {
            if (v.length() >= 2) {
                v = v.substring(1, v.length() - 1).trim();
            }
        }
        if (v.endsWith("]")) {
            v = v.substring(0, v.length() - 1).trim();
        }
        return v;
    }

    private String getFeature(final AbstractActivity activity, final String key) {
        if (activity == null || key == null || key.isBlank()) {
            return null;
        }
        LinkedList<ActionFeature> features = activity.getFeatures();
        if (features == null) {
            return null;
        }
        for (ActionFeature feature : features) {
            if (feature != null && key.equals(feature.getKey())) {
                return feature.getVal(activity.getSubstitutions());
            }
        }
        return null;
    }

    private String firstNonEmpty(final String preferred, final String fallback) {
        String p = safe(preferred).trim();
        if (!p.isBlank()) {
            return p;
        }
        return safe(fallback).trim();
    }

    private double parseDoubleOrDefault(final String value, final double fallback) {
        try {
            if (value == null || value.isBlank()) {
                return fallback;
            }
            return Double.parseDouble(value);
        } catch (NumberFormatException e) {
            return fallback;
        }
    }

    private int parseIntOrDefault(final String value, final int fallback) {
        try {
            if (value == null || value.isBlank()) {
                return fallback;
            }
            return Integer.parseInt(value);
        } catch (NumberFormatException e) {
            return fallback;
        }
    }

    private String resolveRefAudioB64(final String inlineRefAudio, final String refAudioPath) {
        if (inlineRefAudio != null && !inlineRefAudio.isBlank()) {
            return inlineRefAudio.trim();
        }
        if (refAudioPath == null || refAudioPath.isBlank()) {
            return null;
        }
        try {
            byte[] bytes = Files.readAllBytes(Path.of(refAudioPath.trim()));
            return Base64.getEncoder().encodeToString(bytes);
        } catch (Exception ex) {
            setStringVar(errorVar, "failed to read clone ref audio path: " + ex.getMessage());
            return null;
        }
    }

    private void setStringVar(final String varName, final String value) {
        if (!variableWritesEnabled) {
            return;
        }
        if (varName == null || varName.isBlank()) {
            return;
        }
        try {
            if (mProject.hasVariable(varName)) {
                mProject.setVariable(varName, value == null ? "" : value);
            }
        } catch (Throwable ignore) {
            // Runtime may already be stopping and interpreter context can be unavailable.
        }
    }

    private void setBoolVar(final String varName, final boolean value) {
        if (!variableWritesEnabled) {
            return;
        }
        if (varName == null || varName.isBlank()) {
            return;
        }
        try {
            if (mProject.hasVariable(varName)) {
                mProject.setVariable(varName, value);
            }
        } catch (Throwable ignore) {
            // Runtime may already be stopping and interpreter context can be unavailable.
        }
    }

    private void playAudioChunk(final AudioChunkEvent event) {
        byte[] pcm = event.pcmBytes();
        if (pcm == null || pcm.length == 0) {
            return;
        }
        int sampleRate = event.clock() == null ? 24000 : event.clock().sampleRate();
        int channels = event.channels() <= 0 ? 1 : event.channels();

        synchronized (audioLock) {
            try {
                if (audioLine == null || audioSampleRate != sampleRate || audioChannels != channels) {
                    closeAudioLineLocked();
                    AudioFormat format = new AudioFormat(sampleRate, 16, channels, true, false);
                    DataLine.Info info = new DataLine.Info(SourceDataLine.class, format);
                    SourceDataLine line = (SourceDataLine) AudioSystem.getLine(info);
                    line.open(format);
                    line.start();
                    audioLine = line;
                    audioSampleRate = sampleRate;
                    audioChannels = channels;
                }
                audioLine.write(pcm, 0, pcm.length);
            } catch (Exception ex) {
                setStringVar(errorVar, "audio playback failed: " + ex.getMessage());
                closeAudioLineLocked();
            }
        }
    }

    private void closeAudioLine() {
        synchronized (audioLock) {
            closeAudioLineLocked();
        }
    }

    private void closeAudioLineImmediate() {
        synchronized (audioLock) {
            closeAudioLineLockedImmediate();
        }
    }

    private void closeAudioLineLocked() {
        if (audioLine != null) {
            try {
                audioLine.drain();
                audioLine.stop();
            } catch (Exception ignore) {
            }
            try {
                audioLine.close();
            } catch (Exception ignore) {
            }
            audioLine = null;
        }
        audioSampleRate = -1;
        audioChannels = -1;
    }

    private void closeAudioLineLockedImmediate() {
        if (audioLine != null) {
            try {
                audioLine.stop();
                audioLine.flush();
            } catch (Exception ignore) {
            }
            try {
                audioLine.close();
            } catch (Exception ignore) {
            }
            audioLine = null;
        }
        audioSampleRate = -1;
        audioChannels = -1;
    }

    private final class ListenerImpl implements TtsStreamEventListener {
        private final long generation;

        private ListenerImpl(final long generation) {
            this.generation = generation;
        }

        private boolean isCurrentGeneration() {
            return generation == streamGeneration.get();
        }

        @Override
        public void onSessionStarted(final SessionStartedEvent event) {
            if (!isCurrentGeneration()) {
                return;
            }
            setBoolVar(connectedVar, true);
            if (event.selectedCustomVoiceId() != null && !event.selectedCustomVoiceId().isBlank()) {
                mLogger.message("voicetts session started with custom_voice_id=" + event.selectedCustomVoiceId());
            }
        }

        @Override
        public void onAudioChunk(final AudioChunkEvent event) {
            if (!isCurrentGeneration()) {
                return;
            }
            playAudioChunk(event);
        }

        @Override
        public void onViseme(final VisemeEvent event) {
            if (!isCurrentGeneration()) {
                return;
            }
            setStringVar(visemeVar, event.viseme());
        }

        @Override
        public void onWordProvisional(final WordTimingEvent event) {
            if (!isCurrentGeneration()) {
                return;
            }
            setStringVar(wordVar, event.word());
            setBoolVar(wordFinalVar, false);
        }

        @Override
        public void onWordFinal(final WordTimingEvent event) {
            if (!isCurrentGeneration()) {
                return;
            }
            setStringVar(wordVar, event.word());
            setBoolVar(wordFinalVar, true);
        }

        @Override
        public void onSessionCompleted(final SessionCompletedEvent event) {
            if (!isCurrentGeneration()) {
                return;
            }
            setBoolVar(speakingVar, false);
            closeAudioLine();
        }

        @Override
        public void onSessionError(final SessionErrorEvent event) {
            if (!isCurrentGeneration()) {
                return;
            }
            setStringVar(errorVar, event.code() + ": " + event.message());
            setBoolVar(speakingVar, false);
            closeAudioLine();
        }

        @Override
        public void onTransportError(final Throwable error) {
            if (!isCurrentGeneration()) {
                return;
            }
            setStringVar(errorVar, "transport: " + error.getMessage());
            setBoolVar(connectedVar, false);
            setBoolVar(speakingVar, false);
            closeAudioLine();
        }
    }
}
