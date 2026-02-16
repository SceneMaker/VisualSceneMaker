package de.dfki.vsm.xtension.voicetts;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import dev.dfki.affective.tts.client.*;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioFileFormat;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.SourceDataLine;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Base64;
import java.util.LinkedList;
import java.util.Locale;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class VoiceTtsExecutor extends ActivityExecutor {
    private ExecutorService worker = Executors.newSingleThreadExecutor();
    private final AtomicReference<TtsStreamClient> activeClient = new AtomicReference<>();
    private final Object audioLock = new Object();
    private final AtomicLong streamGeneration = new AtomicLong(0);
    private final AtomicLong speakSeq = new AtomicLong(0);

    private volatile String activeRequestId;
    private volatile String activeSpeakTag;
    private volatile boolean isSpeaking = false;
    private volatile String lastChunkRequestId = "";
    private volatile long lastChunkEndSample = -1L;
    private volatile SourceDataLine audioLine;
    private volatile int audioSampleRate = -1;
    private volatile int audioChannels = -1;
    private volatile boolean playbackActive = false;
    private volatile int playbackStartCount = 0;
    private volatile long playbackStartEpochMs = 0L;
    private volatile long playbackBytesWritten = 0L;
    private volatile long playbackChunksWritten = 0L;
    private volatile String playbackRequestId = "";
    private volatile long playbackFirstStartSample = -1L;
    private volatile long playbackLastEndSample = -1L;
    private volatile ByteArrayOutputStream playbackPcmBuffer;
    private volatile int playbackDumpSampleRate = -1;
    private volatile int playbackDumpChannels = -1;
    private volatile boolean variableWritesEnabled = true;
    private volatile String lastSpeakKey = "";
    private volatile long lastSpeakNanos = 0L;
    /** Client-side pre-buffer: accumulate this much audio before starting playback.
     *  Must exceed the TTS model's generation interval (~350-400ms) to prevent
     *  buffer underruns, but kept low to minimize time-to-first-audio. */
    private static final double AUDIO_PREBUFFER_SECONDS = 1.0;
    private volatile ByteArrayOutputStream audioPrebuffer;
    private volatile int audioPrebufferTarget;
    private volatile boolean audioPlaybackStarted;
    private final Object markerLock = new Object();
    private final LinkedList<String> pendingTimemarks = new LinkedList<>();
    private volatile ScheduledExecutorService timemarkScheduler;

    private static final Pattern NEXT_KV_PATTERN = Pattern.compile("\\s+[A-Za-z_][A-Za-z0-9_]*=");

    private String wsUrl;
    private String defaultMode;
    private String defaultCustomVoiceId;
    private String defaultVoice;
    private String defaultInstruct;
    private String defaultGenerationMode;
    private int defaultChunkMs;
    private int defaultVisemeHopMs;
    private int defaultGenerationStreamingIntervalMs;
    private String defaultCloneRefAudioPath;
    private String defaultCloneRefText;
    private boolean debugDumpWavEnabled;
    private String debugDumpDir;

    private String connectedVar;
    private String speakingVar;
    private String visemeVar;
    private String wordVar;
    private String wordFinalVar;
    private String errorVar;
    private String debugSeqVar;
    private String debugReqVar;
    private String debugStateVar;

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
        defaultGenerationMode = sanitizeGenerationMode(configOrDefault("generation_mode", "full_utterance"));
        defaultChunkMs = parseIntOrDefault(configOrDefault("chunk_ms", "100"), 100);
        defaultVisemeHopMs = parseIntOrDefault(configOrDefault("viseme_hop_ms", "10"), 10);
        defaultGenerationStreamingIntervalMs = parseIntOrDefault(
                configOrDefault("generation_streaming_interval_ms", "350"), 350
        );
        defaultCloneRefAudioPath = configOrDefault("clone_ref_audio_path", "");
        defaultCloneRefText = configOrDefault("clone_ref_text", ".");
        debugDumpWavEnabled = Boolean.parseBoolean(configOrDefault("debug_dump_wav_enabled", "true"));
        debugDumpDir = configOrDefault("debug_dump_wav_dir", "/tmp/voicetts-dumps");

        connectedVar = configOrDefault("connectedVar", "tts_connected");
        speakingVar = configOrDefault("speakingVar", "tts_speaking");
        visemeVar = configOrDefault("visemeVar", "tts_viseme");
        wordVar = configOrDefault("wordVar", "tts_word");
        wordFinalVar = configOrDefault("wordFinalVar", "tts_word_final");
        errorVar = configOrDefault("errorVar", "tts_error");
        debugSeqVar = configOrDefault("debugSeqVar", "tts_debug_seq");
        debugReqVar = configOrDefault("debugReqVar", "tts_debug_request_id");
        debugStateVar = configOrDefault("debugStateVar", "tts_debug_state");

        setBoolVar(connectedVar, false);
        setBoolVar(speakingVar, false);
        setBoolVar(wordFinalVar, false);
        setStringVar(errorVar, "");
        setStringVar(debugSeqVar, "0");
        setStringVar(debugReqVar, "");
        setStringVar(debugStateVar, "launch");

        connectOnly();
        mLogger.message("VoiceTtsExecutor launched, ws_url=" + wsUrl);
    }

    @Override
    public void unload() {
        variableWritesEnabled = false;
        stopActiveSession("plugin_unload");
        disconnectOnly();
        closeAudioLine();
        shutdownTimemarkScheduler();
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

        if ("speak".equals(actionName)) {
            // PlayScene speak actions should block until the utterance is complete.
            speak(activity, resolveText(activity), true);
            return;
        }

        if (!safe(activity.getText()).isBlank() || getFeature(activity, "text") != null) {
            speak(activity);
        }
    }

    private void handleSpeechActivity(final SpeechActivity speech) {
        final String textOnly = safe(speech.getTextOnly("$(")).trim();
        final LinkedList<String> timemarks = speech.getTimeMarks("$(");

        if (textOnly.isEmpty()) {
            for (String tm : timemarks) {
                mLogger.warning("Directly executing activity at timemark " + tm);
                dispatchTimemark(tm);
            }
            return;
        }

        preparePendingTimemarks(timemarks);
        mLogger.message("[voicetts] source=speech actor=" + safe(speech.getActor())
                + " text=\"" + textOnly + "\"");
        speak(speech, textOnly, true);
        dispatchAllRemainingTimemarks();
        clearPendingTimemarks();
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
        final String generationMode = sanitizeGenerationMode(
                firstNonEmpty(getFeature(activity, "generation_mode"), defaultGenerationMode)
        );
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
        final long seq = speakSeq.incrementAndGet();
        final String tag = "spk#" + seq;
        final String key = safe(activity.getActor()) + "|" + mode + "|" + customVoiceId + "|" + text;
        final long now = System.nanoTime();
        final String prevKey = lastSpeakKey;
        final long prevNanos = lastSpeakNanos;
        final long deltaMs = (now - prevNanos) / 1_000_000L;
        final boolean nearDuplicate = key.equals(prevKey) && deltaMs >= 0 && deltaMs < 1500;
        lastSpeakKey = key;
        lastSpeakNanos = now;

        mLogger.message("[voicetts] " + tag + " trigger actor=" + safe(activity.getActor())
                + " mode=" + mode + " voice_id=" + customVoiceId
                + " generation_mode=" + generationMode
                + " blocking=" + blocking + " text=\"" + text + "\"");
        if (nearDuplicate) {
            mLogger.warning("[voicetts] " + tag + " near-duplicate suppressed dt_ms=" + deltaMs);
            setStringVar(debugStateVar, "suppressed_near_duplicate");
            return;
        }
        if (isSpeaking && key.equals(prevKey) && deltaMs >= 0 && deltaMs < 5000) {
            mLogger.warning("[voicetts] " + tag + " duplicate while speaking suppressed dt_ms=" + deltaMs);
            setStringVar(debugStateVar, "suppressed_while_speaking");
            return;
        }
        setStringVar(debugSeqVar, Long.toString(seq));
        setStringVar(debugStateVar, "triggered");

        // Interrupt any running session immediately before queueing the next one.
        stopActiveSession("interrupt_previous");

        Runnable runSpeak = () -> runSpeakSession(
                tag, generationMode,
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
            final String speakTag,
            final String generationMode,
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
        activeSpeakTag = speakTag;
        isSpeaking = true;
        resetPlaybackTracking();
        lastChunkRequestId = "";
        lastChunkEndSample = -1L;
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

            TtsStreamRequest.Builder requestBuilder = TtsStreamRequest.builder()
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
                    .refText("clone".equals(mode) ? refText : null);

            requestBuilder = applyGenerationModeIfSupported(requestBuilder, generationMode);
            final TtsStreamRequest request = requestBuilder.build();

            activeRequestId = request.requestId();
            mLogger.message("[voicetts] " + speakTag + " session.start request_id=" + activeRequestId);
            setStringVar(debugReqVar, activeRequestId);
            setStringVar(debugStateVar, "session_start");
            client.startSession(request);

            boolean completed = client.awaitCompletion(Duration.ofMinutes(5));
            if (!completed) {
                setStringVar(errorVar, "TTS session timeout");
                setStringVar(debugStateVar, "timeout");
            }
        } catch (Exception ex) {
            setStringVar(errorVar, "TTS exception: " + ex.getMessage());
            setStringVar(debugStateVar, "exception");
        } finally {
            try {
                client.close();
            } catch (Exception ignore) {
            }
            activeClient.compareAndSet(client, null);
            activeRequestId = null;
            activeSpeakTag = null;
            isSpeaking = false;
            lastChunkRequestId = "";
            lastChunkEndSample = -1L;
            setBoolVar(speakingVar, false);
            setStringVar(debugStateVar, "idle");
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

        // Cancel any pending scheduled timemarks before stopping audio.
        shutdownTimemarkScheduler();

        // Stop local playback first so audio halts immediately.
        closeAudioLineImmediate();

        TtsStreamClient client = activeClient.getAndSet(null);
        String req = activeRequestId;
        if (client != null && req != null && !req.isBlank()) {
            mLogger.message("[voicetts] cancel request_id=" + req + " reason=" + reason);
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
        isSpeaking = false;
        resetPlaybackTracking();
        lastChunkRequestId = "";
        lastChunkEndSample = -1L;
        setBoolVar(speakingVar, false);
        setStringVar(debugStateVar, "stopped:" + reason);
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

    private void preparePendingTimemarks(final LinkedList<String> timemarks) {
        synchronized (markerLock) {
            pendingTimemarks.clear();
            if (timemarks != null) {
                pendingTimemarks.addAll(timemarks);
            }
        }
    }

    private void clearPendingTimemarks() {
        synchronized (markerLock) {
            pendingTimemarks.clear();
        }
    }

    private void dispatchNextTimemark() {
        final String next;
        synchronized (markerLock) {
            if (pendingTimemarks.isEmpty()) {
                return;
            }
            next = pendingTimemarks.removeFirst();
        }
        dispatchTimemark(next);
    }

    private void dispatchAllRemainingTimemarks() {
        while (true) {
            final String next;
            synchronized (markerLock) {
                if (pendingTimemarks.isEmpty()) {
                    return;
                }
                next = pendingTimemarks.removeFirst();
            }
            dispatchTimemark(next);
        }
    }

    private void dispatchTimemark(final String timemark) {
        final String marker = safe(timemark).trim();
        if (marker.isBlank()) {
            return;
        }
        try {
            if (mProject.getRunTimePlayer().getActivityScheduler().hasMarker(marker)) {
                mLogger.message("[voicetts] executing timemark " + marker);
                mProject.getRunTimePlayer().getActivityScheduler().handle(marker);
            } else {
                mLogger.warning("[voicetts] marker already processed " + marker);
            }
        } catch (Exception ex) {
            mLogger.warning("[voicetts] timemark dispatch failed " + marker + ": " + ex.getMessage());
        }
    }

    private void scheduleNextTimemarkAtPlaybackPosition(final StreamClock clock) {
        final String next;
        synchronized (markerLock) {
            if (pendingTimemarks.isEmpty()) {
                return;
            }
            next = pendingTimemarks.removeFirst();
        }
        if (next == null || next.isBlank()) {
            return;
        }

        // Determine how far into playback we are using the audio line's hardware position.
        double targetMs = (clock != null) ? clock.startMs() : -1.0;
        double playbackMs = getPlaybackPositionMs();

        if (targetMs < 0 || playbackMs < 0 || playbackMs >= targetMs) {
            // Already past this point or no clock data — dispatch immediately.
            dispatchTimemark(next);
            return;
        }

        long delayMs = Math.max(1L, (long) (targetMs - playbackMs));
        mLogger.message("[voicetts] scheduling timemark " + next
                + " in " + delayMs + "ms (target=" + String.format("%.0f", targetMs)
                + "ms playback=" + String.format("%.0f", playbackMs) + "ms)");
        ensureTimemarkScheduler();
        final long gen = streamGeneration.get();
        try {
            timemarkScheduler.schedule(() -> {
                if (gen == streamGeneration.get()) {
                    dispatchTimemark(next);
                }
            }, delayMs, TimeUnit.MILLISECONDS);
        } catch (RejectedExecutionException ex) {
            // Scheduler shut down — dispatch immediately as fallback.
            dispatchTimemark(next);
        }
    }

    private double getPlaybackPositionMs() {
        synchronized (audioLock) {
            SourceDataLine line = audioLine;
            if (line == null) {
                return -1.0;
            }
            return line.getMicrosecondPosition() / 1000.0;
        }
    }

    private synchronized void ensureTimemarkScheduler() {
        if (timemarkScheduler == null || timemarkScheduler.isShutdown()) {
            timemarkScheduler = Executors.newSingleThreadScheduledExecutor();
        }
    }

    private synchronized void shutdownTimemarkScheduler() {
        if (timemarkScheduler != null && !timemarkScheduler.isShutdown()) {
            timemarkScheduler.shutdownNow();
        }
    }

    private String sanitizeGenerationMode(final String mode) {
        final String normalized = safe(mode).trim().toLowerCase(Locale.ROOT);
        if ("stream".equals(normalized) || "streaming".equals(normalized)) {
            return "streaming";
        }
        return "full_utterance";
    }

    private TtsStreamRequest.Builder applyGenerationModeIfSupported(
            final TtsStreamRequest.Builder builder,
            final String generationMode
    ) {
        try {
            final java.lang.reflect.Method method = builder.getClass().getMethod("generationMode", String.class);
            method.invoke(builder, generationMode);
        } catch (NoSuchMethodException ignored) {
            mLogger.warning("[voicetts] TtsStreamRequest.Builder has no generationMode(String); "
                    + "server default generation mode will be used");
        } catch (Exception ex) {
            mLogger.warning("[voicetts] failed to set generation mode '" + generationMode + "': " + ex.getMessage());
        }
        return builder;
    }

    private void setStringVar(final String varName, final String value) {
        if (!variableWritesEnabled) {
            return;
        }
        if (varName == null || varName.isBlank()) {
            return;
        }
        try {
            mProject.setVariable(varName, value == null ? "" : value);
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
            mProject.setVariable(varName, value);
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
                    // 4-second hardware buffer for large cushion
                    int bufferBytes = sampleRate * 2 * channels * 4;
                    line.open(format, bufferBytes);
                    // Don't start yet — prebuffer first
                    audioLine = line;
                    audioSampleRate = sampleRate;
                    audioChannels = channels;
                    audioPlaybackStarted = false;
                    audioPrebuffer = new ByteArrayOutputStream();
                    audioPrebufferTarget = (int) (sampleRate * 2 * channels * AUDIO_PREBUFFER_SECONDS);
                }
                markPlaybackStart(event);

                // Phase 1: accumulate into prebuffer
                if (!audioPlaybackStarted) {
                    audioPrebuffer.write(pcm);
                    // Track bytes/chunks even during prebuffering
                    playbackBytesWritten += pcm.length;
                    playbackChunksWritten += 1;
                    if (playbackPcmBuffer != null) {
                        playbackPcmBuffer.write(pcm, 0, pcm.length);
                    }
                    if (event.clock() != null) {
                        long end = event.clock().endSample();
                        if (end > playbackLastEndSample) {
                            playbackLastEndSample = end;
                        }
                    }
                    if (audioPrebuffer.size() >= audioPrebufferTarget) {
                        byte[] buffered = audioPrebuffer.toByteArray();
                        audioPrebuffer = null;
                        audioLine.write(buffered, 0, buffered.length);
                        audioLine.start();
                        audioPlaybackStarted = true;
                        mLogger.message("[voicetts] " + safe(activeSpeakTag)
                                + " audio.prebuffer_flush ms="
                                + String.format("%.0f", buffered.length * 1000.0 / (sampleRate * 2 * channels))
                                + " chunks=" + playbackChunksWritten);
                    }
                    return;
                }

                // Phase 2: stream directly to audio line
                audioLine.write(pcm, 0, pcm.length);
                playbackBytesWritten += pcm.length;
                playbackChunksWritten += 1;
                if (playbackPcmBuffer != null) {
                    playbackPcmBuffer.write(pcm, 0, pcm.length);
                }
                if (event.clock() != null) {
                    long end = event.clock().endSample();
                    if (end > playbackLastEndSample) {
                        playbackLastEndSample = end;
                    }
                }
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
        // Flush any remaining prebuffer (short utterances that didn't reach target)
        if (!audioPlaybackStarted && audioPrebuffer != null && audioLine != null) {
            byte[] remaining = audioPrebuffer.toByteArray();
            audioPrebuffer = null;
            if (remaining.length > 0) {
                audioLine.write(remaining, 0, remaining.length);
                audioLine.start();
                audioPlaybackStarted = true;
            }
        }
        audioPrebuffer = null;
        markPlaybackEnd("drain");
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
        audioPrebuffer = null;
        markPlaybackEnd("immediate");
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
            mLogger.message("[voicetts] " + safe(activeSpeakTag) + " session.started request_id="
                    + event.requestId() + " session_id=" + event.sessionId());
            setBoolVar(connectedVar, true);
            setStringVar(debugStateVar, "session_started");
            if (event.selectedCustomVoiceId() != null && !event.selectedCustomVoiceId().isBlank()) {
                mLogger.message("voicetts session started with custom_voice_id=" + event.selectedCustomVoiceId());
            }
        }

        @Override
        public void onAudioChunk(final AudioChunkEvent event) {
            if (!isCurrentGeneration()) {
                return;
            }
            if (event.clock() != null) {
                mLogger.message("[voicetts] " + safe(activeSpeakTag)
                        + " audio.chunk request_id=" + safe(event.requestId())
                        + " samples=" + event.clock().startSample() + ".." + event.clock().endSample()
                        + " bytes=" + (event.pcmBytes() == null ? 0 : event.pcmBytes().length)
                        + " channels=" + event.channels());
            } else {
                mLogger.message("[voicetts] " + safe(activeSpeakTag)
                        + " audio.chunk request_id=" + safe(event.requestId())
                        + " samples=unknown"
                        + " bytes=" + (event.pcmBytes() == null ? 0 : event.pcmBytes().length)
                        + " channels=" + event.channels());
            }
            if (event.requestId() != null && !event.requestId().isBlank()) {
                if (event.requestId().equals(lastChunkRequestId) && event.clock() != null) {
                    long start = event.clock().startSample();
                    long end = event.clock().endSample();
                    if (lastChunkEndSample >= 0 && start < lastChunkEndSample) {
                        mLogger.warning("[voicetts] dropped overlapping audio chunk request_id="
                                + event.requestId() + " start=" + start + " last_end=" + lastChunkEndSample);
                        return;
                    }
                    if (end > lastChunkEndSample) {
                        lastChunkEndSample = end;
                    }
                } else {
                    lastChunkRequestId = event.requestId();
                    lastChunkEndSample = event.clock() != null ? event.clock().endSample() : -1L;
                }
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
            scheduleNextTimemarkAtPlaybackPosition(event.clock());
        }

        @Override
        public void onSessionCompleted(final SessionCompletedEvent event) {
            if (!isCurrentGeneration()) {
                return;
            }
            mLogger.message("[voicetts] " + safe(activeSpeakTag) + " session.completed request_id="
                    + event.requestId() + " session_id=" + event.sessionId());
            setBoolVar(speakingVar, false);
            setStringVar(debugStateVar, "session_completed");
            dispatchAllRemainingTimemarks();
            closeAudioLine();
        }

        @Override
        public void onSessionError(final SessionErrorEvent event) {
            if (!isCurrentGeneration()) {
                return;
            }
            mLogger.warning("[voicetts] " + safe(activeSpeakTag) + " session.error request_id="
                    + event.requestId() + " session_id=" + event.sessionId()
                    + " code=" + event.code());
            setStringVar(errorVar, event.code() + ": " + event.message());
            setBoolVar(speakingVar, false);
            setStringVar(debugStateVar, "session_error:" + event.code());
            clearPendingTimemarks();
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
            setStringVar(debugStateVar, "transport_error");
            clearPendingTimemarks();
            closeAudioLine();
        }
    }

    private void resetPlaybackTracking() {
        synchronized (audioLock) {
            playbackActive = false;
            playbackStartCount = 0;
            playbackStartEpochMs = 0L;
            playbackBytesWritten = 0L;
            playbackChunksWritten = 0L;
            playbackRequestId = "";
            playbackFirstStartSample = -1L;
            playbackLastEndSample = -1L;
            playbackPcmBuffer = null;
            playbackDumpSampleRate = -1;
            playbackDumpChannels = -1;
        }
    }

    private void markPlaybackStart(final AudioChunkEvent event) {
        if (playbackActive) {
            return;
        }
        playbackActive = true;
        playbackStartCount += 1;
        playbackStartEpochMs = System.currentTimeMillis();
        playbackRequestId = safe(event.requestId());
        playbackBytesWritten = 0L;
        playbackChunksWritten = 0L;
        playbackFirstStartSample = event.clock() != null ? event.clock().startSample() : -1L;
        playbackLastEndSample = event.clock() != null ? event.clock().endSample() : -1L;
        playbackDumpSampleRate = event.clock() == null ? 24000 : event.clock().sampleRate();
        playbackDumpChannels = event.channels() <= 0 ? 1 : event.channels();
        playbackPcmBuffer = new ByteArrayOutputStream();

        setStringVar(debugStateVar, "audio_playback_start#" + playbackStartCount);
        mLogger.message("[voicetts] " + safe(activeSpeakTag)
                + " audio.playback_start n=" + playbackStartCount
                + " request_id=" + playbackRequestId
                + " sample_start=" + playbackFirstStartSample
                + " sample_end=" + playbackLastEndSample);
    }

    private void markPlaybackEnd(final String reason) {
        if (!playbackActive) {
            return;
        }
        final long now = System.currentTimeMillis();
        final long elapsedMs = playbackStartEpochMs > 0L ? (now - playbackStartEpochMs) : -1L;
        String dumpedPath = dumpPlaybackWav();
        setStringVar(debugStateVar, "audio_playback_end#" + playbackStartCount);
        mLogger.message("[voicetts] " + safe(activeSpeakTag)
                + " audio.playback_end n=" + playbackStartCount
                + " reason=" + reason
                + " request_id=" + playbackRequestId
                + " elapsed_ms=" + elapsedMs
                + " chunks=" + playbackChunksWritten
                + " bytes=" + playbackBytesWritten
                + " sample_start=" + playbackFirstStartSample
                + " sample_end=" + playbackLastEndSample
                + (dumpedPath.isBlank() ? "" : " wav_file=" + dumpedPath));
        playbackActive = false;
        playbackRequestId = "";
        playbackStartEpochMs = 0L;
        playbackBytesWritten = 0L;
        playbackChunksWritten = 0L;
        playbackFirstStartSample = -1L;
        playbackLastEndSample = -1L;
        playbackPcmBuffer = null;
        playbackDumpSampleRate = -1;
        playbackDumpChannels = -1;
    }

    private String dumpPlaybackWav() {
        if (!debugDumpWavEnabled || playbackPcmBuffer == null) {
            return "";
        }
        byte[] pcm = playbackPcmBuffer.toByteArray();
        if (pcm.length == 0 || playbackDumpSampleRate <= 0 || playbackDumpChannels <= 0) {
            return "";
        }
        try {
            final Path dir = Path.of(debugDumpDir);
            Files.createDirectories(dir);
            final String req = sanitizeFileToken(playbackRequestId);
            final String tag = sanitizeFileToken(activeSpeakTag);
            final String filename = System.currentTimeMillis()
                    + "_" + (tag.isBlank() ? "spk" : tag)
                    + "_" + (req.isBlank() ? "req" : req)
                    + "_n" + playbackStartCount + ".wav";
            final Path out = dir.resolve(filename);
            final AudioFormat format = new AudioFormat(playbackDumpSampleRate, 16, playbackDumpChannels, true, false);
            final int frameSize = 2 * playbackDumpChannels;
            final long frameLength = pcm.length / frameSize;
            try (ByteArrayInputStream bais = new ByteArrayInputStream(pcm);
                 AudioInputStream ais = new AudioInputStream(bais, format, frameLength)) {
                AudioSystem.write(ais, AudioFileFormat.Type.WAVE, out.toFile());
            }
            return out.toString();
        } catch (Exception ex) {
            mLogger.warning("[voicetts] wav dump failed request_id=" + safe(playbackRequestId)
                    + " error=" + ex.getMessage());
            return "";
        }
    }

    private String sanitizeFileToken(final String value) {
        String v = safe(value);
        v = v.replaceAll("[^A-Za-z0-9._-]", "_");
        if (v.length() > 80) {
            v = v.substring(0, 80);
        }
        return v;
    }
}
