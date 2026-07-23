package de.dfki.vsm.runtime.plugin;

/**
 * Optional capability for an {@link de.dfki.vsm.runtime.activity.executor.ActivityExecutor} whose
 * speech engine can embed a real pause directly into ongoing synthesis (e.g. SSML {@code <break
 * time="500ms"/>} via an Azure-TTS-backed engine), instead of the interpreter having to split the
 * utterance into two separate speak calls with an external sleep in between.
 *
 * <p>The split-based approach ({@code ReactivePlayer.playScene()}'s default {@code pause}
 * handling) adds the *entire* preceding speech call's completion latency on top of the actual
 * pause duration, since the interpreter must wait for that call's real completion signal before
 * it can even start the sleep (see {@code CharamelEmbedExecutor.broadcastSpeakAndAwaitStop}'s
 * docs — confirmed 2026-07-23 to be a consistent ~1s+ tail latency, unrelated to the requested
 * pause length). A speech engine that can embed the pause as literal synthesis markup avoids that
 * entirely: the pause becomes part of one continuous speak call, timed by the TTS engine itself.
 *
 * @author Patrick Gebhard
 */
public interface SpeechBreakCapable {

    /** Markup to embed directly into an in-progress speech text to pause synthesis for
     *  {@code durationMs} milliseconds at that exact point. */
    String speechBreakMarkup(long durationMs);
}
