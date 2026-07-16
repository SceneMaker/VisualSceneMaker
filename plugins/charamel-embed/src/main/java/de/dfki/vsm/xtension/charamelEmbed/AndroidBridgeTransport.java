package de.dfki.vsm.xtension.charamelEmbed;

import java.util.function.Consumer;

/**
 * Android transport: exchanges command envelopes and feedback with a VuppetMaster character page
 * running in an Android {@code WebView}, via the JS bridge — no embedded HTTP server, so the plugin
 * stays {@code androidCompatible}.
 *
 * <p>Pure Java (no Android imports) so it compiles into the plugin's Android thin jar. The Android
 * host wires it at both ends after obtaining it from
 * {@link CharamelEmbedExecutor#getAndroidBridge()}:
 * <ul>
 *   <li><b>VSM → page:</b> the host calls {@link #setEnvelopeSink} with a sink that runs
 *       {@code webView.evaluateJavascript("window.vsmDispatch(" + json + ")")}; the executor's
 *       {@link #send} then routes through it.</li>
 *   <li><b>page → VSM:</b> the page's {@code vm-adapter.js} calls {@code window.AndroidVSM.send(msg)};
 *       the host's {@code @JavascriptInterface} forwards that to {@link #feedback}.</li>
 *   <li><b>Lifecycle:</b> the host calls {@link #markConnected()} once the page has loaded and
 *       {@link #markDisconnected()} when it goes away.</li>
 * </ul>
 * The seam mirrors {@code vm-adapter.js}, whose {@code vsmFeedback}/{@code vsmDispatch} functions
 * already branch to {@code window.AndroidVSM} when present.
 */
public final class AndroidBridgeTransport implements CharamelTransport {

    private final Listener mListener;
    private volatile Consumer<String> mEnvelopeSink;
    private volatile boolean mConnected;

    public AndroidBridgeTransport(Listener listener) {
        this.mListener = listener;
    }

    // ---- Android host API ----------------------------------------------------

    /** Registers the sink that forwards a JSON envelope into the WebView (VSM → page). */
    public void setEnvelopeSink(Consumer<String> sink) {
        this.mEnvelopeSink = sink;
    }

    /** Host pushes a feedback string received from the page's JS bridge (page → VSM). */
    public void feedback(String message) {
        if (mListener != null) mListener.onMessage(message);
    }

    /** Host signals the character page has loaded and can receive envelopes. */
    public void markConnected() {
        mConnected = true;
        if (mListener != null) mListener.onConnected();
    }

    /** Host signals the character page has gone away. */
    public void markDisconnected() {
        mConnected = false;
        if (mListener != null) mListener.onDisconnected();
    }

    // ---- CharamelTransport SPI ----------------------------------------------

    @Override
    public void start() {
        // Nothing to stand up: the Android host owns the WebView lifecycle and calls
        // markConnected()/markDisconnected() as the page loads/unloads.
    }

    @Override
    public void send(String json) {
        Consumer<String> sink = mEnvelopeSink;
        if (sink != null) sink.accept(json);
    }

    @Override
    public boolean isConnected() {
        return mConnected;
    }

    @Override
    public int getPreviewPort() {
        // The character page lives inside the host's WebView, not at an HTTP URL.
        return -1;
    }

    @Override
    public void stop() {
        markDisconnected();
        mEnvelopeSink = null;
    }

    @Override
    public void setPreviewMuted(boolean muted) {
        // The host's WebView is the only possible viewer — nothing to mute against.
    }
}
