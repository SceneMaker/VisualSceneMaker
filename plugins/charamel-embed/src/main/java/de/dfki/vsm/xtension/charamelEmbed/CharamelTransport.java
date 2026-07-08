package de.dfki.vsm.xtension.charamelEmbed;

/**
 * Transport seam between {@link CharamelEmbedExecutor} and the VuppetMaster character page.
 *
 * <p>The executor is transport-agnostic: it builds JSON command envelopes (speak, emotion,
 * background, …) and hands them to {@link #send(String)}, and receives page feedback strings
 * (marker echoes, {@code vm.ready}, …) via a {@link Listener}. Two implementations exist:
 * <ul>
 *   <li>{@link JettyTransport} — desktop: an embedded Jetty/Javalin server hosts the page and
 *       exchanges envelopes/feedback over a WebSocket.</li>
 *   <li>{@link AndroidBridgeTransport} — Android: the page runs in a WebView and envelopes/feedback
 *       cross the JS bridge (no embedded HTTP server, so the plugin stays {@code androidCompatible}).</li>
 * </ul>
 *
 * <p>Keeping all Jetty/Javalin references confined to {@link JettyTransport} is what lets the
 * executor class load on Android without those classes on the classpath.
 */
public interface CharamelTransport {

    /** Stands the transport up (start server / prepare bridge). */
    void start() throws Exception;

    /** Sends a JSON command envelope to the character page (VSM → page). */
    void send(String json);

    /** True when a character page is attached and can receive envelopes. */
    boolean isConnected();

    /** Tears the transport down (stop server / detach bridge). */
    void stop();

    /** Callbacks the transport invokes on the executor as the page connects, speaks back, and leaves. */
    interface Listener {
        /** A character page attached. */
        void onConnected();

        /** A feedback string arrived from the page (marker echo, {@code vm.ready}, …). */
        void onMessage(String message);

        /** The character page detached. */
        void onDisconnected();
    }
}
