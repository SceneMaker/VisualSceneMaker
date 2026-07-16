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

    /** Port the live character page's own HTTP+WebSocket server listens on, or {@code -1} if
     *  there is no such server (not started yet, or no HTTP page at all — e.g. Android). Not a
     *  full URL: the caller must build one against its own reachable host, not a hardcoded
     *  "localhost" (which breaks for a remote LAN collaborator — see {@code CharacterPreviewCapable}). */
    int getPreviewPort();

    /** Tears the transport down (stop server / detach bridge). */
    void stop();

    /** Mutes or unmutes delivery to whichever connected client is the authoring-time preview page
     *  (see {@link JettyTransport}'s {@code vsmPreview} query-param tagging), so a real SceneFlow
     *  run doesn't also speak out of the preview panel while an audience-facing viewer speaks the
     *  same line. No-op where there is only one possible viewer (e.g. Android). */
    void setPreviewMuted(boolean muted);

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
