package de.dfki.vsm.runtime.api.android;

/**
 * Minimal session abstraction for Android-hosted WebSocket connections.
 *
 * Implement this with the concrete WebSocket stack used on Android
 * (e.g. NanoHTTPD, Ktor, OkHttp, Netty, etc.).
 */
public interface AndroidRuntimeWsSession {

    /**
     * Send a text frame to this session.
     */
    void sendText(String message);
}
