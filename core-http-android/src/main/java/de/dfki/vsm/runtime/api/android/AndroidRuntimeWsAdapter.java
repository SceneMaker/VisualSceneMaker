package de.dfki.vsm.runtime.api.android;

import de.dfki.vsm.runtime.api.RuntimeCommandEndpoint;
import de.dfki.vsm.runtime.api.RuntimeWsProtocol;
import org.json.JSONObject;

import java.util.Objects;

/**
 * Android WebSocket adapter that delegates runtime commands to {@link RuntimeCommandEndpoint}
 * and uses the shared {@link RuntimeWsProtocol} envelope.
 */
public final class AndroidRuntimeWsAdapter {

    private final RuntimeCommandEndpoint endpoint;
    private final AndroidRuntimeWsSessionRegistry sessions;

    public AndroidRuntimeWsAdapter(final RuntimeCommandEndpoint endpoint) {
        this(endpoint, new AndroidRuntimeWsSessionRegistry());
    }

    public AndroidRuntimeWsAdapter(final RuntimeCommandEndpoint endpoint,
                                   final AndroidRuntimeWsSessionRegistry sessions) {
        this.endpoint = Objects.requireNonNull(endpoint, "endpoint");
        this.sessions = Objects.requireNonNull(sessions, "sessions");
    }

    public AndroidRuntimeWsSessionRegistry sessions() {
        return sessions;
    }

    public void onOpen(final AndroidRuntimeWsSession session) {
        sessions.add(session);
    }

    public void onClose(final AndroidRuntimeWsSession session) {
        sessions.remove(session);
    }

    public void onMessage(final AndroidRuntimeWsSession session, final String rawMessage) {
        try {
            final RuntimeWsProtocol.CommandRequest request = RuntimeWsProtocol.parseRequest(rawMessage);
            final JSONObject result = endpoint.dispatchCommand(
                    request.method(),
                    request.params(),
                    sessions.broadcaster()
            );
            session.sendText(RuntimeWsProtocol.successResponse(request.id(), result).toString());
        } catch (Exception exc) {
            final String message = exc.getMessage() == null ? "Unknown error" : exc.getMessage();
            session.sendText(RuntimeWsProtocol.errorResponse(message).toString());
        }
    }
}
