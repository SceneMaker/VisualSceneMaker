package de.dfki.vsm.runtime.gateway;

import de.dfki.vsm.ui.protocol.UiEventBus;
import de.dfki.vsm.ui.protocol.UiProtocol;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.function.Consumer;

/**
 * Gateway used when runtime bridge operations are intentionally disabled.
 */
public final class DisabledRuntimeGateway implements RuntimeGateway {

    @Override
    public JSONObject dispatch(final String method, final JSONObject params, final Consumer<String> broadcaster) {
        JSONObject response = new JSONObject();
        response.put("status", "error");
        JSONObject error = new JSONObject();
        error.put("code", "UNSUPPORTED_FEATURE");
        error.put("message", "Runtime gateway is disabled on this platform");
        response.put("error", error);
        return response;
    }

    @Override
    public JSONObject snapshot(final String projectId) {
        JSONObject snapshot = new JSONObject();
        snapshot.put("nodes", new JSONArray());
        snapshot.put("edges", new JSONArray());
        snapshot.put("comments", new JSONArray());
        return snapshot;
    }

    @Override
    public UiEventBus eventBus() {
        return UiProtocol.getEventBus();
    }
}
