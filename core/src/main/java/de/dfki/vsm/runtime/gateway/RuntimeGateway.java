package de.dfki.vsm.runtime.gateway;

import de.dfki.vsm.ui.protocol.UiEventBus;
import org.json.JSONObject;

import java.util.function.Consumer;

/**
 * Runtime command/snapshot gateway that allows swapping in-process calls
 * with transport-backed implementations later.
 */
public interface RuntimeGateway {
    JSONObject dispatch(String method, JSONObject params, Consumer<String> broadcaster);

    JSONObject snapshot(String projectId);

    UiEventBus eventBus();
}
