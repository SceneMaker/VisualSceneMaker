package de.dfki.vsm.runtime.api;

import org.json.JSONObject;

import java.util.function.Consumer;

/**
 * Transport-neutral runtime command endpoint contract.
 *
 * Intended for adapter layers (desktop webserver, Android host, embedded transports)
 * to expose the same command/snapshot semantics over different I/O stacks.
 */
public interface RuntimeCommandEndpoint {

    JSONObject dispatchCommand(String method, JSONObject params, Consumer<String> broadcaster);

    JSONObject snapshot(String projectId);
}
