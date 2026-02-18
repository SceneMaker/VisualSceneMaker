package de.dfki.vsm.runtime.gateway;

import de.dfki.vsm.ui.protocol.UiEventBus;
import de.dfki.vsm.ui.protocol.UiProtocol;
import org.json.JSONObject;

import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * Local runtime gateway implementation that delegates directly to in-process handlers.
 */
public final class InProcessRuntimeGateway implements RuntimeGateway {

    @FunctionalInterface
    public interface CommandDispatcher {
        JSONObject dispatch(String method, JSONObject params, Consumer<String> broadcaster);
    }

    private final CommandDispatcher mCommandDispatcher;
    private final Function<String, JSONObject> mSnapshotProvider;
    private final UiEventBus mEventBus;

    public InProcessRuntimeGateway(final CommandDispatcher commandDispatcher,
                                   final Function<String, JSONObject> snapshotProvider) {
        this(commandDispatcher, snapshotProvider, UiProtocol.getEventBus());
    }

    public InProcessRuntimeGateway(final CommandDispatcher commandDispatcher,
                                   final Function<String, JSONObject> snapshotProvider,
                                   final UiEventBus eventBus) {
        mCommandDispatcher = Objects.requireNonNull(commandDispatcher, "commandDispatcher");
        mSnapshotProvider = Objects.requireNonNull(snapshotProvider, "snapshotProvider");
        mEventBus = Objects.requireNonNull(eventBus, "eventBus");
    }

    @Override
    public JSONObject dispatch(final String method, final JSONObject params, final Consumer<String> broadcaster) {
        JSONObject safeParams = params != null ? params : new JSONObject();
        return mCommandDispatcher.dispatch(method, safeParams, broadcaster);
    }

    @Override
    public JSONObject snapshot(final String projectId) {
        return mSnapshotProvider.apply(projectId != null ? projectId : "");
    }

    @Override
    public UiEventBus eventBus() {
        return mEventBus;
    }
}
