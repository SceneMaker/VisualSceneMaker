package de.dfki.vsm.runtime.activity.executor;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.project.RunTimeProject;

/**
 * Built-in processing executor for runtime/playaction testing.
 *
 * Commands:
 * - action time=<milliseconds> -> wait for the given duration
 *
 * Blocking vs. non-blocking is controlled by PlayAction invocation mode
 * (default blocking or __vsm_mode="nonblocking"), not by command name.
 */
public final class TestProcessingExecutor extends ActivityExecutor {

    public TestProcessingExecutor(
            final PluginConfig config,
            final RunTimeProject project) {
        super(config, project);
    }

    @Override
    public synchronized String marker(final long id) {
        return "$(" + id + ")";
    }

    @Override
    public void launch() {
        mLogger.message("Launching TestProcessingExecutor ...");
    }

    @Override
    public void unload() {
        // Nothing to release.
    }

    @Override
    public void execute(final AbstractActivity activity) {
        if (activity == null) {
            return;
        }
        final String name = activity.getName() == null ? "" : activity.getName().trim().toLowerCase();
        if (!"action".equals(name)) {
            mLogger.warning("TestProcessingExecutor: unknown action '" + activity.getName()
                    + "'. Expected command name is 'action'.");
            return;
        }
        handleTimedAction(activity);
    }

    private void handleTimedAction(final AbstractActivity activity) {
        final String timeRaw = activity.get("time");
        final long millis;
        try {
            millis = Long.parseLong(timeRaw);
        } catch (Exception exc) {
            mLogger.warning("TestProcessingExecutor: missing/invalid 'time' feature for action.");
            return;
        }
        if (millis < 0) {
            mLogger.warning("TestProcessingExecutor: negative time is not allowed: " + millis);
            return;
        }
        try {
            Thread.sleep(millis);
        } catch (InterruptedException exc) {
            Thread.currentThread().interrupt();
            mLogger.warning("TestProcessingExecutor: blocking wait interrupted.");
        }
    }
}
