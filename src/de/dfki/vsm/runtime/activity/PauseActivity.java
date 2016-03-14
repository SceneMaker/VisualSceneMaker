package de.dfki.vsm.runtime.activity;

/**
 * @author Gregor Mehlmann
 */
public final class PauseActivity extends AbstractActivity {

    // The pause timeout delay
    private final long mTimeout;

    // Construct a pause activity
    public PauseActivity(
            final String actor,
            final long timeout) {
        super(Policy.BLOCKING, actor, "SPEECH", "PAUSE");
        // Initialize the timeout
        mTimeout = timeout;
    }

    // Get the pause timeout
    public final long getTimeout() {
        return mTimeout;
    }
}
