package de.dfki.vsm.runtime.activity.trigger;

/**
 * @author Gregor Mehlmann
 */
public final class TimeoutTrigger implements ActivityTrigger, Comparable<TimeoutTrigger> {

    private final long mTimeout;

    public TimeoutTrigger(final long timeout) {
        mTimeout = timeout;
    }

    public final long getTimeout() {
        return mTimeout;
    }

    @Override
    public final String toString() {
        return Long.toString(mTimeout);
    }

    @Override
    public final int compareTo(final TimeoutTrigger trigger) {
        if (trigger.getTimeout() < mTimeout) {
            return 1;
        } else if (trigger.getTimeout() > mTimeout) {
            return -1;
        } else {
            return 0;
        }
    }
}
