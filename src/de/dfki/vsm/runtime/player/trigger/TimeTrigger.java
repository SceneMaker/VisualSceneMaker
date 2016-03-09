package de.dfki.vsm.runtime.player.trigger;

import java.util.concurrent.TimeUnit;

/**
 * @author Gregor Mehlmann
 */
public final class TimeTrigger implements AbstractTrigger {

    private final long mDelay;
    private final TimeUnit mUnit;

    public TimeTrigger(final long time, final TimeUnit unit) {
        mDelay = time;
        mUnit = unit;
    }

    public final long getDelay() {
        return mDelay;
    }

    public final TimeUnit getUnit() {
        return mUnit;
    }

    @Override
    public final String toString() {
        return Long.toString(mDelay) + mUnit.name();
    }
}
