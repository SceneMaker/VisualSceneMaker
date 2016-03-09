package de.dfki.vsm.runtime.player.trigger;

/**
 * @author Gregor Mehlmann
 */
public final class MarkTrigger implements AbstractTrigger {

    private final String mMark;

    public MarkTrigger(final String mark) {
        mMark = mark;
    }

    @Override
    public final String toString() {
        return mMark;
    }
}
