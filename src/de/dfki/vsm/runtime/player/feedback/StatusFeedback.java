package de.dfki.vsm.runtime.player.feedback;

/**
 * @author Gregor Mehlmann
 */
public final class StatusFeedback implements AbstractFeedback {

    private final String mStatus;

    public StatusFeedback(String status) {
        mStatus = status;
    }

    public final String getStatus() {
        return mStatus;
    }

    @Override
    public final Object getObject() {
        return getStatus();
    }
}
