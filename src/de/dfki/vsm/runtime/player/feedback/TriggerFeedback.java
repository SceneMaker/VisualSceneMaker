package de.dfki.vsm.runtime.player.feedback;

import de.dfki.vsm.runtime.player.trigger.AbstractTrigger;

/**
 * @author Gregor Mehlmann
 */
public final class TriggerFeedback implements AbstractFeedback {

    private final AbstractTrigger mTrigger;

    public TriggerFeedback(final AbstractTrigger trigger) {
        mTrigger = trigger;
    }

    public final AbstractTrigger getTrigger() {
        return mTrigger;
    }

    @Override
    public final Object getObject() {
        return getTrigger();
    }
    
    
}
