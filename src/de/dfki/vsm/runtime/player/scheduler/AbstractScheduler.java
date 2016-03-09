package de.dfki.vsm.runtime.player.scheduler;

import de.dfki.vsm.runtime.player.activity.AbstractActivity;
import de.dfki.vsm.runtime.player.executor.AbstractExecutor;
import de.dfki.vsm.runtime.player.feedback.AbstractFeedback;
import de.dfki.vsm.runtime.player.trigger.AbstractTrigger;

/**
 * @author Gregor Mehlmann
 */
public interface AbstractScheduler {

    public enum SchedulingPolicy {

        BLOCKING,
        CONCURRENT
    }

    public boolean launch();

    public boolean unload();

    public AbstractScheduler register(
            final SchedulingPolicy policy,
            final AbstractTrigger trigger,
            final AbstractActivity activity,
            final AbstractExecutor executor);

    public void feedback(final AbstractFeedback feedback);
}
