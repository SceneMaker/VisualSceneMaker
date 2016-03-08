package de.dfki.vsm.runtime.player.executor;

import de.dfki.vsm.runtime.player.activity.AbstractActivity;
import de.dfki.vsm.runtime.player.scheduler.AbstractScheduler;

/**
 * @author Gregor Mehlmann
 */
public interface AbstractExecutor {

    public String getMarker(final Long id);

    public void execute(
            final AbstractActivity activity,
            final AbstractScheduler scheduler);

}
