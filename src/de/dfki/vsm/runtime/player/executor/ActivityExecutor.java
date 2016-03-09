package de.dfki.vsm.runtime.player.executor;

import de.dfki.vsm.runtime.player.activity.AbstractActivity;
import de.dfki.vsm.runtime.player.activity.player.ActivityPlayer;

/**
 * @author Gregor Mehlmann
 */
public interface ActivityExecutor {

    public void launch();

    public void unload();

    public String marker(final Long id);

    public void execute(
            final AbstractActivity activity,
            final ActivityPlayer scheduler);

}
