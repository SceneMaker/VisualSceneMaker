package de.dfki.vsm.runtime.activity.executor;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.manager.ActivityManager;

/**
 * @author Gregor Mehlmann
 */
public interface ActivityExecutor {

    public void launch();

    public void unload();

    public String marker(final long id);

    public void execute(
            final AbstractActivity activity,
            final ActivityManager player);

}
