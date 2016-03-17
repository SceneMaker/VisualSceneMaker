package de.dfki.vsm.xtension.stickman;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.manager.ActivityManager;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;

/**
 * @author Gregor Mehlmann
 */
public final class Stickman implements ActivityExecutor {

    public Stickman(final RunTimeProject project) {
    }

    
    @Override
    public void launch() {

    }

    @Override
    public void unload() {

    }

    @Override
    public final void execute(
            final AbstractActivity activity,
            final ActivityManager scheduler) {
        // Compile the activity
        final String command = activity.toString();
        // Execute the command
        System.err.println("Stickman executing command '" + command + "'");

    }

    @Override
    public final String marker(final long id) {
        // Microsoft style bookmarks
        return "<mark name=\"" + id + "\"/>";
    }
}
