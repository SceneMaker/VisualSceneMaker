package de.dfki.vsm.xtension.stickman;

import de.dfki.vsm.runtime.player.activity.AbstractActivity;
import de.dfki.vsm.runtime.player.activity.player.ActivityPlayer;
import de.dfki.vsm.runtime.player.executor.ActivityExecutor;
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
            final ActivityPlayer scheduler) {
        // Compile the activity
        final String command = activity.getText();
        // Execute the command
        System.err.println("Stickman executing command '" + command + "'");

    }

    @Override
    public final String marker(final Long id) {
        // Microsoft style bookmarks
        return "<mark name=\"" + id + "\"/>";
    }
}
