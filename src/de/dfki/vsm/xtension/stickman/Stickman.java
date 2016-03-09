package de.dfki.vsm.xtension.stickman;

import de.dfki.vsm.runtime.player.activity.AbstractActivity;
import de.dfki.vsm.runtime.player.activity.ActionActivity;
import de.dfki.vsm.runtime.player.activity.VerbalActivity;
import de.dfki.vsm.runtime.player.context.AbstractContext;
import de.dfki.vsm.runtime.player.executor.AbstractExecutor;
import de.dfki.vsm.runtime.player.factory.AbstractFactory;
import de.dfki.vsm.runtime.player.scheduler.AbstractScheduler;

/**
 * @author Gregor Mehlmann
 */
public final class Stickman implements AbstractExecutor, AbstractFactory, AbstractContext {

    @Override
    public final String compile(
            final AbstractActivity action,
            final AbstractContext context) {
        if (action instanceof VerbalActivity) {
            // Return stickman speech command
            return new String();
        } else if (action instanceof ActionActivity) {
            // Return stickman action command
            return new String();
        } else {
            return new String();
        }
    }

    @Override
    public final void execute(
            final AbstractActivity activity,
            final AbstractScheduler scheduler) {
        // Compile the activity
        final String command = compile(activity, this);
        // Execute the command
        System.err.println("Stickman executing command '" + command + "'");

    }

    @Override
    public final String getMarker(final Long id) {
        // Microsoft style bookmarks
        return "<mark name=\"" + id + "\"/>";
    }
}
