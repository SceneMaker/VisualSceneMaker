package de.dfki.vsm.xtension.charamel;

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
public final class Charamel implements AbstractExecutor, AbstractFactory, AbstractContext {

    @Override
    public final String compile(
            final AbstractActivity action,
            final AbstractContext context) {
        if (action instanceof VerbalActivity) {
            // Return charamel speech command
            return new String();
        } else if (action instanceof ActionActivity) {
            // Return charamel action command 
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
        System.err.println("Charamel executing command '" + command + "'");

    }

    @Override
    public final String getMarker(final Long id) {
        // Acapela style bookmarks
        return "\\mrk=" + id + "\\";
    }
}
