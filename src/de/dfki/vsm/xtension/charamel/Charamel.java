package de.dfki.vsm.xtension.charamel;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.manager.ActivityManager;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.feedback.StatusFeedback;
import static de.dfki.vsm.runtime.activity.feedback.StatusFeedback.Status.ABORTED;
import static de.dfki.vsm.runtime.activity.feedback.StatusFeedback.Status.RUNNING;
import static de.dfki.vsm.runtime.activity.feedback.StatusFeedback.Status.STARTED;
import static de.dfki.vsm.runtime.activity.feedback.StatusFeedback.Status.STOPPED;
import de.dfki.vsm.runtime.project.RunTimeProject;

/**
 * @author Gregor Mehlmann
 */
public final class Charamel implements ActivityExecutor {

    public Charamel(final RunTimeProject project) {
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
        // Print some information
        System.err.println("Charamel executing command '" + command + "'");
        // Give some status feeback
        scheduler.handle(new StatusFeedback(activity, STARTED));
        //
        try {
            for (int i = 0; i < 5; i++) {
                // Simulate the execution
                Thread.sleep(1000);
                // Give some status feeback
                scheduler.handle(new StatusFeedback(activity, RUNNING));
            }
        } catch (final Exception exc) {
            // Print some information
            System.err.println("Interrupting command execution '" + command + "'");
            // Give some status feeback
            scheduler.handle(new StatusFeedback(activity, ABORTED));
        }

        // Give some status feeback
        scheduler.handle(new StatusFeedback(activity, STOPPED));

    }

    @Override
    public final String marker(final long id) {
        // Acapela style bookmarks
        return "\\mrk=" + id + "\\";
    }
}
