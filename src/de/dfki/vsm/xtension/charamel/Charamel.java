package de.dfki.vsm.xtension.charamel;

import de.dfki.vsm.runtime.player.activity.AbstractActivity;
import de.dfki.vsm.runtime.player.activity.player.ActivityPlayer;
import de.dfki.vsm.runtime.player.executor.ActivityExecutor;
import de.dfki.vsm.runtime.player.executor.feedback.StatusFeedback;
import static de.dfki.vsm.runtime.player.executor.feedback.StatusFeedback.ExecutionStatus.ABORTED;
import static de.dfki.vsm.runtime.player.executor.feedback.StatusFeedback.ExecutionStatus.RUNNING;
import static de.dfki.vsm.runtime.player.executor.feedback.StatusFeedback.ExecutionStatus.STARTED;
import static de.dfki.vsm.runtime.player.executor.feedback.StatusFeedback.ExecutionStatus.STOPPED;
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
            final ActivityPlayer scheduler) {
        // Compile the activity
        final String command = activity.getText();
        // Print some information
        System.err.println("Charamel executing command '" + command + "'");
        // Give some status feeback
        scheduler.feedback(new StatusFeedback(activity, STARTED));
        //
        try {
            for (int i = 0; i < 5; i++) {
                // Simulate the execution
                Thread.sleep(1000);
                // Give some status feeback
                scheduler.feedback(new StatusFeedback(activity, RUNNING));
            }
        } catch (final Exception exc) {
            // Print some information
            System.err.println("Interrupting command execution '" + command + "'");
            // Give some status feeback
            scheduler.feedback(new StatusFeedback(activity, ABORTED));
        }

        // Give some status feeback
        scheduler.feedback(new StatusFeedback(activity, STOPPED));

    }

    @Override
    public final String marker(final Long id) {
        // Acapela style bookmarks
        return "\\mrk=" + id + "\\";
    }
}
