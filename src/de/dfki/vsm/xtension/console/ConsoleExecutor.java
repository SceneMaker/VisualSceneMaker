package de.dfki.vsm.xtension.console;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import java.awt.Dimension;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

/**
 * @author Gregor Mehlmann
 */
public final class ConsoleExecutor extends ActivityExecutor {

    private final JFrame mMainFrame;
    private final JTextArea mTextArea;
    private final JScrollPane mScrollPane;

    public ConsoleExecutor(final PluginConfig config, final RunTimeProject project) {
        //
        super(config, project);
        // Create the text area
        mTextArea = new JTextArea();
        mScrollPane = new JScrollPane(mTextArea);
        mScrollPane.setMinimumSize(new Dimension(400, 300));
        mScrollPane.setMaximumSize(new Dimension(400, 300));
        mScrollPane.setPreferredSize(new Dimension(400, 300));
        //
        mMainFrame = new JFrame();
        // Set the content pane
        mMainFrame.setContentPane(mScrollPane);
        // TODO: Get the agent name for this executor object here
        mMainFrame.setTitle(project.getProjectName());
        //
        mMainFrame.pack();
    }

    @Override
    public void launch() {
        // Clear the text area
        mTextArea.setText("");
        // Set frame visible
        mMainFrame.setVisible(true);
    }

    @Override
    public void unload() {
        mMainFrame.setVisible(false);
    }

    @Override
    public final void execute(
            final AbstractActivity object) {
        // Execute the command
        System.err.println("Console executing activity '" + object + "'");
        try {
            mTextArea.getDocument().insertString(
                    mTextArea.getDocument().getLength(), "\n", null);
            mTextArea.getDocument().insertString(
                    mTextArea.getDocument().getLength(), "AGENT:", null);

        } catch (final Exception exc) {
            System.err.println(exc.toString());
        }
        // Chech activity type
        if (object instanceof SpeechActivity) {
            final SpeechActivity activity = (SpeechActivity) object;
            // Execute speech command
            final Scanner scanner = new Scanner(activity.toString());
            while (scanner.hasNext()) {
                // Get the next token
                final String token = scanner.next();
                final Pattern pattern = Pattern.compile("#(.*?)#");
                final Matcher matcher = pattern.matcher(token);
                if (matcher.matches()) {
                    // Check for bookmark
                    final String id = matcher.group(1);
                    // Give some feedback
                    mScheduler.handle(token);
                } else {
                    try {
                        mTextArea.getDocument().insertString(
                                mTextArea.getDocument().getLength(), token, null);
                        mTextArea.getDocument().insertString(
                                mTextArea.getDocument().getLength(), " ", null);
                        // Sleep for some time
                        Thread.sleep(500);
                    } catch (final Exception exc) {
                        System.err.println(exc.toString());
                        // Return when aborted
                        return;
                    }
                }
            }
        } else if (object instanceof ActionActivity) {
            final ActionActivity activity = (ActionActivity) object;
            try {
                //
                mTextArea.getDocument().insertString(
                        mTextArea.getDocument().getLength(), activity.getText(), null);
                Thread.sleep(10000);
            } catch (final Exception exc) {
                System.err.println(exc.toString());
            }
        }
        // Give some feedback
        //scheduler.handle(new StatusFeedback(object, STOPPED));

    }

    @Override
    public final String marker(final long id) {
        // Phantasie style bookmarks
        return "#" + id + "#";
    }
}
