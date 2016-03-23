package de.dfki.vsm.xtension.ssi;

import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;

/**
 * @author Gregor Mehlmann
 */
public class SSIRunTimePlugin extends RunTimePlugin {

    // The SSI event handler
    private final SSIEventHandler mHandler;
    // The SSI event handler
    private final SSIEventSender mSender;

    public SSIRunTimePlugin(final RunTimeProject project) {
        // Initialize the runtime plugin
        super(project);
        // Initialize the event handler
        mHandler = new SSIEventHandler(this, "127.0.0.1", 1991);
        // Initialize the event sender
        mSender = new SSIEventSender(this, "127.0.0.1", 1992, "127.0.0.1", 1992);

    }

    // Launch SSI plugin
    @Override
    public void launch() {
        // Start the SSI event handler
        mHandler.start();
        // Start the SSI event sender
        mSender.start();
    }

    // Unload SSI plugin
    @Override
    public void unload() {
        // Abort the SSI event handler
        mHandler.abort();
        // Abort the SSI event sender
        mSender.abort();
        // Join the SSI event threads
        try {
            // Join the SSI event handler
            mHandler.join();
            // Join the SSI event sender
            mSender.join();
        } catch (final Exception exc) {
            mLogger.failure(exc.toString());
        }
    }

    // Handle some SSI event
    public void handle(final SSIEventObject event) {
        mLogger.warning("Handling SSI event:\n" + event.toString());
        // TODO@Patrick:  Do something with the event

    }
}
