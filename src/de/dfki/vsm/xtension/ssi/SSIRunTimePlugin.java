package de.dfki.vsm.xtension.ssi;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;

/**
 * @author Gregor Mehlmann
 */
public abstract class SSIRunTimePlugin extends RunTimePlugin implements SSIEventHandler {

    // The SSI event receiver
    private SSIEventReceiver mReceiver;
    // The SSI event handler
    private SSIEventSender mSender;

    public SSIRunTimePlugin(
            final PluginConfig config,
            final RunTimeProject project) {
        // Initialize the runtime plugin
        super(config, project);
    }
    
    // Launch SSI plugin
    @Override
    public void launch() {
        // Get the plugin configuration
        final String hlhost = mConfig.getProperty("hlhost");
        final String hlport = mConfig.getProperty("hlport");
        final String slhost = mConfig.getProperty("slhost");
        final String slport = mConfig.getProperty("slport");
        final String srhost = mConfig.getProperty("srhost");
        final String srport = mConfig.getProperty("srport");
        // Initialize the event receiver
        mReceiver = new SSIEventReceiver(this,
                hlhost, Integer.parseInt(hlport));
        // Initialize the event sender
        mSender = new SSIEventSender(
                slhost, Integer.parseInt(slport),
                srhost, Integer.parseInt(srport));
        // Start the SSI event receiver
        mReceiver.start();
        // Start the SSI event sender
        mSender.start();
    }

    // Unload SSI plugin
    @Override
    public void unload() {
        // Abort the SSI event receiver
        mReceiver.abort();
        // Abort the SSI event sender
        mSender.abort();
        // Join the SSI event threads
        try {
            // Join the SSI event receiver
            mReceiver.join();
            // Join the SSI event sender
            mSender.join();
        } catch (final InterruptedException exc) {
            mLogger.failure(exc.toString());
        }
    }
}
