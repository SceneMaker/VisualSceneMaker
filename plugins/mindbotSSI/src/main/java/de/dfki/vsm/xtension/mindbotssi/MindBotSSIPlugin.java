package de.dfki.vsm.xtension.mindbotssi;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;

import de.dfki.vsm.xtension.ssi.SSIEventSender;
import de.dfki.vsm.xtension.ssi.SSIRunTimePlugin;
import de.dfki.vsm.xtension.ssi.event.SSIEventArray;
import de.dfki.vsm.xtension.ssi.event.SSIEventEntry;
import de.dfki.vsm.xtension.ssi.event.data.SSIEventData;

import java.io.IOException;
import java.net.DatagramPacket;

/**
 * @author Fabrizio Nunnari
 */
public abstract class MindBotSSIPlugin extends SSIRunTimePlugin {

    // Construct SSI plugin
    public MindBotSSIPlugin(
            final PluginConfig config,
            final RunTimeProject project) {
        super(config, project);

    }

    // Launch SSI plugin
    @Override
    public void launch() {
        mLogger.message("Launching MindBotSSI Plugin...");

        ///super.launch();
    }

    // Unload SSI plugin
    @Override
    public void unload() {
//       super.unload();
//
        // Print some information
        mLogger.message("MindBotSSI Plugin unloaded.");
    }

    // Handle SSI event array

}
