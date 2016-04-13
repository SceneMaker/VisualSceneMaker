package de.dfki.vsm.xtension.tworld;

import de.dfki.vsm.xtension.ssi.event.SSIEventArray;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.player.RunTimePlayer;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.ssi.SSIRunTimePlugin;
import de.dfki.vsm.xtension.ssi.event.SSIEventObject;
import de.dfki.vsm.xtension.ssi.event.data.SSIEventData;
import de.dfki.vsm.xtension.ssi.event.data.SSIXMLData;

/**
 * @author Gregor Mehlmann
 */
public final class TWorldSSIPlugin extends SSIRunTimePlugin {

 
            
    public TWorldSSIPlugin(
            final PluginConfig config,
            final RunTimeProject project) {
        super(config, project);
    }

    @Override
    public void handle(final SSIEventArray array) {
        ///
        for (final SSIEventObject event : array.getEventList()) {
            final SSIEventData obj = event.getData();
            //
            if (obj instanceof SSIXMLData) {
                final TWorldSSIData data = new TWorldSSIData(
                        ((SSIXMLData) obj).getXML());
                // TODO: Set variables ...
               // mProject.setVariable();
            }
        }
    }
}
