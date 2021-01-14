package de.dfki.vsm.xtension.mindbotssi;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.interpreter.value.FloatValue;
import de.dfki.vsm.runtime.interpreter.value.ListValue;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.jpl.JPLEngine;
import de.dfki.vsm.util.xml.XMLUtilities;
import de.dfki.vsm.xtension.ssi.SSIRunTimePlugin;
import de.dfki.vsm.xtension.ssi.event.SSIEventArray;
import de.dfki.vsm.xtension.ssi.event.SSIEventEntry;
import de.dfki.vsm.xtension.ssi.event.data.SSIEventData;
import de.dfki.vsm.xtension.ssi.event.data.SSIStringData;
import de.dfki.vsm.xtension.ssi.event.data.SSITupleData;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public final class MindBotSSIPlugin extends SSIRunTimePlugin {

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
        super.launch();
    }

    // Unload SSI plugin
    @Override
    public void unload() {
        super.unload();

        // Print some information
        mLogger.message("MindBotSSI Plugin unloaded.");
    }

    // Handle SSI event array
    @Override
    public void handle(final SSIEventArray array) {
        // Print some information
        mLogger.message("Got SSI message array of size " + array.size());
        for (final SSIEventEntry event : array.list()) {
            final SSIEventData data = event.getData();
            // Imagine the event was produced from address "coords@mouse" or "click@mouse"
            mLogger.message(" - sender: " +  event.getSender() + // "mouse"
                    "\t event: " + event.getEvent() + // "coords"
                    "\t from: " + event.getFrom() + // an integer number (?)
                    "\t type: " + event.getType() + // TUPLE for coords, or EMPTY for clicks
                    "\t state: " + event.getState() +  // "continued" for streamed coords and clicks down, or "completed" for clicks up.
                    "\t data: " + data);

            if (event.getSender().equals("mouse") && event.getEvent().equals("coords")) {
                assert event.getType().equals("STRING") ;
                String data_str = data.toString() ;
                // Format, e.g.: 0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675
                String[] coords = data_str.split(";") ;
                // Split the last element
                String[] last_coords_str = coords[coords.length - 1].split(",") ;
                assert last_coords_str.length == 2 ;
                float x = Float.parseFloat(last_coords_str[0]) ;
                float y = Float.parseFloat(last_coords_str[1]) ;
                mLogger.message("Most recent Mouse coords: \t" + x + "\t" + y);
            }

            // mProject.setVariable("UserSaidKeyword", keyword);

        }
    }
}
