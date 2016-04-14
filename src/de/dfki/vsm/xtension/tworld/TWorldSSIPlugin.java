package de.dfki.vsm.xtension.tworld;

import de.dfki.vsm.xtension.ssi.event.SSIEventArray;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.interpreter.value.IntValue;
import de.dfki.vsm.runtime.interpreter.value.StructValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.ssi.SSIEventHandler;
import de.dfki.vsm.xtension.ssi.SSIEventSender;
import de.dfki.vsm.xtension.ssi.SSIRunTimePlugin;
import de.dfki.vsm.xtension.ssi.event.SSIEventObject;
import de.dfki.vsm.xtension.ssi.event.data.SSIEventData;
import de.dfki.vsm.xtension.ssi.event.data.SSIXMLData;
import java.io.File;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Gregor Mehlmann, Patrick Gebhard
 */
public final class TWorldSSIPlugin extends SSIRunTimePlugin {

    // The map of processes
    private final HashMap<String, Process> mProcessMap = new HashMap();

    public TWorldSSIPlugin(
            final PluginConfig config,
            final RunTimeProject project) {
        super(config, project);
        
        // necessary for the internal assignment of the handler (should be this and not super!)
        mPlugin = this;
    }

    // Launch SSI plugin
    @Override
    public void launch() {
        final String ssidir = mConfig.getProperty("ssidir");
        final String ssibat = mConfig.getProperty("ssibat");

        // Create the plugin's processes
        try {
            mProcessMap.put(ssibat, Runtime.getRuntime().exec(
                    "cmd /c start " + ssibat + "" + /* place for arguments */ "", null, new File(ssidir)));
        } catch (final Exception exc) {
            mLogger.failure(exc.toString());
        }

        super.launch();
    }

    // Unload SSI plugin
    @Override
    public void unload() {
        super.unload();

        // Wait for pawned processes
        for (final Map.Entry<String, Process> entry : mProcessMap.entrySet()) {
            // Get the process entry
            final String name = entry.getKey();
            final Process process = entry.getValue();
            try {
                // Kill the processes
                final Process killer = Runtime.getRuntime().exec("taskkill /F /IM " + name);
                // Wait for the killer
                killer.waitFor();
                // Print some information 
                mLogger.message("Joining killer " + name + "");
                // Wait for the process
                process.waitFor();
                // Print some information 
                mLogger.message("Joining process " + name + "");
            } catch (final Exception exc) {
                mLogger.failure(exc.toString());
            }
        }
    }

    @Override
    public void handle(final SSIEventArray array) {
         mLogger.message("TWORLD SSI PLUGIN HANDLING ...");
        
        
        HashMap<String, AbstractValue> values = new HashMap<>();

        for (final SSIEventObject event : array.getEventList()) {
            final SSIEventData obj = event.getData();
            //
            if (obj instanceof SSIXMLData) {
                final TWorldSSIData data = new TWorldSSIData(
                        ((SSIXMLData) obj).getXML());

                //mLogger.message(".");
                values.put("headxpos", new IntValue(new Integer(data.getHeadData().getPosData().getX())));
                values.put("headypos", new IntValue(new Integer(data.getHeadData().getPosData().getY())));

                try {
                    StructValue struct = new StructValue(values);
                    mProject.setVariable("usercues", struct);//GM
                } catch (Exception e) {
                    // System.out.println("not running");
                }
            }
        }
    }
}
