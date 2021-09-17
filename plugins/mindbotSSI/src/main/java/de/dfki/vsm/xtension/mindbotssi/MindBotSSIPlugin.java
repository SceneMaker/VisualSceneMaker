package de.dfki.vsm.xtension.mindbotssi;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.ssi.SSIRunTimePlugin;
import de.dfki.vsm.xtension.ssi.event.SSIEventArray;
import de.dfki.vsm.xtension.ssi.event.SSIEventEntry;
import de.dfki.vsm.xtension.ssi.event.data.SSIEventData;

/**
 * @author Fabrizio Nunnari
 */
public final class MindBotSSIPlugin extends SSIRunTimePlugin {

    // Construct SSI plugin
    public MindBotSSIPlugin(
            final PluginConfig config,
            final RunTimeProject project) {
        super(config, project);

    }

    public void sendStart() {


        mSender.sendBytes(
                // 0000"enable using wireshark"
                new byte[] {0X05,0X00,0X00,0X00}


        );

        mLogger.message("message sent");

    }

    void sendStop() {
        mSender.sendBytes(
                // 0000"disable using wireshark"
                new byte[]{ 0X02,0X00,0X00,0X00}


                //new byte[]{02,00,00,00}
        );
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
        for (final SSIEventEntry event_entry : array.list()) {
            final SSIEventData data = event_entry.getData();
            // Imagine the event was produced from address "coords@mouse" or "click@mouse"
            mLogger.message(" - sender: " +  event_entry.getSender() + // "mouse"
                    "\t event: " + event_entry.getEvent() + // "coords"
                    "\t from: " + event_entry.getFrom() + // an integer number (?)
                    "\t type: " + event_entry.getType() + // TUPLE for coords, or EMPTY for clicks
                    "\t state: " + event_entry.getState() +  // "continued" for streamed coords and clicks down, or "completed" for clicks up.
                    "\t data: " + data);

            String sender = event_entry.getSender() ;
            String event = event_entry.getEvent() ;

            if (sender.equals("mouse") && event.equals("coords")) {
                assert event_entry.getType().equals("STRING") ;
                String data_str = data.toString() ;
                // Format, e.g.: 0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675;0.249269,0.507675
                String[] coords = data_str.split(";") ;
                // Split the last element
                String[] last_coords_str = coords[coords.length - 1].split(",") ;
                assert last_coords_str.length == 2 ;
                float x = Float.parseFloat(last_coords_str[0]) ;
                float y = Float.parseFloat(last_coords_str[1]) ;
                mLogger.message("Most recent Mouse coords: \t" + x + "\t" + y);

                mProject.setVariable("ssi_mouse_x", x);
                mProject.setVariable("ssi_mouse_y", y);

            } else if (sender.equals("mouse") && event.equals("click")) {
                assert event_entry.getType().equals("STRING") ;
                assert data == null ; // Mouse clicks bring no data
                String state = event_entry.getState() ;
                if (state.equals("continued")) {
                    mLogger.message("Mouse Click DOWN");
                } else if (state.equals("completed")) {
                    mLogger.message("Mouse Click UP. Duration: " + event_entry.getDur());
                } else {
                    assert false;
                }
            }

        }
    }
}
