package de.dfki.vsm.xtension.mindbotssi;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.ssi.SSIRunTimePlugin;
import de.dfki.vsm.xtension.ssi.event.SSIEventArray;
import de.dfki.vsm.xtension.ssi.event.SSIEventEntry;
import de.dfki.vsm.xtension.ssi.event.data.SSIEventData;

/**
 * @author Fabrizio Nunnari
 */
public final class MindBotSSIPlugin<Int> extends SSIRunTimePlugin {

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

        //   mLogger.message("Got SSI message array of size " + array.size());

        for (final SSIEventEntry event_entry : array.list()) {
            final SSIEventData data = event_entry.getData();

            // Imagine the event was produced from address "coords@mouse" or "click@mouse"
            mLogger.message(" - sender: " +  event_entry.getSender() + // "mouse"
                    "\t event: " + event_entry.getEvent() + // "coords"
                    "\t from: " + event_entry.getFrom() + // an integer number (?)
                    "\t type: " + event_entry.getType() +  // TUPLE for coords, or EMPTY for clicks
                    "\t state: " + event_entry.getState() +  // "continued" for streamed coords and clicks down, or "completed" for clicks up.
                    "\t data: " + event_entry.getData() );
            String sender = event_entry.getSender() ;
            String event = event_entry.getEvent() ;
            if (sender.equals("ecg") && event.equals("stress")) {
                assert  event_entry.getData().equals("string");
                String tupledata = data.toString();
                String[] coordss = tupledata.split(",") ;
                //String[] coords =  tupledata.split("=", 4);
                String[] last_coordss_str = coordss[coordss.length-1].split("=") ;
                String[] last_coordss_strr = last_coordss_str[last_coordss_str.length-1].split("}") ;
                String[] last_coordss_str2 = coordss[coordss.length - 2].split("=") ;
                assert coordss.length == 3 ;
                Float stress = Float.parseFloat(last_coordss_str2[1]);
                Float neutral = Float.parseFloat(last_coordss_strr[0]);
                mLogger.message("The Detected: \t" + stress + "\t" + neutral);
                mProject.setVariable("stress", stress);
                mProject.setVariable("neutral", neutral);


            }
            else if (sender.equals("video") && event.equals("emotion")) {
                assert event_entry.getData().equals("string");
                String tupledataa = data.toString();
                // mLogger.message("new one " + tupledata);
                String[] coordsss = tupledataa.split(",");
                String[] last_coordss_str = coordsss[coordsss.length-1].split("=") ;
                String[] last_coordss_strr = last_coordss_str[last_coordss_str.length-1].split("}") ;
                String[] last_coordss_str2 = coordsss[coordsss.length - 2].split("=") ;
                assert coordsss.length == 3 ;
                //   String[] last_coordss_str = coordss[coordss.length - 1].split(",") ;
                // String[] last_coordss_str2 = coordss[coordss.length - 2].split(",") ;
                assert coordsss.length == 2;
                Float Pain = Float.parseFloat(last_coordss_str2[1]);
                //  String Pain = String.valueOf(coordsss[0]);
                Float NoPain = Float.parseFloat(last_coordss_strr[0]);
                //String No_Pain = String.valueOf(coordsss[1]);
                mLogger.message("The detected: \t" + Pain + "\t" + NoPain  );
                mProject.setVariable("Pain", Pain);
                mProject.setVariable("NoPain", NoPain);




            }

            else if (sender.equals("video") && event.equals("attention")) {
                assert event_entry.getData().equals("string");
                String tupledataa = data.toString();
                // mLogger.message("new one " + tupledata);
                String[] coordsss = tupledataa.split(",");
                String[] last_coordss_str = coordsss[coordsss.length-1].split("=") ;
                String[] last_coordss_strr = last_coordss_str[last_coordss_str.length-1].split("}") ;
                String[] last_coordss_str2 = coordsss[coordsss.length - 2].split("=") ;
                assert coordsss.length == 3 ;
                //   String[] last_coordss_str = coordss[coordss.length - 1].split(",") ;
                // String[] last_coordss_str2 = coordss[coordss.length - 2].split(",") ;
                assert coordsss.length == 2;
                Float Attention = Float.parseFloat(last_coordss_str2[1]);
                //  String Pain = String.valueOf(coordsss[0]);
                Float NoAttention = Float.parseFloat(last_coordss_strr[0]);
                //   String No_Pain = String.valueOf(coordsss[1]);
                mLogger.message("The detected: \t" + NoAttention);
                mProject.setVariable("Attention", Attention);
                mProject.setVariable("NoAttention", NoAttention);




            }
            else if (sender.equals("mouse") && event.equals("click")) {
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
