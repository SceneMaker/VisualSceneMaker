package de.dfki.vsm.xtension.mindbotssi;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;

import de.dfki.vsm.xtension.ssi.SSIRunTimePlugin;
import de.dfki.vsm.xtension.ssi.event.SSIEventArray;
import de.dfki.vsm.xtension.ssi.event.SSIEventEntry;
import de.dfki.vsm.xtension.ssi.event.data.SSIEventData;
import de.dfki.vsm.xtension.ssi.event.data.SSITupleData;


/**
 * @author Fabrizio Nunnari
 */
public class MindBotSSIPlugin extends SSIRunTimePlugin {

    // Construct SSI plugin
    public MindBotSSIPlugin(
            final PluginConfig config,
            final RunTimeProject project) {
        super(config, project);
        mLogger.message("MindSSI plugin constructor...");

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
        mLogger.message("MindBotSSI Plugin unloaded.");
    }


    // Handle SSI event array
    @Override
    public void handle(final SSIEventArray array) {
        //   mLogger.message("Got SSI message array of size " + array.size());

        for (final SSIEventEntry event_entry : array.list()) {
            final SSIEventData data = event_entry.getData();

            // Print information about the received message
            // For example, the event was produced from address "coords@mouse" or "click@mouse"
            mLogger.message(" - sender: " +  event_entry.getSender() + // e.g.: "mouse"
                    "\t event: " + event_entry.getEvent() + // e.g.: "coords"
                    "\t from: " + event_entry.getFrom() + // an integer number (?)
                    "\t type: " + event_entry.getType() +  // "TUPLE" for coords, or "EMPTY" for clicks
                    "\t state: " + event_entry.getState() +  // "continued" for streamed coords and clicks down, or "completed" for clicks up.
                    "\t data: " + event_entry.getData() );

            String sender = event_entry.getSender() ;
            String event = event_entry.getEvent() ;

            if (sender.equals("ecg") && event.equals("stress")) {
                assert event_entry.getType().equals("MAP") ;
                SSITupleData tupleData = (SSITupleData) data ;
                float stressVal = Float.parseFloat(tupleData.get("stress")) ;
                float neutralVal = Float.parseFloat(tupleData.get("neutral")) ;
                mLogger.message("Got stress/neutral values: \t" + stressVal + "\t" + neutralVal);

                if(mProject.hasVariable("stress")) {
                    mProject.setVariable("stress", stressVal);
                }
                if(mProject.hasVariable("neutral")) {
                    mProject.setVariable("neutral", neutralVal);
                }
            }
            else if (sender.equals("video") && event.equals("emotion")) {
                assert event_entry.getType().equals("MAP") ;
                SSITupleData tupleData = (SSITupleData) data ;
                float painVal = Float.parseFloat(tupleData.get("pain")) ;
                float noPainVal = Float.parseFloat(tupleData.get("no pain")) ;
                mLogger.message("Got pain/no-pain values: \t" + painVal + "\t" + noPainVal);

                if(mProject.hasVariable("Pain")) {
                    mProject.setVariable("Pain", painVal);
                }
                if(mProject.hasVariable("NoPain")) {
                    mProject.setVariable("NoPain", noPainVal);
                }
            }
            else if (sender.equals("video") && event.equals("attention")) {
                assert event_entry.getType().equals("MAP") ;
                SSITupleData tupleData = (SSITupleData) data ;
                float attentionVal = Float.parseFloat(tupleData.get("attention")) ;
                float noAttentionVal = Float.parseFloat(tupleData.get("no attention")) ;
                mLogger.message("Got attention/no-attention values: \t" + attentionVal + "\t" + noAttentionVal);

                if(mProject.hasVariable("Attention")) {
                    mProject.setVariable("Attention", attentionVal);
                }
                if(mProject.hasVariable("NoAttention")) {
                    mProject.setVariable("NoAttention", noAttentionVal);
                }
            }

        }

    } // end handle()


}
