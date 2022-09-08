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

                if(mProject.hasVariable("ssi-stress")) {
                    mProject.setVariable("ssi-stress", stressVal);
                }
                //if(mProject.hasVariable("neutral")) {
                //    mProject.setVariable("neutral", neutralVal);
                //}
            }
            else if (sender.equals("video") && event.equals("emotion")) {
                assert event_entry.getType().equals("MAP") ;
                SSITupleData tupleData = (SSITupleData) data ;
                mLogger.message("Got emotion+pain+var+arousal values: \t" + tupleData);

                // For each of the variables expected in this map, compose a corresponding
                // project variable name prepending the "ssi-emotion-" prefix
                String[] ssiVarNames = {"surprise","pain","happy","sad","neutral","valence","disgust","anger","fear","arousal"} ;
                for (String ssiVarName: ssiVarNames) {
                    float ssiVarValue = Float.parseFloat(tupleData.get(ssiVarName)) ;
                    String projectVarName = "ssi-emotion-" + ssiVarName ;
                    if(mProject.hasVariable(projectVarName)) {
                        mProject.setVariable(projectVarName, ssiVarValue);
                    }
                }

            }
            else if (sender.equals("video") && event.equals("focus")) {
                assert event_entry.getType().equals("MAP") ;
                SSITupleData tupleData = (SSITupleData) data ;
                mLogger.message("Got video focus values: \t" + tupleData);

                // For each of the variables expected in this map, compose a corresponding
                // project variable name prepending the "ssi-focus-" prefix
                String[] ssiVarNames = {"away","screen","device"} ;
                for (String ssiVarName: ssiVarNames) {
                    float ssiVarValue = Float.parseFloat(tupleData.get(ssiVarName)) ;
                    String projectVarName = "ssi-focus-" + ssiVarName ;
                    if(mProject.hasVariable(projectVarName)) {
                        mProject.setVariable(projectVarName, ssiVarValue);
                    }
                }

            }

        }

    } // end handle()

}
