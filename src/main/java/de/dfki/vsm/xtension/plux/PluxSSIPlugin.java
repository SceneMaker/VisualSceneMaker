package de.dfki.vsm.xtension.plux;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.ssi.SSIRunTimePlugin;
import de.dfki.vsm.xtension.ssi.event.SSIEventArray;

import java.util.Arrays;

import static java.lang.Math.sqrt;


public class PluxSSIPlugin extends SSIRunTimePlugin {

    int maxSpikes = 10;
    int[] spikes = new int[maxSpikes];
    int bufferSize = 2;
    int nbuffer = 0;
    int[] bufferedSpikes = new int[maxSpikes + 2 * bufferSize];
    int nSpike = 0;


    public PluxSSIPlugin(PluginConfig config, RunTimeProject project) {
        super(config, project);
        mLogger.message("PluxHandler started");
        if ((!mProject.hasVariable("Resp")) || (!mProject.hasVariable("HRV")))
            mLogger.failure("Plux Variables not properly declared");
    }


    public void handle(SSIEventArray array) {
        //mLogger.message("Plux handler handles"+array.toString());
        int i = 0;


        while (i < array.size()) {
            String event = array.get(i).getEvent();
            //SSIEventData data = array.get(i).getData();
            // mLogger.message("Event: "+event);
            if (event.equals("rspike")) {
                bufferedSpikes[nSpike++] = Integer.parseInt(array.get(i).getFrom());

                if (nSpike >= maxSpikes + 2 * bufferSize) {
                    this.calculateHRV();
                    nSpike = 0;

                }
            }
            i++;
        }

    }

    public void calculateHRV() {
        //mLogger.message("calculating HRV");
        Arrays.sort(bufferedSpikes);
        spikes = Arrays.copyOfRange(bufferedSpikes, bufferSize, maxSpikes + bufferSize);
        if (spikes.length != maxSpikes)
            mLogger.failure("spikes lenght invalid " + spikes.length + "instead of " + maxSpikes);
        int[] deltaT = new int[maxSpikes - 1];
        for (int i = 0; i < maxSpikes - 1; i++) {
            // mLogger.message("spikes: "+ spikes[i]);
            deltaT[i] = spikes[i + 1] - spikes[i];
        }
        //mLogger.message("spikes: "+ spikes[maxSpikes-1]);


        int ddT;
        int ddT2;
        int sum = 0;
        float hrv;
        for (int i = 0; i < maxSpikes - 2; i++) {
            // mLogger.message("dt: "+ deltaT[i]);
            ddT = deltaT[i + 1] - deltaT[i];
            ddT2 = ddT * ddT;
            sum += ddT2;
        }
        //mLogger.message("dt: "+ deltaT[maxSpikes-2]);

        //mLogger.message("sum: "+ sum);
        double smean = (double) sum / ((double) maxSpikes - 1.0);
        //mLogger.message("mean: "+ smean);
        hrv = (float) sqrt(smean);
        mLogger.message("calculated HRV of " + hrv);
        mProject.setVariable("HRV", hrv);
    }
}
