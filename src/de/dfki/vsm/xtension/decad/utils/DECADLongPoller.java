package de.dfki.vsm.xtension.decad.utils;


import de.dfki.vsm.xtension.decad.DecadExecutor;
import de.dfki.vsm.xtension.decad.commands.IsSpeakingCommand;

import java.io.IOException;

public class DECADLongPoller {
    private boolean isCharacterSpeaking;

    public DECADLongPoller() {

    }

    public boolean pollIsSpeaking(DecadExecutor executor) {
        IsSpeakingCommand isSpeaking = new IsSpeakingCommand();
        try {
            return poll(executor, isSpeaking);
        } catch (IOException | InterruptedException e) {
            return true;
        }
    }

    private boolean poll(DecadExecutor executor, IsSpeakingCommand isSpeaking) throws IOException, InterruptedException {
        int counter = 0;
        while (!isCharacterSpeaking && counter <= 4) {
            isSpeaking.execute();
            String response = isSpeaking.getResponse();
            if (response.equals("1")) {
                isCharacterSpeaking = true;
            }
            Thread.sleep(20);
            counter++;
        }
        while (isCharacterSpeaking) {
            isSpeaking.execute();
            String response = isSpeaking.getResponse();
            if (!response.equals("1")) {
                isCharacterSpeaking = false;

            }
            Thread.sleep(20);
        }
        executor.handle();
        Thread.sleep(100);
        return true;
    }
}
