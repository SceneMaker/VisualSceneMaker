package de.dfki.vsm.util.xtensions.starter;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

/**
 * Created by alvaro on 6/20/17.
 */
public class CmdStreamNotifier implements Notifier {
    private final InputStream inputStream;
    private final String finishingLine;
    private InputStreamReader isr;
    private boolean started;

    public CmdStreamNotifier(InputStream inputStream, String finishingLine){
        this.inputStream = inputStream;
        this.finishingLine = finishingLine; //"started in";
        started = false;
        isr = new InputStreamReader(inputStream);
    }

    @Override
    public boolean waitForStartedNotification() {
        try{
            waitForNotification();
        }catch (IOException e){
            started = false;
        }finally {
            cleanup();
        }
        return started;

    }

    private void cleanup() {
        try {
            inputStream.close();
            isr.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void waitForNotification() throws IOException {
        String line;
        BufferedReader br = new BufferedReader(isr);
        while (!started) {
            line = br.readLine();
            if (line != null && line.contains(finishingLine)) {
                started = true;
            }
        }
    }
}
