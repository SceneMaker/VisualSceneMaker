package de.dfki.vsm.xtension.smartjacket;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;

import java.io.DataOutputStream;
import java.io.IOException;
import java.net.Socket;

public class SmartjacketExecutor extends ActivityExecutor {

    Socket writeSocket;
    int writePort = 7867;
    private DataOutputStream outStream;


    public SmartjacketExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    @Override
    public String marker(long id) {
        return null;
    }

    @Override
    public void execute(AbstractActivity activity) {
        if (activity instanceof SpeechActivity || activity == null) {
            return;
        }

        String cmd = "";
        switch (activity.getName()) {
            case "overtake":
                //TODO: replace placeholder
                cmd = "go";
        }
        if(!cmd.equals("")){
            try {
                outStream.writeChars("go");
                mLogger.message("send command \""+cmd+"\" to SmartJacket");
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

    }

    @Override
    public void launch() {
        while (writeSocket == null) {
            try {
                // Create the socket
                writeSocket = new Socket("localhost", writePort);
            } catch (final IOException exc) {
                mLogger.failure(exc.toString());
            }

        }
        while (outStream == null) {
            try {
                outStream = new DataOutputStream(writeSocket.getOutputStream());
                outStream.flush();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        mLogger.message("SmartJacket plugin ready");
    }


    public void onReceipt(String msg, Session session){
        session.getBasicRemote().sendText("got your message ");
    }

    @Override
    public void unload() {
        try {
            outStream.flush();
            outStream.close();
            writeSocket.close();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
