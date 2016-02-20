package de.dfki.vsm.players.server;

import de.dfki.vsm.players.EventActionPlayer;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class ServerConnectionHandler extends Thread {

    private Socket mClientSocket;
    private PrintWriter mOut;
    private BufferedReader mIn;
    private boolean mRunning = true;
    public String mClientId = "";

    private static final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    public ServerConnectionHandler(Socket s) {
        super.setName("Server Connection Handler");
        try {
            mClientSocket = s;

            mOut = new PrintWriter(mClientSocket.getOutputStream(), true);
            mIn = new BufferedReader(new InputStreamReader(mClientSocket.getInputStream()));
        } catch (IOException e) {
            mLogger.message("Client connection could not properly established " + e.getMessage());
        }
    }

    public void sendToApplication(String message) {
        //mLogger.message("Sending " + message);

        if (mClientSocket.isConnected()) {
            mOut.println(message.replace("\n", ""));

            mOut.flush();
        }
    }

    public void end() {
        //mLogger.message("Shutting down client connection ...");
        try {
            mClientSocket.shutdownInput();
            mClientSocket.shutdownOutput();
            mClientSocket.close();
            mRunning = false;
        } catch (IOException ex) {
            mLogger.failure("VSM ActionServer i/o exception during closing client connection");
        }
    }

    @Override
    public void run() {
        mLogger.message("VSM ActionServer listening to " + mClientSocket.toString());

        String input = "";
        StringBuilder tworldFeedback = new StringBuilder();
        boolean receivingTWorldFeedback = false;

        while (mRunning) {
            try {
                input = mIn.readLine();

                if (input != null) {
                    input = input.trim();
                    if (!input.isEmpty()) {
                        // DEBUG mLogger.message("Receiving " + input);
                        if (input.contains("CLIENTID")) {
                            int start = input.lastIndexOf("#") + 1;
                            mClientId = input.substring(start);
                        }

                        if (input.contains("#TM")) {
                            EventActionPlayer.getInstance().runActionAtTimeMark(input);
                        }
                        if (input.contains("#ANIM")) {
                            int start = input.lastIndexOf("#") + 1;
                            String animId = input.substring(start);
                            TCPActionServer.getInstance().notifyListeners(animId);
                        }
                        if (receivingTWorldFeedback) {
                            tworldFeedback.append(input).append("\n");
                            if (input.contains("</TWorldFeedback>")) {
                                mLogger.message("Received TWorld Feedback " + tworldFeedback);
                                // parse message
                                // if success extract id then send id 
                                String animID = "";
                                //TCPActionServer.getInstance().notifyListeners(animId);
                                receivingTWorldFeedback = false;
                                tworldFeedback = new StringBuilder();
                            }
                        }
                        if (input.contains("<TWorldFeedback>")) {
                            tworldFeedback.append(input).append("\n");
                            receivingTWorldFeedback = true;
                        }

                    }
                }
            } catch (IOException ex) {
                mLogger.failure("VSM ActionServer connection i/o exception!");
            }
        }
    }
}
