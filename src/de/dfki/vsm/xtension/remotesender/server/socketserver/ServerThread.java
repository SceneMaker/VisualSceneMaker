package de.dfki.vsm.xtension.remotesender.server.socketserver;


import de.dfki.vsm.xtension.remotesender.server.receiver.Receiver;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

/**
 * Created by alvaro on 4/30/17.
 */
public class ServerThread extends Thread {
    private final Socket socket;
    private String line = "";
    private BufferedReader is = null;
    private PrintWriter os = null;
    private Receiver receiver;

    public ServerThread(Socket socket, Receiver receiver) throws IOException {
        this.socket = socket;
        this.receiver = receiver;
        init(socket);

    }

    private void init(Socket socket) throws IOException {
        is = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        os = new PrintWriter(socket.getOutputStream());
    }

    public void run() {
        try {
            line = is.readLine();
            keepReadingData();
        } catch (IOException e) {
            reportError();
        } finally {
            cleanup();
        }
    }

    private void cleanup() {
        try {
            os.close();
            is.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void reportError() {
        line = this.getName(); //reused String line for getting thread name
        System.out.println("IO Error/ Client " + line + " terminated abruptly");
    }

    private void keepReadingData() throws IOException {
        while (notQuitCommand()) {
            receiver.receive(line.trim());
            System.out.println("Response to Client  :  " + line);
            line = is.readLine();
        }
    }
    public void interrupt(){
        try {
            socket.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private boolean notQuitCommand() {
        return !line.equals("QUIT");
    }
}
