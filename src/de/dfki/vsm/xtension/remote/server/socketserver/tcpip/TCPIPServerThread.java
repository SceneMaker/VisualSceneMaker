package de.dfki.vsm.xtension.remote.server.socketserver.tcpip;




import de.dfki.vsm.xtension.remote.server.receiver.Receiver;
import de.dfki.vsm.xtension.remote.server.socketserver.ServerLoop;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

/**
 * Created by alvaro on 4/30/17.
 */
public class TCPIPServerThread extends ServerLoop {
    private final Socket socket;
    private String line = "";
    private BufferedReader is = null;
    private PrintWriter os = null;
    private Receiver receiver;

    public TCPIPServerThread(Socket socket, Receiver receiver) throws IOException {
        this.socket = socket;
        this.receiver = receiver;
        init(socket);

    }

    private void init(Socket socket) throws IOException {
        is = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        os = new PrintWriter(socket.getOutputStream());
    }


    @Override
    protected void receive() throws IOException {
        line = is.readLine();
        keepReadingData();
    }



    protected void close() throws IOException {
        socket.close();
        cleanup();
    }

    @Override
    protected void cleanup() {
        try {
            os.close();
            is.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void keepReadingData() throws IOException {
        while (notQuitCommand()) {
            receiver.receive(line.trim());
            System.out.println("TCP Message from Client  :  " + line);
            line = is.readLine();
        }
    }

    private boolean notQuitCommand() {
        return !line.equals("QUIT");
    }
}
