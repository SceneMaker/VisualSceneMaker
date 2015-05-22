package de.dfki.vsm.util.server;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.service.Service;

//~--- JDK imports ------------------------------------------------------------

import java.io.IOException;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.SocketException;
import java.net.UnknownHostException;

/**
 * @author Gregor Mehlmann
 */
public class UDPReceiver extends Listener {
    private final DatagramSocket mSocket;

    public UDPReceiver(ThreadGroup group, int port, Service service, Server server) throws SocketException {
        super(group, port, service, server);
        mSocket = new DatagramSocket();
        mSocket.setSoTimeout(60000);

        try {
            mSocket.connect(InetAddress.getLocalHost(), 343);
        } catch (UnknownHostException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void pleaseStop() {

        // TODO: NOT thread save!!!
        mStop = true;    // Set the stop flag
        interrupt();    // Stop blocking in accept()
        mSocket.close();
    }

    @Override
    public void run() {

        // Create a buffer to read datagrams into. If a
        // packet is larger than this buffer, the
        // excess will simply be discarded!
        byte[] buffer = new byte[1024];

        // Create a packet to receive data into the buffer
        DatagramPacket packet = new DatagramPacket(buffer, buffer.length);

        // Now loop forever, waiting to receive packets and printing them.
        while (!mStop) {
            try {

                // Wait to receive a datagram
                mSocket.receive(packet);
            } catch (IOException e) {

                // e.printStackTrace();
                mLogger.message("ServiceServer: UDPReceiver: " + e.getMessage());

                // mLogger.message(e.getMessage());
            }

            try {
                mService.serve(buffer, packet.getLength());
            } catch (IOException e) {

                // e.printStackTrace();
                mLogger.message("ServiceServer: UDPReceiver: " + e.getMessage());

                // mLogger.message(e.getMessage());
            }

            // Convert the contents to a string, and display them
            // String msg = new String(buffer, 0, packet.getLength());
            // System.out.println(packet.getAddress().getHostName() + ": " + msg);
            // Reset the length of the packet before reusing it.
            // packet.setLength(buffer.length);
        }
    }
}
