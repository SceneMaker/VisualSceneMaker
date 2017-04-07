package de.dfki.vsm.xtension.ssi;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.SocketException;

/**
 * @author Gregor Mehlmann
 */
public final class SSIUtilitiesSuite {

    private static long mInit = System.currentTimeMillis();
    // TODO: Make a ash map with timer values

    public static void init() {
        mInit = System.currentTimeMillis();
    }

    public static int time() {
        return Math.toIntExact(System.currentTimeMillis() - mInit);
    }

    public static void start() {
        try {
            final DatagramSocket socket = new DatagramSocket();
            // Try to receive new data
            String message = "SSI:STRT:RUN1\0";
            // Receive the datagram packet
            final byte[] buffer = message.getBytes("UTF-8");
            // Create the UDP packet
            final DatagramPacket packet
                    = new DatagramPacket(buffer, buffer.length, 
                            new InetSocketAddress("127.0.0.1", 1111));
            // And send the UDP packet
            socket.send(packet);
            socket.close();
            //
            System.err.println("Sending '" + message + "'");

        } catch (final IOException exc) {
            exc.printStackTrace();
        }
    }
    
      public static void stop() {
        try {
            final DatagramSocket socket = new DatagramSocket();
            // Try to receive new data
            String message = "SSI:STOP:RUN1\0";
            // Receive the datagram packet
            final byte[] buffer = message.getBytes("UTF-8");
            // Create the UDP packet
            final DatagramPacket packet
                    = new DatagramPacket(buffer, buffer.length, 
                            new InetSocketAddress("127.0.0.1", 1111));
            // And send the UDP packet
            socket.send(packet);
            socket.close();
            //
            System.err.println("Sending '" + message + "'");

        } catch (final IOException exc) {
            exc.printStackTrace();
        }
    }
}
