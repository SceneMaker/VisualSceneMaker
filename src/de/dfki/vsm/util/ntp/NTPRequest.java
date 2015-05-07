package de.dfki.vsm.util.ntp;

//~--- JDK imports ------------------------------------------------------------

import java.io.IOException;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;

import java.text.DecimalFormat;

public class NTPRequest {

    // private static final String sServerName = "atom.uhr.de";
    private static final String sServerName = "time.windows.com";

    public static String getGlobalTime(final boolean debug) throws IOException {

        // Create the request packet
        DatagramSocket socket  = new DatagramSocket();
        InetAddress    address = InetAddress.getByName(sServerName);
        byte[]         buf     = new NTPMessage().toByteArray();
        DatagramPacket packet  = new DatagramPacket(buf, buf.length, address, 123);

        // Create the transmit timestamp
        NTPMessage.encodeTimestamp(packet.getData(), 40, (System.currentTimeMillis() / 1000.0) + 2208988800.0);

        //
        if (debug) {
            System.err.println("Local Computer Time Long: " + System.currentTimeMillis());
        }

        // Send the time request
        socket.send(packet);

        // Get the time response
        packet = new DatagramPacket(buf, buf.length);
        socket.receive(packet);

        // Record the incoming timestamp
        double destinationTimestamp = (System.currentTimeMillis() / 1000.0) + 2208988800.0;

        // Process the response
        NTPMessage msg = new NTPMessage(packet.getData());

        // Compute round trip delay
        double roundTripDelay = (destinationTimestamp - msg.originateTimestamp)
                                - (msg.transmitTimestamp - msg.receiveTimestamp);

        // Compute local clock offset
        double localClockOffset = ((msg.receiveTimestamp - msg.originateTimestamp)
                                   + (msg.transmitTimestamp - destinationTimestamp)) / 2;

        // Display Response
        if (debug) {
            System.err.println("NTP Server URL: " + sServerName);
            System.err.println(msg.toString());
            System.err.println("Destination.timestamp:     " + NTPMessage.timestampToString(destinationTimestamp));
            System.err.println("Round-Trip Delay: " + new DecimalFormat("0.00").format(roundTripDelay * 1000) + " ms");
            System.err.println("Local Clock Offset: " + new DecimalFormat("0.00").format(localClockOffset * 1000)
                               + " ms");
            System.err.println("Global Server Time Date: " + NTPMessage.timestampToString(msg.receiveTimestamp));
            System.err.println("Global Server Time Long: " + NTPMessage.timestampToLong(msg.receiveTimestamp));
        }

        socket.close();

        //
        return NTPMessage.timestampToString(msg.receiveTimestamp);
    }
}
