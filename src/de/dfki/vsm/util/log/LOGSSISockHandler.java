package de.dfki.vsm.util.log;

//~--- JDK imports ------------------------------------------------------------

import java.io.IOException;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.SocketAddress;

import java.util.logging.ErrorManager;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;

/**
 * @author Gregor Mehlmann
 */
public class LOGSSISockHandler extends Handler {
    private final DatagramSocket mSocket;
    private final SocketAddress  mAddress;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public LOGSSISockHandler(final String host, final int port) throws IOException {

        // Install The Socket Handler
        mSocket  = new DatagramSocket();
        mAddress = new InetSocketAddress(host, port);

        // Connect The Datagram Socket
        mSocket.connect(mAddress);

        // Install A New Console Formatter
        setFormatter(new LOGSSISockFormat());

        // Log The Messages From All Levels
        setLevel(Level.ALL);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void close() {

        // Close The Socket
        mSocket.close();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void flush() {

        // No Flushing Necessary For UDP Sockets
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void publish(final LogRecord record) {
        if (isLoggable(record)) {
            try {

                // Format The Message
                final Formatter formatter = getFormatter();
                final String    message   = formatter.format(record);
                byte[]          data      = message.getBytes();

                // Addressiere Paket und setze Daten
                final DatagramPacket packet = new DatagramPacket(data, data.length, mAddress);

                // Send The Paket
                mSocket.send(packet);
            } catch (Exception exc) {
                reportError(exc.getMessage(), exc, ErrorManager.WRITE_FAILURE);
            }
        }
    }
}
