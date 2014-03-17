package de.dfki.vsm.util.log;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.SocketHandler;

/**
 * @author Gregor Mehlmann
 */
public class LOGSSISockHandler extends SocketHandler {

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public LOGSSISockHandler(final String host, final int port) throws IOException {
        // Install The Socket Handler
        super(host, port);
        // Install A New Console Formatter
        setFormatter(new LOGSSISockFormat());
        // Log The Messages From All Levels
        setLevel(Level.ALL);
    }
    
    
}
