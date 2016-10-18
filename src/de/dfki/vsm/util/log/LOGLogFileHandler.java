package de.dfki.vsm.util.log;

//~--- JDK imports ------------------------------------------------------------

import java.io.IOException;

import java.util.logging.FileHandler;
import java.util.logging.Level;

/**
 * @author Gregor Mehlmann
 */
public class LOGLogFileHandler extends FileHandler {

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public LOGLogFileHandler(final String pattern, final int limit, final int count, final boolean append)
            throws IOException {
        super(pattern, limit, count, append);

        // Install A New Console Formatter
        setFormatter(new LOGLogFileFormat());

        // Log The Messages From All Levels
        setLevel(Level.ALL);
    
    }
}
