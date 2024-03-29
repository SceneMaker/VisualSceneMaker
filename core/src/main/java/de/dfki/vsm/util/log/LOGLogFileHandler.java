package de.dfki.vsm.util.log;

import de.dfki.vsm.Preferences;

import java.io.IOException;
import java.util.logging.FileHandler;

/**
 * @author Gregor Mehlmann
 */
public class LOGLogFileHandler extends FileHandler {

    public LOGLogFileHandler(final String pattern, final int limit, final int count, final boolean append)
            throws IOException {
        super(pattern, limit, count, append);
        // Install A New Console Formatter
        setFormatter(new LOGLogFileFormat());
        // Log The Messages From All Levels
        setLevel(Preferences.getLogLevel());
        // Set Logger Encoding To UTF-8
        try {
            setEncoding("UTF-8");
        } catch (final Exception exc) {
            exc.printStackTrace();
        }
    }
}
