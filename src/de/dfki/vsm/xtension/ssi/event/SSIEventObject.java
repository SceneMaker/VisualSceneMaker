package de.dfki.vsm.xtension.ssi.event;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import java.io.ByteArrayOutputStream;
import java.io.UnsupportedEncodingException;

/**
 * @author Gregor Mehlmann
 */
public abstract class SSIEventObject implements XMLParseable, XMLWriteable {

    // The singelton logger instance
    private final LOGConsoleLogger mLogger
            = LOGConsoleLogger.getInstance();
    
    // Get string representation
    @Override
    public final String toString() {
        final ByteArrayOutputStream stream = new ByteArrayOutputStream();
        final IOSIndentWriter writer = new IOSIndentWriter(stream);
        try {
            writeXML(writer);
        } catch (final XMLWriteError exc) {
            mLogger.failure(exc.toString());
        }
        writer.flush();
        writer.close();
        try {
            return stream.toString("UTF-8");
        } catch (final UnsupportedEncodingException exc) {
            mLogger.failure(exc.toString());
            return stream.toString();
        }
    }
}