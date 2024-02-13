package de.dfki.vsm.xtension.ssj.event;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;

import java.io.ByteArrayOutputStream;

/**
 * @author Gregor Mehlmann
 */
public abstract class SSJEventObject implements XMLParseable, XMLWriteable
{

    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    // Get string representation
    @Override
    public final String toString()
    {
        final ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        final IOSIndentWriter writer = new IOSIndentWriter(buffer);
        try
        {
            writeXML(writer);
        }
        catch (final XMLWriteError exc)
        {
            mLogger.failure(exc.toString());
        }
        writer.flush();
        writer.close();
        try
        {
            //return buffer.toString("UTF-8");
            return buffer.toString();
        }
        catch (final Exception exc)
        {
            exc.printStackTrace();
            //
            return null;
        }
    }
}