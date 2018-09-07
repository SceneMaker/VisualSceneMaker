package de.dfki.vsm.xtension.charamel.xml.command.object.action;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import java.io.ByteArrayOutputStream;
import org.w3c.dom.Element;

/**
 *
 * @author Patrick Gebhard
 */
public class CharamelActObject implements XMLParseable, XMLWriteable {

    protected String mName = "";
    protected String mId = "";
    
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public void setId(String id) {
        mId = id;
    }

    public String getId() {
        return mId;
    }

    public String getActionCmd() {
        return mName;
    }

    public void resetActionCmd(String newcmdname) {
        mName = newcmdname;
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
    }
        @Override
    public final String toString() {
        // Create A Byte Array Stream
        final ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        // Initialize The Indent Writer
        final IOSIndentWriter stream = new IOSIndentWriter(buffer);
        try {
            // Write Object
            writeXML(stream);
        } catch (final XMLWriteError exc) {
            mLogger.failure(exc.toString());
        }
        // Cleanup Stream and Writer
        stream.flush();
        stream.close();
        // Return String Representation
       try {
            //return buffer.toString("UTF-8");
            return buffer.toString();
        } catch (final Exception exc) {
            exc.printStackTrace();
            //
            return null;
        }
    }
}
