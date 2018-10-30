package de.dfki.vsm.xtension.tricatworld.xml.command;

import de.dfki.vsm.xtension.tricatworld.xml.command.object.TriCatWorldCmdObject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import org.w3c.dom.Element;

/**
 * @author Patrick Gebhard
 * @author Gregor Mehlmann
 */
public final class TriCatWorldCommand implements XMLParseable, XMLWriteable {

    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public final ArrayList<TriCatWorldCmdObject> mObjects = new ArrayList<>();

    public TriCatWorldCommand() {

    }

    public final void addObject(final TriCatWorldCmdObject object) {
        mObjects.add(object);
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) throws XMLWriteError {
        out.println("<TWorldCommand>").push();
        for (TriCatWorldCmdObject object : mObjects) {
            object.writeXML(out);
        }
        out.pop().println("</TWorldCommand>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        // Process The Child Nodes
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(final Element element) throws XMLParseError {
                final String name = element.getTagName();
                if (name.equalsIgnoreCase("Object")) {
                    final TriCatWorldCmdObject object = new TriCatWorldCmdObject();
                    object.parseXML(element);
                    mObjects.add(object);
                }
            }
        });
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
