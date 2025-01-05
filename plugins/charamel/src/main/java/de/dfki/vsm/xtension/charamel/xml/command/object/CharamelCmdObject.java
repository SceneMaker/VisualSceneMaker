package de.dfki.vsm.xtension.charamel.xml.command.object;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.*;
import de.dfki.vsm.xtension.charamel.xml.command.object.action.CharamelActObject;
import org.w3c.dom.Element;

import java.io.ByteArrayOutputStream;

/**
 * @author Patrick Gebhard
 * @author Gregor Mehlmann
 */
public class CharamelCmdObject implements XMLParseable, XMLWriteable {

    String mName;
    CharamelActObject mAction;

    // Logger
    static final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public CharamelCmdObject(String name, CharamelActObject a) {
        mName = name;
        mAction = a;
    }

    public CharamelCmdObject() {
    }

    public final void setAction(final CharamelActObject action) {
        mAction = action;
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<Object name=\"" + mName + "\">").push();
        mAction.writeXML(out);
        out.pop().pop().println("</Object>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        // Process The Child Nodes
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(final Element element) throws XMLParseError {
                final String name = element.getTagName();
                if (name.equalsIgnoreCase("Action")) {
                    String actionName = element.getAttribute("name");
//                    } else if (actionName.equalsIgnoreCase("caixml")) {
//                        mAction = new CharamelAction();
//                        mAction.parseXML(element);
//                    }
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
