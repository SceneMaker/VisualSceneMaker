package de.dfki.vsm.xtension.tricatworld.xml.command.object;

import de.dfki.vsm.xtension.tricatworld.xml.command.object.action.MoveTo;
import de.dfki.vsm.xtension.tricatworld.xml.command.object.action.SoundAmbient;
import de.dfki.vsm.xtension.tricatworld.xml.command.object.action.charamel.CharamelAction;
import de.dfki.vsm.xtension.tricatworld.xml.command.object.action.AmbientLight;
import de.dfki.vsm.xtension.tricatworld.xml.command.object.action.TriCatWorldActObject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import java.io.ByteArrayOutputStream;
import org.w3c.dom.Element;

/**
 * @author Patrick Gebhard
 * @author Gregor Mehlmann
 */
public class TriCatWorldCmdObject implements XMLParseable, XMLWriteable {

    String mName;
    TriCatWorldActObject mAction;

    // Logger
    static final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public TriCatWorldCmdObject(String name, TriCatWorldActObject a) {
        mName = name;
        mAction = a;
    }

    public TriCatWorldCmdObject() {
    }

    public final void setAction(final TriCatWorldActObject action) {
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
                    if (actionName.equalsIgnoreCase("MovetoLocation")) {
                        mAction = new MoveTo();
                        mAction.parseXML(element);
                    } else if (actionName.equalsIgnoreCase("ambient_setup")) {
                        mAction = new AmbientLight();
                        mAction.parseXML(element);
                    } else if (actionName.equalsIgnoreCase("set_sound_ambient")) {
                        mAction = new SoundAmbient();
                        mAction.parseXML(element);
                    } else if (actionName.equalsIgnoreCase("caixml")) {
                        mAction = new CharamelAction();
                        mAction.parseXML(element);
                    }
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
