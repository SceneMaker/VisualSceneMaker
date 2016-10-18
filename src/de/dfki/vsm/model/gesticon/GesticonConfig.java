package de.dfki.vsm.model.gesticon;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------

import java.io.ByteArrayOutputStream;

import java.util.ArrayList;

/**
 * * @author Gregor Mehlmann
 */
public class GesticonConfig implements ModelObject {

    // The Agent Entry List
    private final ArrayList<GesticonAgent> mAgentList;

    public GesticonConfig() {
        mAgentList = new ArrayList<>();
    }

    public GesticonConfig(final ArrayList<GesticonAgent> list) {
        mAgentList = list;
    }

    public final void append(final GesticonAgent entry) {
        mAgentList.add(entry);
    }

    public final void remove(final GesticonAgent entry) {
        mAgentList.remove(entry);
    }

    public final ArrayList<GesticonAgent> getAgentList() {
        return mAgentList;
    }

    public final ArrayList<GesticonAgent> copyAgentList() {

        // Construct A List Copy
        final ArrayList<GesticonAgent> copy = new ArrayList<>();

        // Copy Each Single Member
        for (final GesticonAgent entry : mAgentList) {
            copy.add(entry.getCopy());
        }

        // Return The Final Clone
        return copy;
    }

    @Override
    public final void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        //stream.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
        stream.println("<Gesticon>");
        stream.push();

        for (final GesticonAgent agent : mAgentList) {
            agent.writeXML(stream);
            stream.endl();
        }

        stream.pop();
        stream.print("</Gesticon>");
        stream.flush();
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(final Element element) throws XMLParseError {
                final String tag = element.getTagName();

                if (tag.equalsIgnoreCase("Agent")) {
                    final GesticonAgent agent = new GesticonAgent();

                    agent.parseXML(element);
                    mAgentList.add(agent);
                }
            }
        });
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final String toString() {

        // Create A Byte Array Stream
        final ByteArrayOutputStream buffer = new ByteArrayOutputStream();

        // Initialize The Indent Writer
        final IOSIndentWriter stream = new IOSIndentWriter(buffer);

        try {

            // Write Object
            writeXML(stream);
        } catch (XMLWriteError exc) {

            // mLogger.failure(exc.toString());
        }

        // Cleanup Stream and Writer
        stream.flush();
        stream.close();

        // Return String Representation
        try {
            return buffer.toString("UTF-8");
        } catch (Exception exc) {
            return buffer.toString();
        }
    }

    @Override
    public GesticonConfig getCopy() {
        return new GesticonConfig(copyAgentList());
    }
}
