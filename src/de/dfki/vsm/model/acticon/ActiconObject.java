package de.dfki.vsm.model.acticon;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.util.ios.IndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------

import java.io.ByteArrayOutputStream;

import java.util.ArrayList;

/**
 * @author Gregor Mehlmann
 */
public class ActiconObject implements ModelObject {

    // The Action Entry List
    private final ArrayList<ActiconAction> mActionList;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public ActiconObject() {
        mActionList = new ArrayList<>();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public ActiconObject(final ArrayList<ActiconAction> list) {
        mActionList = list;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void append(final ActiconAction entry) {
        mActionList.add(entry);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void remove(final ActiconAction entry) {
        mActionList.remove(entry);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final ArrayList<ActiconAction> getActionList() {
        return mActionList;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final ArrayList<ActiconAction> copyActionList() {

        // Construct A List Copy
        final ArrayList<ActiconAction> copy = new ArrayList<>();

        // Copy Each Single Member
        for (final ActiconAction entry : mActionList) {
            copy.add(entry.getCopy());
        }

        // Return The Final Clone
        return copy;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(final Element element) throws XMLParseError {
                final String tag = element.getTagName();

                if (tag.equals("Action")) {

                    // Construct New
                    final ActiconAction action = new ActiconAction();

                    // Parse The Entry
                    action.parseXML(element);

                    // Append The Entry
                    append(action);
                }
            }
        });
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void writeXML(final IndentWriter stream) throws XMLWriteError {
        stream.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
        stream.println("<Acticon>");
        stream.push();

        for (ActiconAction action : mActionList) {
            action.writeXML(stream);
            stream.endl();
        }

        stream.pop();
        stream.print("</Acticon>");
        stream.flush();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final String toString() {

        // Create A Byte Array Stream
        final ByteArrayOutputStream buffer = new ByteArrayOutputStream();

        // Initialize The Indent Writer
        final IndentWriter stream = new IndentWriter(buffer);

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

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final ActiconObject getCopy() {
        return new ActiconObject(copyActionList());
    }
}
