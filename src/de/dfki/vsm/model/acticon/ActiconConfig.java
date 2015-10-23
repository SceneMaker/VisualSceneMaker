package de.dfki.vsm.model.acticon;

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;

/**
 * @author Gregor Mehlmann
 */
public final class ActiconConfig implements ModelObject {

    // The Action List
    private final ArrayList<ActiconAction> mActionList;

    // Construct An Acticon
    public ActiconConfig() {
        // Initialize The Action List
        mActionList = new ArrayList<>();
    }

    // Construct An Acticon
    public ActiconConfig(final ArrayList<ActiconAction> list) {
        // Initialize The Action List
        mActionList = list;
    }

    // Append An Action
    public final void append(final ActiconAction entry) {
        mActionList.add(entry);
    }

    // Remove An Action
    public final void remove(final ActiconAction entry) {
        mActionList.remove(entry);
    }

    // Get The Action List
    public final ArrayList<ActiconAction> getActionList() {
        return mActionList;
    }

    // Copy The Action List
    public final ArrayList<ActiconAction> copyActionList() {
        // Construct A List Copy
        final ArrayList<ActiconAction> copy = new ArrayList<>();
        // Copy Each Single Action
        for (final ActiconAction action : mActionList) {
            copy.add(action.getCopy());
        }
        // Return The Final Copy
        return copy;
    }

    // Parse Action From XML
    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(final Element element) throws XMLParseError {
                // Get The Tag Name
                final String tag = element.getTagName();
                // Check The Tag Name
                if (tag.equals("Action")) {
                    // Construct A New Action
                    final ActiconAction action = new ActiconAction();
                    // Parse The New Action
                    action.parseXML(element);
                    // Append The New Action
                    append(action);
                }
            }
        });
    }

    // Write Action To XML
    @Override
    public final void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        stream.println("<Acticon>");
        stream.push();
        // Write The Action List
        for (ActiconAction action : mActionList) {
            action.writeXML(stream);
            stream.endl();
        }
        stream.pop();
        stream.print("</Acticon>");
        stream.flush();
    }

    // Get String Representation
    @Override
    public final String toString() {
        // Create A Byte Array Stream
        final ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        // Initialize The Indent Writer
        final IOSIndentWriter stream = new IOSIndentWriter(buffer);

        try {
            // Write The Acticon Now
            writeXML(stream);
        } catch (XMLWriteError exc) {
            //mLogger.failure(exc.toString());
        }
        // Cleanup Stream and Writer
        stream.flush();
        stream.close();
        // Get String Representation
        try {
            return buffer.toString("UTF-8");
        } catch (Exception exc) {
            return buffer.toString();
        }
    }

    // Get A Copy Of The Acticon
    @Override
    public final ActiconConfig getCopy() {
        return new ActiconConfig(copyActionList());
    }
}
