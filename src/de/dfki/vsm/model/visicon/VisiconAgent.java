package de.dfki.vsm.model.visicon;

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
 * @author Not me
 */
public class VisiconAgent implements ModelObject {

    // The Action Entry Name
    private String mAgentName;
    private String mAgentIcon;

    // The Key Value Pairs
    private final ArrayList<VisiconViseme> mVisemeList;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public VisiconAgent() {
        mAgentName  = null;
        mAgentIcon  = null;
        mVisemeList = new ArrayList<>();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public VisiconAgent(final String name, final String icon, final ArrayList<VisiconViseme> list) {
        mAgentName  = name;
        mAgentIcon  = icon;
        mVisemeList = list;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final String getAgentName() {
        return mAgentName;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final String getAgentIcon() {
        return mAgentIcon;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void append(final VisiconViseme viseme) {
        mVisemeList.add(viseme);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void remove(final VisiconViseme viseme) {
        mVisemeList.remove(viseme);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final ArrayList<VisiconViseme> getVisemeList() {
        return mVisemeList;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final ArrayList<VisiconViseme> copyVisemeList() {

        // Construct A List Copy
        final ArrayList<VisiconViseme> copy = new ArrayList<>();

        // Copy Each Single Member
        for (final VisiconViseme entry : mVisemeList) {
            copy.add(entry.getCopy());
        }

        // Return The Final Clone
        return copy;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        stream.println("<Agent name=\"" + mAgentName + "\" icon=\"" + mAgentIcon + "\">");
        stream.push();

        for (final VisiconViseme viseme : mVisemeList) {
            viseme.writeXML(stream);
            stream.endl();
        }

        stream.pop();
        stream.print("</Agent>");
        stream.flush();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void parseXML(final Element element) throws XMLParseError {

        // Parse The Name
        mAgentName = element.getAttribute("name");

        // Parse The Icon
        mAgentIcon = element.getAttribute("icon");

        // Parse The Features
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(final Element element) throws XMLParseError {
                final String tag = element.getTagName();

                if (tag.equals("Gesture")) {

                    // Construct New
                    final VisiconViseme viseme = new VisiconViseme();

                    // Parse The Feature
                    viseme.parseXML(element);

                    // Append The Feature
                    append(viseme);
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

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public VisiconAgent getCopy() {
        return new VisiconAgent(mAgentName, mAgentIcon, copyVisemeList());
    }
}
