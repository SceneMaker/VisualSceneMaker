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
public final class ActiconAction implements ModelObject {

    // The Action Entry Name
    private String mActionName;

    // The Key Value Pairs
    private final ArrayList<ActiconFeature> mFeatureList;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public ActiconAction() {
        mActionName  = null;
        mFeatureList = new ArrayList<>();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public ActiconAction(final String name, final ArrayList<ActiconFeature> list) {
        mActionName  = name;
        mFeatureList = list;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final String getActionName() {
        return mActionName;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void append(final ActiconFeature feature) {
        mFeatureList.add(feature);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void remove(final ActiconFeature feature) {
        mFeatureList.remove(feature);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final ArrayList<ActiconFeature> getFeatureList() {
        return mFeatureList;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final ArrayList<ActiconFeature> copyFeatureList() {

        // Construct A List Copy
        final ArrayList<ActiconFeature> copy = new ArrayList<>();

        // Copy Each Single Member
        for (final ActiconFeature feature : mFeatureList) {
            copy.add(feature.getCopy());
        }

        // Return The Final Clone
        return copy;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final String toScript() {
        String text = "[ action " + mActionName + " ";

        for (final ActiconFeature feature : mFeatureList) {
            text += feature.toScript() + " ";
        }

        return text + "]";
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void writeXML(final IndentWriter stream) throws XMLWriteError {
        stream.println("<Action name=\"" + mActionName + "\">");
        stream.push();

        for (final ActiconFeature feature : mFeatureList) {
            feature.writeXML(stream);
            stream.endl();
        }

        stream.pop();
        stream.print("</Action>");
        stream.flush();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void parseXML(final Element element) throws XMLParseError {

        // Parse The Name
        mActionName = element.getAttribute("name");

        // Parse The Features
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(final Element element) throws XMLParseError {
                final String tag = element.getTagName();

                if (tag.equals("Feature")) {

                    // Construct New
                    final ActiconFeature feature = new ActiconFeature();

                    // Parse The Feature
                    feature.parseXML(element);

                    // Append The Feature
                    append(feature);
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
    public final ActiconAction getCopy() {
        return new ActiconAction(mActionName, copyFeatureList());
    }
}
