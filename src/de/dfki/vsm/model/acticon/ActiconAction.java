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
 * @author Not me
 */
public final class ActiconAction implements ModelObject {

    // The Action Name
    private String mActionName;

    // The Feature List
    private final ArrayList<ActiconFeature> mFeatureList;

    // Construct An Action
    public ActiconAction() {
        // Initialize The Action Name
        mActionName = null;
        // Initialize The Feature List
        mFeatureList = new ArrayList<>();
    }

    // Construct An Action
    public ActiconAction(final String name, final ArrayList<ActiconFeature> list) {
        // Initialize The Action Name
        mActionName = name;
        // Initialize The Feature List
        mFeatureList = list;
    }

    // Get The Action Name
    public final String getActionName() {
        return mActionName;
    }

    // Append A Feature
    public final void append(final ActiconFeature feature) {
        mFeatureList.add(feature);
    }

    // Remove A Feature
    public final void remove(final ActiconFeature feature) {
        mFeatureList.remove(feature);
    }

    // Get Feature List
    public final ArrayList<ActiconFeature> getFeatureList() {
        return mFeatureList;
    }

    // Copy Feature List
    public final ArrayList<ActiconFeature> copyFeatureList() {
        // Construct A List Copy
        final ArrayList<ActiconFeature> copy = new ArrayList<>();
        // Copy Each Single Member
        for (final ActiconFeature feature : mFeatureList) {
            copy.add(feature.getCopy());
        }
        // Return The Final Copy
        return copy;
    }

    // Get Script Representation
    public final String toScript() {
        String text = "[ action " + mActionName + " ";
        // Append The Features 
        for (final ActiconFeature feature : mFeatureList) {
            text += feature.toScript() + " ";
        }
        return text + "]";
    }

    // Write Action To XML
    @Override
    public final void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        stream.println("<Action name=\"" + mActionName + "\">");
        stream.push();
        // Write The Feature List
        for (final ActiconFeature feature : mFeatureList) {
            feature.writeXML(stream);
            stream.endl();
        }
        stream.pop();
        stream.print("</Action>");
        stream.flush();
    }

    // Parse Action From XML
    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        // Parse The Action Name
        mActionName = element.getAttribute("name");
        // Parse The Feature List
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(final Element element) throws XMLParseError {
                // Get The Tag Name
                final String tag = element.getTagName();
                // Check The Tag Name
                if (tag.equals("Feature")) {
                    // Construct A New Feature
                    final ActiconFeature feature = new ActiconFeature();
                    // Parse The New Feature
                    feature.parseXML(element);
                    // Append The New Feature
                    append(feature);
                }
            }
        });
    }

    // Get String Representation
    @Override
    public final String toString() {
        // Create A Byte Array Stream
        final ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        // Initialize The Indent Writer
        final IOSIndentWriter stream = new IOSIndentWriter(buffer);

        try {
            // Write The Action
            writeXML(stream);
        } catch (XMLWriteError exc) {
            // mLogger.failure(exc.toString());
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

    // Get A Copy Of The Action
    @Override
    public final ActiconAction getCopy() {
        return new ActiconAction(mActionName, copyFeatureList());
    }
}
