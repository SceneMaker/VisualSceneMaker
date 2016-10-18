package de.dfki.vsm.model.config;

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Collections;

/**
 * @author Gregor Mehlmann
 */
public class ConfigElement implements ModelObject {

    // The Element Name
    protected final String mElementName;
    // The Feature Type
    protected final String mFeatureName;
    // The Feature List
    protected final ArrayList<ConfigFeature> mFeatureList;

    // Construct An Element
    public ConfigElement(final String name, final String type) {
        // Initialize The Element Name
        mElementName = name;
        // Initialize The Feature Type
        mFeatureName = type;
        // Initialize The Feature List
        mFeatureList = new ArrayList<>();
    }

    // Construct An Element
    public ConfigElement(
            final String name, final String type,
            final ArrayList<ConfigFeature> list) {
        // Initialize The Element Name
        mElementName = name;
        // Initialize The Feature Type
        mFeatureName = type;
        // Initialize The Feature List
        mFeatureList = list;
    }

    // Get The Element Name 
    public final String getElementName() {
        return mElementName;
    }

    // Get The Feature Type Name 
    public final String getFeatureType() {
        return mFeatureName;
    }

    // Sort The Feature List
    public final void sort() {
        Collections.sort(mFeatureList);
    }

    // Append Some Feature
    public final void append(final ConfigFeature entry) {
        mFeatureList.add(entry);
    }

    // Remove Some Feature
    public final void remove(final ConfigFeature entry) {
        mFeatureList.remove(entry);
    }

    // Locate Some Feature
    public final boolean contains(final ConfigFeature entry) {
        return mFeatureList.contains(entry);
    }

    // Locate Some Feature
    public final boolean containsKey(final String key) {
        for (final ConfigFeature entry : mFeatureList) {
            if (entry.getKey().equals(key)) {
                return true;
            }
        }
        return false;
    }

    // Locate Some Feature
    public final boolean containsVal(final String val) {
        for (final ConfigFeature entry : mFeatureList) {
            if (entry.getValue().equals(val)) {
                return true;
            }
        }
        return false;
    }

    // Add A New Feature
    public final void addProperty(final String key, final String value) {
        mFeatureList.add(new ConfigFeature(mFeatureName, key, value));
    }

    // Get Some Feature
    public final String getProperty(final String key) {
        for (final ConfigFeature entry : mFeatureList) {
            if (entry.getKey().equals(key)) {
                return entry.getValue();
            }
        }
        return null;
    }

    // Set A Property Of A Given Key 
    public final void setProperty(final String key, final String value) {
        if (containsKey(key)) {
            for (final ConfigFeature entry : mFeatureList) {
                if (entry.getKey().equals(key)) {
                    entry.setValue(value);
                }
            }
        } else {
            addProperty(key, value);
        }
    }

    // Get The List Of Config Entries 
    public final ArrayList<ConfigFeature> getEntryList() {
        return mFeatureList;
    }

    // Copy The List Of Config Entries 
    public final ArrayList<ConfigFeature> copyEntryList() {
        // Construct A List Copy
        final ArrayList<ConfigFeature> copy = new ArrayList<>();
        // Copy Each Single Member
        for (final ConfigFeature entry : mFeatureList) {
            copy.add(entry.getCopy());
        }
        // Return The Final Clone
        return copy;
    }

    // Write The Config As XML To Stream 
    @Override
    public void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        stream.println("<" + mElementName + ">");
        stream.push();
        for (final ConfigFeature entry : mFeatureList) {
            entry.writeXML(stream);
            stream.endl();
        }
        stream.pop().print("</" + mElementName + ">").flush();
    }

    // Parse The Config As XML From Element 
    @Override
    public void parseXML(final Element element) throws XMLParseError {
        // Get The Type Of The Config
        final String tag = element.getTagName();
        // Check The Type Of The Config
        if (tag.equals(mElementName)) {
            // Parse The Individual Entries
            XMLParseAction.processChildNodes(element, mFeatureName, new XMLParseAction() {
                @Override
                public void run(final Element element) throws XMLParseError {
                    final ConfigFeature entry = new ConfigFeature(mFeatureName);
                    // Parse The New Entry Here
                    entry.parseXML(element);
                    // And Add It To The List
                    mFeatureList.add(entry);
                }
            });
        }
    }

    // Get The String Representation
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

    // Get A Copy Of The Configiguration
    @Override
    public ConfigElement getCopy() {
        return new ConfigElement(mFeatureName, mElementName, copyEntryList());
    }
}
