package de.dfki.vsm.model.config;

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class ConfigFeature implements ModelObject, Comparable {

    // The Name Of The Feature
    private String mFeatureName;
    // The Key Of The Feature
    private String mFeatureKey;
    // The Value Of The Feature
    private String mFeatureValue;

    // Construct An  Entry
    public ConfigFeature(final String name) {
        // Initialize The Members
        mFeatureName = name;
        mFeatureKey = null;
        mFeatureValue = null;
    }

    // Construct An Entry
    public ConfigFeature(final String name, final String key, final String value) {
        // Initialize The Members
        mFeatureName = name;
        mFeatureKey = key;
        mFeatureValue = value;
    }

    // Get Name Of The Feature
    public final String getName() {
        return mFeatureName;
    }

    // Get Key Of The Feature
    public final String getKey() {
        return mFeatureKey;
    }

    // Get Value Of The Feature
    public final String getValue() {
        return mFeatureValue;
    }

    // Set Name Of The Feature
    public final void setName(final String name) {
        mFeatureName = name;
    }

    // Set Key Of The Feature
    public final void setKey(final String key) {
        mFeatureKey = key;
    }

    // Set Value Of The Feature
    public final void setValue(final String value) {
        mFeatureValue = value;
    }

    // Write As XML To Stream
    @Override
    public final void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        stream.print(toString()).flush();
    }

    // Parse As XML From Element
    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        // Get The Type Of The Config
        final String tag = element.getTagName();
        // Check The Type Of The Config
        if (tag.equals(mFeatureName)) {
            mFeatureKey = element.getAttribute("key");
            mFeatureValue = element.getAttribute("val");
        }
    }

    // Get String Representation
    @Override
    public final String toString() {
        return "<" + mFeatureName + " key=\"" + mFeatureKey + "\" val=\"" + mFeatureValue + "\"/>";
    }

    // Get A Copy Of This Entry
    @Override
    public final ConfigFeature getCopy() {
        return new ConfigFeature(mFeatureName, mFeatureKey, mFeatureValue);
    }

    // Compare Entry To Another
    @Override
    public final int compareTo(final Object obj) {
        if (obj instanceof ConfigFeature) {
            return mFeatureKey.compareTo(((ConfigFeature) obj).mFeatureKey);
        } else {
            return 0;
        }
    }
}
