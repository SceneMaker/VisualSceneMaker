package de.dfki.vsm.model.project;

import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.model.config.ConfigElement;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.util.ArrayList;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public class DeviceConfig extends ConfigElement {

    // The singelton logger instance
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    // The Name Of The Player
    private String mDeviceName;
    // The Class Of The Player
    private String mClassName;

    // Construct A New Player
    public DeviceConfig() {
        // Initialize The Config
        super("Device", "Feature");
        // Initialize The Members
        mDeviceName = new String();
        mClassName = new String();
    }

    // Construct A New Player
    public DeviceConfig(final String name, final String clazz) {
        // Initialize The Config
        super("Device", "Feature");
        // Initialize The Members
        mDeviceName = name;
        mClassName = clazz;
    }

    // Construct A New Player
    public DeviceConfig(
            final String name, final String clazz,
            final ArrayList<ConfigFeature> features) {
        // Initialize The Config
        super("Device", "Feature", features);
        // Initialize The Members
        mDeviceName = name;
        mClassName = clazz;
    }

    // Get Player Name
    public final String getDeviceName() {
        return mDeviceName;
    }

    // Get Class Name
    public final String getClassName() {
        return mClassName;
    }

    // Write A Player As XML
    @Override
    public final void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        stream.println("<Device name=\"" + mDeviceName + "\" class=\"" + mClassName + "\">").push();
        for (final ConfigFeature entry : mFeatureList) {
            entry.writeXML(stream);
        }
        stream.pop().println("</Device>");
    }

    // Write A Player From XML
    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        // Get The Type Of The Config
        final String tag = element.getTagName();
        // Check The Type Of The Config
        if (tag.equals("Device")) {
            // Get The Attributes
            mDeviceName = element.getAttribute("name");
            mClassName = element.getAttribute("class");
            // Parse The Entries
            XMLParseAction.processChildNodes(element, "Feature", new XMLParseAction() {
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

    // Get Copy Of Player Config
    @Override
    public final DeviceConfig getCopy() {
        return new DeviceConfig(mDeviceName, mClassName, copyEntryList());
    }

}
