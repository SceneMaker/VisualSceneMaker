package de.dfki.vsm.model.project;

import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.model.config.ConfigElement;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.util.ArrayList;
import org.w3c.dom.Element;

/**
 *
 * @author Gregor Mehlmann
 */
public class AgentConfig extends ConfigElement {

    // The Name Of The Agent
    private String mAgentName;
    // The Class Of The Agent
    private String mDeviceName;

    // Construct A New Agent
    public AgentConfig() {
        // Initialize The Config
        super("Agent", "Feature");
        // Initialize The Members
        mAgentName = new String();
        mDeviceName = new String();
    }

    // Construct A New Agent
    public AgentConfig(final String name, final String player) {
        // Initialize The Config
        super("Agent", "Feature");
        // Initialize The Members
        mAgentName = name;
        mDeviceName = player;
    }

    // Construct A New Agent
    public AgentConfig(
            final String name, final String player,
            final ArrayList<ConfigFeature> features) {
        // Initialize The Config
        super("Feature", "Agent", features);
        // Initialize The Members
        mAgentName = name;
        mDeviceName = player;
    }

    // Get Agent Name
    public final String getAgentName() {
        return mAgentName;
    }

    // Get Class Name
    public final String getDeviceName() {
        return mDeviceName;
    }

    // Write A Agent As XML
    @Override
    public final void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        stream.println("<Agent name=\"" + mAgentName + "\" device=\"" + mDeviceName + "\">").push();
        for (final ConfigFeature entry : mFeatureList) {
            entry.writeXML(stream);
            stream.endl();
        }
        stream.pop().println("</Agent>");
    }

    // Write A Agent From XML
    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        // Get The Type Of The Config
        final String tag = element.getTagName();
        // Check The Type Of The Config
        if (tag.equals("Agent")) {
            // Get The Attributes
            mAgentName = element.getAttribute("name");
            mDeviceName = element.getAttribute("device");
            // Parse The Entries
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

    // Get Copy Of Agent Config
    @Override
    public final AgentConfig getCopy() {
        return new AgentConfig(mAgentName, mDeviceName, copyEntryList());
    }

}
