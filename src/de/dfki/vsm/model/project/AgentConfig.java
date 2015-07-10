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
 * @author Not me
 */
public class AgentConfig extends ConfigElement {

    // The Name Of The Agent
    private String mAgentName;
    // The Class Of The Agent
    private String mPlayerName;

    // Construct A New Agent
    public AgentConfig() {
        // Initialize The Config
        super("Agent", "Feature");
        // Initialize The Members
        mAgentName = new String();
        mPlayerName = new String();
    }

    // Construct A New Agent
    public AgentConfig(final String name, final String player) {
        // Initialize The Config
        super("Agent", "Feature");
        // Initialize The Members
        mAgentName = name;
        mPlayerName = player;
    }

    // Construct A New Agent
    public AgentConfig(
            final String name, final String player,
            final ArrayList<ConfigFeature> features) {
        // Initialize The Config
        super("Feature", "Agent", features);
        // Initialize The Members
        mAgentName = name;
        mPlayerName = player;
    }

    // Get Agent Name
    public final String getAgentName() {
        return mAgentName;
    }

    // Get Class Name
    public final String getClassName() {
        return mPlayerName;
    }

    // Write A Agent As XML
    @Override
    public final void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        stream.println("<Agent name=\"" + mAgentName + "\" player=\"" + mPlayerName + "\">");
        stream.push();
        for (final ConfigFeature entry : mFeatureList) {
            entry.writeXML(stream);
            stream.endl();
        }
        stream.pop().print("</Agent>").flush();
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
            mPlayerName = element.getAttribute("player");
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
        return new AgentConfig(mAgentName, mPlayerName, copyEntryList());
    }

}
