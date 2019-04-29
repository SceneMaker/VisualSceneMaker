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
 * @author Gregor Mehlmann
 */
public class PlayerConfig extends ConfigElement {

    // Construct A New Agent
    public PlayerConfig() {
        // Initialize The Config
        super("Player", "Feature");
    }

    // Construct A New Agent
    public PlayerConfig(
            final ArrayList<ConfigFeature> features) {
        // Initialize The Config
        super("Player", "Feature", features);
    }

    // Write A Agent As XML
    @Override
    public final void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        stream.println("<Player>").push();
        for (final ConfigFeature entry : mFeatureList) {
            entry.writeXML(stream);
            stream.endl();
        }
        stream.pop().println("</Player>");
    }

    // Write A Agent From XML
    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        // Get The Type Of The Config
        final String tag = element.getTagName();
        // Check The Type Of The Config
        if (tag.equals("Player")) {

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
    public final PlayerConfig getCopy() {
        return new PlayerConfig(copyEntryList());
    }
}