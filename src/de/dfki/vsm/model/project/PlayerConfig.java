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

    // The Name Of The Player
    private String mPlayerName;
    // The Class Of The Player
    private String mClassName;

    // Construct A New Player
    public PlayerConfig() {
        // Initialize The Config
        super("Player", "Feature");
        // Initialize The Members
        // TODO: Take these from static preferences
        // and differntiate dialog and scene players 
        mPlayerName = new String();
        mClassName =new String();
    }

    // Construct A New Player
    public PlayerConfig(final String name, final String clazz) {
        // Initialize The Config
        super("Player", "Feature");
        // Initialize The Members
        mPlayerName = name;
        mClassName = clazz;
    }

    // Construct A New Player
    public PlayerConfig(
            final String name, final String clazz,
            final ArrayList<ConfigFeature> features) {
        // Initialize The Config
        super("Player", "Feature", features);
        // Initialize The Members
        mPlayerName = name;
        mClassName = clazz;
    }

    // Get Player Name
    public final String getPlayerName() {
        return mPlayerName;
    }

    // Get Class Name
    public final String getClassName() {
        return mClassName;
    }

    // Write A Player As XML
    @Override
    public final void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        stream.println("<Player name=\"" + mPlayerName + "\" class=\"" + mClassName + "\">");
        stream.push();
        for (final ConfigFeature entry : mFeatureList) {
            entry.writeXML(stream);
            stream.endl();
        }
        stream.pop().print("</Player>").flush();
    }

    // Write A Player From XML
    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        // Get The Type Of The Config
        final String tag = element.getTagName();
        // Check The Type Of The Config
        if (tag.equals("Player")) {
            // Get The Attributes
            mPlayerName = element.getAttribute("name");
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
    public final PlayerConfig getCopy() {
        return new PlayerConfig(mPlayerName, mClassName, copyEntryList());
    }

}
