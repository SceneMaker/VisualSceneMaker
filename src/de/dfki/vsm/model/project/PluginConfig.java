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
public class PluginConfig extends ConfigElement {

    // The Name Of The Plugin
    private String mPluginName;
    // The Class Of The Plugin
    private String mClassName;

    // Construct A New Plugin
    public PluginConfig() {
        // Initialize The Config
        super("Plugin", "Feature");
        // Initialize The Members
        mPluginName = new String();
        mClassName = new String();
    }

    // Construct A New Plugin
    public PluginConfig(final String name, final String clazz) {
        // Initialize The Config
        super("Plugin", "Feature");
        // Initialize The Members
        mPluginName = name;
        mClassName = clazz;
    }

    // Construct A New Plugin
    public PluginConfig(
            final String name, final String clazz,
            final ArrayList<ConfigFeature> features) {
        // Initialize The Config
        super("Plugin", "Feature", features);
        // Initialize The Members
        mPluginName = name;
        mClassName = clazz;
    }

    // Get Plugin Name
    public final String getPluginName() {
        return mPluginName;
    }

    // Get Class Name
    public final String getClassName() {
        return mClassName;
    }

    // Write A Plugin As XML
    @Override
    public final void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        stream.println("<Plugin name=\"" + mPluginName + "\" class=\"" + mClassName + "\">");
        stream.push();
        for (final ConfigFeature entry : mFeatureList) {
            entry.writeXML(stream);
            stream.endl();
        }
        stream.pop().print("</Plugin>").flush();
    }

    // Write A Plugin From XML
    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        // Get The Type Of The Config
        final String tag = element.getTagName();
        // Check The Type Of The Config
        if (tag.equals("Plugin")) {
            // Get The Attributes
            mPluginName = element.getAttribute("name");
            mClassName = element.getAttribute("class");
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

    // Get Copy Of Plugin Config
    @Override
    public final PluginConfig getCopy() {
        return new PluginConfig(mPluginName, mClassName, copyEntryList());
    }

}
