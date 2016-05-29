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

    // The type of the plugin
    private String mPluginType;
    // The name of the plugin
    private String mPluginName;
    // The class of the plugin
    private String mClassName;
    // The information if plugin should be loaded
    private boolean mLoad = true;

    // Construct A New Plugin
    public PluginConfig() {
        // Initialize The Config
        super("Plugin", "Feature");
        // Initialize The Members
        mPluginName = new String();
        mClassName = new String();
    }

        // Construct A New Plugin
    public PluginConfig(final String type, final String name, final String clazz, final boolean load) {
        // Initialize The Config
        super("Plugin", "Feature");
        // Initialize The Members
        mPluginType = type;
        mPluginName = name;
        mClassName = clazz;
        mLoad = load;
    }
    
    // Construct A New Plugin
    public PluginConfig(final String type, final String name, final String clazz) {
        // Initialize The Config
        super("Plugin", "Feature");
        // Initialize The Members
        mPluginType = type;
        mPluginName = name;
        mClassName = clazz;
        mLoad = true;
    }

    // Construct A New Plugin
    public PluginConfig(
            final String type,
            final String name,
            final String clazz,
            final boolean load,
            final ArrayList<ConfigFeature> features) {
        // Initialize The Config
        super("Plugin", "Feature", features);
        // Initialize The Members
        mPluginType = type;
        mPluginName = name;
        mClassName = clazz;
        mLoad = load;
    }

    // Get Plugin Name
    public final String getPluginType() {
        return mPluginType;
    }

    // Get Plugin Name
    public final String getPluginName() {
        return mPluginName;
    }

    // Get Class Name
    public final String getClassName() {
        return mClassName;
    }
    
        // Get Loading information
    public final boolean isMarkedtoLoad() {
        return mLoad;
    }

    public void setLoad(boolean load){
        mLoad = load;
    }

    // Write A Plugin As XML
    @Override
    public final void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        stream.println("<Plugin type=\"" + mPluginType + "\" name=\"" + mPluginName + "\" class=\"" + mClassName + "\" load=\"" + mLoad + "\">");
        stream.push();
        for (final ConfigFeature entry : mFeatureList) {
            entry.writeXML(stream);
            stream.endl();
        }
        stream.pop().println("</Plugin>");
    }

    // Write A Plugin From XML
    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        // Get The Type Of The Config
        final String tag = element.getTagName();
        // Check The Type Of The Config
        if (tag.equals("Plugin")) {
            // Get The Attributes
            mPluginType = element.getAttribute("type");
            mPluginName = element.getAttribute("name");
            mClassName = element.getAttribute("class");
            
            mLoad = (element.hasAttribute("load") ? Boolean.valueOf(element.getAttribute("load")) : true);
            
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
        return new PluginConfig(mPluginType, mPluginName, mClassName, mLoad, copyEntryList());
    }

}
