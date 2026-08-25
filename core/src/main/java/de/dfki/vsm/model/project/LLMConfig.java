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
 * Configuration for a named LLM service endpoint.
 * Stored as an &lt;LLM name="..."&gt; element inside the &lt;LLMs&gt; section of project.xml.
 *
 * Standard feature keys:
 * - baseUrl         : The OpenAI-compatible API base URL (e.g. http://localhost:8234/v1/)
 * - apiKey          : Optional API key for authentication
 * - model           : The selected model identifier
 * - temperature     : Default sampling temperature (0.0 - 2.0)
 * - timeout         : Request timeout in seconds
 * - disableThinking : Skip a thinking-capable model's reasoning pass (default true; set "false" to
 *                      let it think)
 * - reasoningEffort  : Reasoning effort for models that honor it, e.g. "low"/"medium"/"high"
 *                      (default "low"; "none" or blank omits the field)
 */
public class LLMConfig extends ConfigElement {

    private String mLLMName;

    public LLMConfig() {
        super("LLM", "Feature");
        mLLMName = "";
    }

    public LLMConfig(final String name) {
        super("LLM", "Feature");
        mLLMName = name;
    }

    public LLMConfig(final String name, final ArrayList<ConfigFeature> features) {
        super("LLM", "Feature", features);
        mLLMName = name;
    }

    public final String getLLMName() {
        return mLLMName;
    }

    public final void setLLMName(final String name) {
        mLLMName = name;
    }

    @Override
    public final void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        stream.println("<LLM name=\"" + mLLMName + "\">").push();
        for (final ConfigFeature entry : mFeatureList) {
            entry.writeXML(stream);
            stream.endl();
        }
        stream.pop().println("</LLM>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        final String tag = element.getTagName();
        if (tag.equals("LLM")) {
            mLLMName = element.getAttribute("name");
            mFeatureList.clear();
            XMLParseAction.processChildNodes(element, mFeatureName, new XMLParseAction() {
                @Override
                public void run(final Element element) throws XMLParseError {
                    final ConfigFeature entry = new ConfigFeature(mFeatureName);
                    entry.parseXML(element);
                    mFeatureList.add(entry);
                }
            });
        }
    }

    @Override
    public final LLMConfig getCopy() {
        return new LLMConfig(mLLMName, copyEntryList());
    }
}
