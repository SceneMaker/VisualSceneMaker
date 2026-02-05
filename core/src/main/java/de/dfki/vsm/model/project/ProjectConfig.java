package de.dfki.vsm.model.project;

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.model.config.ConfigElement;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author Gregor Mehlmann
 */
public final class ProjectConfig implements ModelObject {

    // The singelton logger instance
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();
    // The name of the project  
    private String mProjectName;
    //
    private final PlayerConfig mPlayerConfig;
    // The list of plugin configurations
    private final List<PluginConfig> mPluginList;
    // The list of agent configurations
    private final List<AgentConfig> mAgentList;
    // The list of LLM configurations
    private final List<LLMConfig> mLLMList;
    // The LLM prompts configuration (formatPrompt, actionPrompt.0, etc.)
    private final ConfigElement mLLMPrompts;

    // Construct an empty project
    public ProjectConfig() {
        // Initialize The Project Name
        mProjectName = "";
        // Initialize The Plugin List
        mPluginList = new ArrayList<>();
        // Initialize The Agent List
        mAgentList = new ArrayList<>();
        // Initialize The LLM List
        mLLMList = new ArrayList<>();
        // Initialize The LLM Prompts
        mLLMPrompts = new ConfigElement("LLMPrompts", "Feature");
        // Initialize the player config
        mPlayerConfig = new PlayerConfig();
    }

    // Construct an empty project
    public ProjectConfig(final String name,
                         final List<PluginConfig> plugins,
                         final List<AgentConfig> agents,
                         final PlayerConfig player) {
        this(name, plugins, agents, new ArrayList<>(), player);
    }

    // Construct a project with LLM configurations
    public ProjectConfig(final String name,
                         final List<PluginConfig> plugins,
                         final List<AgentConfig> agents,
                         final List<LLMConfig> llms,
                         final PlayerConfig player) {
        this(name, plugins, agents, llms, new ConfigElement("LLMPrompts", "Feature"), player);
    }

    // Construct a project with LLM configurations and LLM prompts
    public ProjectConfig(final String name,
                         final List<PluginConfig> plugins,
                         final List<AgentConfig> agents,
                         final List<LLMConfig> llms,
                         final ConfigElement llmPrompts,
                         final PlayerConfig player) {
        // Initialize The Project Name
        mProjectName = name;
        // Initialize The Plugin List
        mPluginList = plugins;
        // Initialize The Agent List
        mAgentList = agents;
        // Initialize The LLM List
        mLLMList = llms;
        // Initialize The LLM Prompts
        mLLMPrompts = llmPrompts;
        // Initialize the player config
        mPlayerConfig = player;
    }

    // Get the name of the project
    public final String getProjectName() {
        return mProjectName;
    }

    // Set the name of the project
    public final void setProjectName(final String name) {
        mProjectName = name;
    }

    public final PlayerConfig getPlayerConfig() {
        return mPlayerConfig;
    }

    public final AgentConfig getAgentConfig(final String name) {
        for (final AgentConfig config : mAgentList) {
            if (config.getAgentName().equals(name)) {
                return config;
            }
        }
        return null;
    }

    public final PluginConfig getPluginConfig(final String name) {
        for (final PluginConfig config : mPluginList) {
            if (config.getPluginName().equals(name)) {
                return config;
            }
        }
        return null;
    }

    // Get the list of agent configurations
    public final List<AgentConfig> getAgentConfigList() {
        return mAgentList;
    }

    // Get the list of agent names (added PG - 8.4.2016)
    public final ArrayList<String> getAgentNames() {
        ArrayList<String> agentNames = new ArrayList<>();
        for (AgentConfig ac : getAgentConfigList()) {
            agentNames.add(ac.getAgentName());
        }
        return agentNames;
    }

    // Get the list of plugin configurations
    public List<PluginConfig> getPluginConfigList() {
        return mPluginList;
    }

    // Get the list of LLM configurations
    public final List<LLMConfig> getLLMConfigList() {
        return mLLMList;
    }

    // Get a specific LLM configuration by name
    public final LLMConfig getLLMConfig(final String name) {
        for (final LLMConfig config : mLLMList) {
            if (config.getLLMName().equals(name)) {
                return config;
            }
        }
        return null;
    }

    // Get the LLM prompts configuration
    public final ConfigElement getLLMPrompts() {
        return mLLMPrompts;
    }

    // Write the project configuration
    @Override
    public final void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        stream.println("<Project name=\"" + mProjectName + "\">");
        stream.push();
        // Write the plugin configurations
        stream.println("<Plugins>").push();
        for (final PluginConfig plugin : mPluginList) {
            plugin.writeXML(stream);
        }
        stream.pop().println("</Plugins>");
        // Write the agent configurations
        stream.println("<Agents>").push();
        for (final AgentConfig agent : mAgentList) {
            agent.writeXML(stream);
            //stream.endl();
        }
        stream.pop().println("</Agents>");
        // Write the LLM configurations
        if (!mLLMList.isEmpty()) {
            stream.println("<LLMs>").push();
            for (final LLMConfig llm : mLLMList) {
                llm.writeXML(stream);
            }
            stream.pop().println("</LLMs>");
        }
        // Write the LLM prompts configuration
        if (!mLLMPrompts.getEntryList().isEmpty()) {
            mLLMPrompts.writeXML(stream);
            stream.endl();
        }
        // Write the player configurations
        mPlayerConfig.writeXML(stream);
        stream.pop().print("</Project>").flush();
    }

    public boolean deleteDevice(PluginConfig plugin) {
        return mPluginList.remove(plugin);
    }

    public boolean deleteAgent(AgentConfig agent) {
        return mAgentList.remove(agent);
    }

    // Parse the project configuration
    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        // Get The Type Of The Config
        final String tag = element.getTagName();
        // Check The Type Of The Config
        if (tag.equals("Project")) {
            // Get The Project Name
            mProjectName = element.getAttribute("name");
            // Parse The Individual Entries
            XMLParseAction.processChildNodes(element, new XMLParseAction() {
                @Override
                public void run(final Element element) throws XMLParseError {
                    // Get The Tag Name
                    final String tag = element.getTagName();
                    // Check The Tag Name
                    switch (tag) {
                        case "Plugins":
                            XMLParseAction.processChildNodes(element, "Plugin", new XMLParseAction() {
                                @Override
                                public void run(Element element) throws XMLParseError {
                                    // Create A New Project Plugin
                                    final PluginConfig plugin = new PluginConfig();
                                    // And Parse The Project Plugin
                                    plugin.parseXML(element);
                                    // And Add It To The Plugin List
                                    mPluginList.add(plugin);
                                }
                            });
                            break;
                        case "Agents":
                            XMLParseAction.processChildNodes(element, "Agent", new XMLParseAction() {
                                @Override
                                public void run(Element element) throws XMLParseError {
                                    // Create A New Project Player
                                    final AgentConfig agent = new AgentConfig();
                                    // And Parse The Project Player
                                    agent.parseXML(element);
                                    // And Add It To The Player List
                                    mAgentList.add(agent);
                                }
                            });
                            break;
                        case "LLMs":
                            XMLParseAction.processChildNodes(element, "LLM", new XMLParseAction() {
                                @Override
                                public void run(Element element) throws XMLParseError {
                                    final LLMConfig llm = new LLMConfig();
                                    llm.parseXML(element);
                                    mLLMList.add(llm);
                                }
                            });
                            break;
                        case "LLMPrompts":
                            mLLMPrompts.parseXML(element);
                            break;
                        case "Player":
                            // Parse the player configuration
                            mPlayerConfig.parseXML(element);
                            break;
                    }
                }
            });
        }
    }

    // Get string representation
    @Override
    public final String toString() {
        final ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        final IOSIndentWriter writer = new IOSIndentWriter(buffer);
        try {
            writeXML(writer);
        } catch (final XMLWriteError exc) {
            mLogger.failure(exc.toString());
        }
        writer.flush();
        writer.close();
        try {
            //return buffer.toString("UTF-8");
            return buffer.toString();
        } catch (final Exception exc) {
            exc.printStackTrace();
            //
            return null;
        }
    }

    // Get a copy of the project configuration
    @Override
    public ProjectConfig getCopy() {
        List<PluginConfig> plugins = getPluginConfigList().stream()
                .map(PluginConfig::getCopy)
                .collect(Collectors.toList());

        List<AgentConfig> agents = getAgentConfigList().stream()
                .map(AgentConfig::getCopy)
                .collect(Collectors.toList());

        List<LLMConfig> llms = getLLMConfigList().stream()
                .map(LLMConfig::getCopy)
                .collect(Collectors.toList());

        ConfigElement llmPromptsCopy = new ConfigElement("LLMPrompts", "Feature",
                mLLMPrompts.copyEntryList());

        PlayerConfig player = getPlayerConfig().getCopy();

        return new ProjectConfig(mProjectName, plugins, agents, llms, llmPromptsCopy, player);
    }
}
