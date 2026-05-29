package de.dfki.vsm.model.project;

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.model.config.ConfigElement;
import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
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
    // Stable project identity for machine-local execution history tracking
    private String mProjectUUID;
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
    // Selected LLM services for authoring tools (generate, semantic)
    private final ConfigElement mLLMSelections;
    // Semantic service configuration (UD provider, endpoint, timeout)
    private final ConfigElement mSemanticServices;
    // Scene title concept list for scene title generator
    private final ConfigElement mSceneTitleConcepts;
    // Android project flag (portable/runtime-oriented project profile)
    private boolean mAndroidProject;

    // Construct an empty project
    public ProjectConfig() {
        // Initialize The Project Name
        mProjectName = "";
        // Generate a fresh UUID; overwritten by parseXML if the file already has one
        mProjectUUID = UUID.randomUUID().toString();
        // Initialize The Plugin List
        mPluginList = new ArrayList<>();
        // Initialize The Agent List
        mAgentList = new ArrayList<>();
        // Initialize The LLM List
        mLLMList = new ArrayList<>();
        // Initialize The LLM Prompts
        mLLMPrompts = new ConfigElement("LLMPrompts", "Feature");
        // Initialize The LLM selections
        mLLMSelections = new ConfigElement("LLMSelections", "Feature");
        // Initialize semantic services configuration
        mSemanticServices = new ConfigElement("SemanticServices", "Feature");
        // Initialize Scene Title Concepts
        mSceneTitleConcepts = new ConfigElement("SceneTitleConcepts", "Concept");
        // Initialize the player config
        mPlayerConfig = new PlayerConfig();
        // Initialize Android project flag
        mAndroidProject = false;
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
        this(name, plugins, agents, llms, new ConfigElement("LLMPrompts", "Feature"),
                new ConfigElement("SceneTitleConcepts", "Concept"), player);
    }

    // Construct a project with LLM configurations and LLM prompts
    public ProjectConfig(final String name,
                         final List<PluginConfig> plugins,
                         final List<AgentConfig> agents,
                         final List<LLMConfig> llms,
                         final ConfigElement llmPrompts,
                         final ConfigElement sceneTitleConcepts,
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
        // Initialize The LLM selections
        mLLMSelections = new ConfigElement("LLMSelections", "Feature");
        // Initialize semantic services configuration
        mSemanticServices = new ConfigElement("SemanticServices", "Feature");
        // Initialize Scene Title Concepts
        mSceneTitleConcepts = sceneTitleConcepts;
        // Initialize the player config
        mPlayerConfig = player;
        // Initialize Android project flag
        mAndroidProject = false;
    }

    // Legacy signature without scene title concepts
    public ProjectConfig(final String name,
                         final List<PluginConfig> plugins,
                         final List<AgentConfig> agents,
                         final List<LLMConfig> llms,
                         final ConfigElement llmPrompts,
                         final PlayerConfig player) {
        this(name, plugins, agents, llms, llmPrompts,
                new ConfigElement("SceneTitleConcepts", "Concept"), player);
    }

    public final String getProjectUUID() {
        return mProjectUUID;
    }

    public final void setProjectUUID(String uuid) {
        mProjectUUID = uuid;
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

    public final ConfigElement getLLMSelections() {
        return mLLMSelections;
    }

    public final ConfigElement getSceneTitleConcepts() {
        return mSceneTitleConcepts;
    }

    public final ConfigElement getSemanticServices() {
        return mSemanticServices;
    }

    public final boolean isAndroidProject() {
        return mAndroidProject;
    }

    public final void setAndroidProject(final boolean androidProject) {
        mAndroidProject = androidProject;
    }

    // Write the project configuration
    @Override
    public final void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        stream.println("<Project name=\"" + mProjectName + "\" androidProject=\"" + mAndroidProject + "\" uuid=\"" + mProjectUUID + "\">");
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
        if (!mLLMSelections.getEntryList().isEmpty()) {
            mLLMSelections.writeXML(stream);
            stream.endl();
        }
        if (!mSemanticServices.getEntryList().isEmpty()) {
            mSemanticServices.writeXML(stream);
            stream.endl();
        }
        if (!mSceneTitleConcepts.getEntryList().isEmpty()) {
            mSceneTitleConcepts.writeXML(stream);
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
            mAndroidProject = Boolean.parseBoolean(element.getAttribute("androidProject"));
            String uuid = element.getAttribute("uuid");
            if (uuid != null && !uuid.isBlank()) mProjectUUID = uuid;
            mPluginList.clear();
            mAgentList.clear();
            mLLMList.clear();
            mLLMPrompts.getEntryList().clear();
            mLLMSelections.getEntryList().clear();
            mSemanticServices.getEntryList().clear();
            mSceneTitleConcepts.getEntryList().clear();
            mPlayerConfig.getEntryList().clear();
            final java.util.Set<String> seenPlugins = new java.util.HashSet<>();
            final java.util.Set<String> seenAgents = new java.util.HashSet<>();
            final java.util.Set<String> seenLLMs = new java.util.HashSet<>();
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
                                    final String name = plugin.getPluginName() == null ? "" : plugin.getPluginName().trim().toLowerCase();
                                    if (!name.isEmpty() && !seenPlugins.add(name)) {
                                        return;
                                    }
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
                                    final String name = agent.getAgentName() == null ? "" : agent.getAgentName().trim().toLowerCase();
                                    if (!name.isEmpty() && !seenAgents.add(name)) {
                                        return;
                                    }
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
                                    final String name = llm.getLLMName() == null ? "" : llm.getLLMName().trim().toLowerCase();
                                    if (!name.isEmpty() && !seenLLMs.add(name)) {
                                        return;
                                    }
                                    mLLMList.add(llm);
                                }
                            });
                            break;
                        case "LLMPrompts":
                            mLLMPrompts.parseXML(element);
                            dedupeConfigElementByKey(mLLMPrompts);
                            break;
                        case "LLMSelections":
                            mLLMSelections.parseXML(element);
                            dedupeConfigElementByKey(mLLMSelections);
                            break;
                        case "SemanticServices":
                            mSemanticServices.parseXML(element);
                            dedupeConfigElementByKey(mSemanticServices);
                            break;
                        case "SceneTitleConcepts":
                            mSceneTitleConcepts.parseXML(element);
                            for (ConfigFeature feature : mSceneTitleConcepts.getEntryList()) {
                                String key = feature.getKey() == null ? "" : feature.getKey().trim();
                                String value = feature.getValue() == null ? "" : feature.getValue().trim();
                                if (value.isEmpty()) {
                                    continue;
                                }
                                if (key.isEmpty() || "name".equalsIgnoreCase(key)) {
                                    feature.setKey(value);
                                    feature.setValue(value);
                                }
                            }
                            dedupeConfigElementByKey(mSceneTitleConcepts);
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
        ConfigElement llmSelectionsCopy = new ConfigElement("LLMSelections", "Feature",
                mLLMSelections.copyEntryList());
        ConfigElement semanticServicesCopy = new ConfigElement("SemanticServices", "Feature",
                mSemanticServices.copyEntryList());
        ConfigElement sceneTitleConceptsCopy = new ConfigElement("SceneTitleConcepts", "Concept",
                mSceneTitleConcepts.copyEntryList());

        PlayerConfig player = getPlayerConfig().getCopy();

        ProjectConfig copy = new ProjectConfig(mProjectName, plugins, agents, llms, llmPromptsCopy,
                sceneTitleConceptsCopy, player);
        copy.setAndroidProject(mAndroidProject);
        copy.getLLMSelections().getEntryList().clear();
        copy.getLLMSelections().getEntryList().addAll(llmSelectionsCopy.copyEntryList());
        copy.getSemanticServices().getEntryList().clear();
        copy.getSemanticServices().getEntryList().addAll(semanticServicesCopy.copyEntryList());
        return copy;
    }

    private static void dedupeConfigElementByKey(final ConfigElement element) {
        final java.util.LinkedHashMap<String, ConfigFeature> byKey = new java.util.LinkedHashMap<>();
        for (ConfigFeature feature : element.getEntryList()) {
            final String key = feature.getKey() == null ? "" : feature.getKey().trim().toLowerCase();
            if (key.isEmpty()) {
                continue;
            }
            if (byKey.containsKey(key)) {
                byKey.remove(key);
            }
            byKey.put(key, feature);
        }
        element.getEntryList().clear();
        element.getEntryList().addAll(byKey.values());
    }
}
