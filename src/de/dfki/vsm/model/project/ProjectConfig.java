package de.dfki.vsm.model.project;

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.util.ios.IndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class ProjectConfig implements ModelObject {

    // Name Of The Project 
    private String mProjectName;
    // The List Of Plugins
    private final ArrayList<PluginConfig> mPluginList;
    // The List Of Players
    private final ArrayList<PlayerConfig> mPlayerList;
    // The List Of Agents
    private final ArrayList<AgentConfig> mAgentList;

    // Construct A Project
    public ProjectConfig() {
        // Initialize The Project Name
        mProjectName = null;
        // Initialize The Plugin List
        mPluginList = new ArrayList<>();
        // Initialize The Player List
        mPlayerList = new ArrayList<>();
        // Initialize The Agent List
        mAgentList = new ArrayList<>();
    }

    public final String getProjectName() {
        return mProjectName;
    }

    public final void setProjectName(final String name) {
        mProjectName = name;
    }

    public final ArrayList<AgentConfig> getAgentList() {
        return mAgentList;
    }

    public ArrayList<PlayerConfig> getPlayerList() {
        return mPlayerList;
    }

    public ArrayList<PluginConfig> getPluginList() {
        return mPluginList;
    }

    // Write Config As XML
    @Override
    public final void writeXML(final IndentWriter stream) throws XMLWriteError {
        stream.println("<Project name=\"" + mProjectName + "\">");
        stream.push();
        // Write The Plugins As XML
        stream.println("<Plugins>").push();
        for (final PluginConfig plugin : mPluginList) {
            plugin.writeXML(stream);
            stream.endl();
        }
        stream.pop().println("</Plugins>");
        // Write The Players As XML
        stream.println("<Players>").push();
        for (final PlayerConfig player : mPlayerList) {
            player.writeXML(stream);
            stream.endl();
        }
        stream.pop().println("</Players>");
        // Write The Agents As XML
        stream.println("<Agents>").push();
        for (final AgentConfig agent : mAgentList) {
            agent.writeXML(stream);
            stream.endl();
        }
        stream.pop().println("</Agents>");
        //
        stream.pop().print("</Project>").flush();
    }

    // Parse Config From XML
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
                    if (tag.equals("Plugins")) {
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
                    } else if (tag.equals("Players")) {
                        XMLParseAction.processChildNodes(element, "Player", new XMLParseAction() {
                            @Override
                            public void run(Element element) throws XMLParseError {
                                // Create A New Project Player
                                final PlayerConfig player = new PlayerConfig();
                                // And Parse The Project Player 
                                player.parseXML(element);
                                // And Add It To The Player List
                                mPlayerList.add(player);
                            }
                        });
                    } else if (tag.equals("Agents")) {
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
                    }
                }
            });
        }
    }

    // Get The String Representation
    @Override
    public final String toString() {
        // Create A Byte Array Stream
        final ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        // Initialize The Indent Writer
        final IndentWriter stream = new IndentWriter(buffer);

        try {

            // Write Object
            writeXML(stream);
        } catch (final XMLWriteError exc) {
            // mLogger.failure(exc.toString());
        }
        // Cleanup Stream and Writer
        stream.flush();
        stream.close();
        // Return String Representation
        try {
            return buffer.toString("UTF-8");
        } catch (Exception exc) {
            return buffer.toString();
        }
    }

    // Get A Copy Of The Configiguration
    @Override
    public ProjectConfig getCopy() {
        return null;
    }
}
