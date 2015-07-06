package de.dfki.vsm.model.project;

import de.dfki.vsm.model.acticon.ActiconObject;
import de.dfki.vsm.model.config.ConfigData;
import de.dfki.vsm.model.config.ConfigEntry;
import de.dfki.vsm.model.config.ProjectPreferences;
import de.dfki.vsm.model.gesticon.GesticonObject;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.script.SceneScript;
import de.dfki.vsm.model.visicon.VisiconObject;
import de.dfki.vsm.runtime.dialogact.DialogActInterface;
import de.dfki.vsm.runtime.dialogact.DummyDialogAct;
import de.dfki.vsm.runtime.player.DefaultDialogueActPlayer;
import de.dfki.vsm.runtime.player.Player;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.runtime.plugin.Plugin;
import de.dfki.vsm.util.xml.XMLUtilities;
import java.io.File;
import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;

/**
 * @author Gregor Mehlmann
 */
public class ProjectData implements Serializable {

    // The Logger Instance
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();
    // Project Information
    //private String mConfigFileName;
    private File mProjectDirFile;
    // Name Of The Project 
    private String mProjectName;
    // Project File Names
    private String mPluginsFileName;
    private String mPlayersFileName;
    private String mGesticonFileName;
    private String mVisiconFileName;
    private String mActiconFileName;
    private String mSceneFlowFileName;
    private String mSceneScriptFileName;
    private String mPreferencesFileName;
    // Maintained Structures
    private final SceneFlow mSceneFlow = new SceneFlow();
    private final SceneScript mSceneScript = new SceneScript();
    private final ActiconObject mActicon = new ActiconObject();
    private final GesticonObject mGesticon = new GesticonObject();
    private final VisiconObject mVisicon = new VisiconObject();
    // Project Configurations
    private final ConfigData mProjectConfig = new ConfigData("Project");
    private final ConfigData mPlayersConfig = new ConfigData("Players");
    private final ConfigData mPluginsConfig = new ConfigData("Plugins");
    private final ConfigData mPlayerConfig = new ConfigData("Player");
    // Maintained Plugins
    private final HashMap<String, Plugin> mPluginMap = new HashMap<>();
    // Maintained Players
    private final HashMap<String, Player> mPlayerMap = new HashMap<>();

    // Default DialogPlayer
    private Player mDialogPlayer;
    // TODO:  Refactor The Dialog Act Interface
    private DialogActInterface mDialogueAct;
    private String mDialogActClassName;
    private String mDialogPlayerClassName;

    // Default ScenePlayer
    private Player mScenePlayer;
    private String mScenePlayerClassName;
    private String mScenePlayerFileName;

    // Internal Information
    // TODO:  Refactor The Hash Initialization
    protected int mProjectInitialHash;
    // TODO:  Refactor The Project Properties
    private ProjectPreferences mProjectPreferences;

    // Freshly Created Flag
    private boolean mIsPending = false;

    ////////////////////////////////////////////////////////////////////////////
    public ProjectData(final File file) {
        // Init Project Path  
        mProjectDirFile = file.getAbsoluteFile();
        // Load The Project
        load();
        // TODO: Clean Up To Other Function
        // Make Analogous To Other Structures
        // Load Project Preferences
        mProjectPreferences = new ProjectPreferences();
        mProjectPreferences.load(mPreferencesFileName);
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized boolean isPending() {
        return mIsPending;
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void setPending(final boolean state) {
        mIsPending = state;
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized File getProjectDirFile() {
        return mProjectDirFile.getAbsoluteFile();
    }

    ////////////////////////////////////////////////////////////////////////////
    public void setProjectDirFile(final File file) {
        mProjectDirFile = file.getAbsoluteFile();
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized String getProjectDirName() {
        return mProjectDirFile.getAbsolutePath();
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized String getProjectName() {
        return mProjectName;
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized ActiconObject getActicon() {
        return mActicon;
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized VisiconObject getVisicon() {
        return mVisicon;
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized GesticonObject getGesticon() {
        return mGesticon;
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized SceneFlow getSceneFlow() {
        return mSceneFlow;
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized SceneScript getSceneScript() {
        return mSceneScript;
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized Player getScenePlayer() {
        return mScenePlayer;
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized Player getDialogPlayer() {
        return mDialogPlayer;
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized ProjectPreferences getPreferences() {
        return mProjectPreferences;
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized String getPreferencesFileName() {
        return mPreferencesFileName;
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized ConfigData getPlayerConfig() {
        return mPlayerConfig;
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized DialogActInterface getDialogAct() {
        return mDialogueAct;
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized Plugin getPlugin(final String name) {
        return mPluginMap.get(name);
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized Player getPlayer(final String name) {
        return mPlayerMap.get(name);
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void load() {
        loadProject();
        loadPlayers();
        loadPlugins();
        loadActicon();
        loadVisicon();
        loadGesticon();
        loadSceneFlow();
        loadSceneScript();
        // TODO: Clean
        loadScenePlayer();
        loadDialogPlayer();
        loadDialogueAct();
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void save() {
        saveProject();
        savePlayers();
        savePlugins();
        saveActicon();
        saveVisicon();
        saveGesticon();
        saveSceneFlow();
        saveSceneScript();
        // TODO: Clean
        saveScenePlayer();
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void loadProject() {
        // Create The Project Config File
        final File file = new File(mProjectDirFile, "project.xml");
        // Check The Project Config File
        if (file.exists()) {
            // Parse The Project Config File
            if (XMLUtilities.parseFromXMLFile(mProjectConfig, file)) {
                // Print Some Information
                mLogger.message("Success: Parsing Project Configuration '"
                        + file.getAbsolutePath() + "':\n" + mProjectConfig.toString());
                // Get Project Data
                mProjectName = mProjectConfig.property("project.name");
                mPluginsFileName = mProjectConfig.property("project.plugins");
                mPlayersFileName = mProjectConfig.property("project.players");
                mActiconFileName = mProjectConfig.property("project.acticon");
                mVisiconFileName = mProjectConfig.property("project.visicon");
                mGesticonFileName = mProjectConfig.property("project.gesticon");
                mSceneFlowFileName = mProjectConfig.property("project.sceneflow");
                mSceneScriptFileName = mProjectConfig.property("project.scenescript");

                // TODO: Clean
                // Added condition for legacy support for project independent preferences
                if (mProjectConfig.property("project.preferences") == null) {
                    mPreferencesFileName = "preferences.xml";
                } else {
                    mPreferencesFileName = mProjectConfig.property("project.preferences");
                }

                // Read Player Propertiesy   // TODO: Clean
                mScenePlayerClassName = mProjectConfig.property("project.player.default.class");
                mScenePlayerFileName = mProjectConfig.property("project.player.default.config");
                // Get Project ...   // TODO: Clean
                mDialogActClassName = mProjectConfig.property("project.dialogact.class");
                mDialogPlayerClassName = mProjectConfig.property("project.dialogact.player");
            } else {
                // Print Some Information
                mLogger.warning("Failure: Cannot Parse Project Configuration '"
                        + file.getAbsolutePath() + "'");
            }
        } else {
            // Print Some Information
            mLogger.failure("Failure: Cannot Find Project Configuration '"
                    + file.getAbsolutePath() + "'");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void saveProject() {
        // Create The Project Config File
        final File file = new File(mProjectDirFile, "project.xml");
        // Check The Project Config File
        if (file.exists()) {
            // Write The Project Config File
            if (XMLUtilities.writeToXMLFile(mProjectConfig, file)) {
                // Print Some Information
                mLogger.message("Success: Writing Project Configuration File '"
                        + file.getAbsolutePath() + "':\n" + mProjectConfig.toString());
            } else {
                // Print Some Information
                mLogger.failure("Failure: Cannot Write Project Configuration File '"
                        + file.getAbsolutePath() + "'");
            }
        } else {
            // Print Some Information
            mLogger.failure("Failure: Cannot Find Project Configuration File '"
                    + file.getAbsolutePath() + "'");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void loadPlayers() {
        // Check The Players Config File Name
        if (mPlayersFileName != null) {
            // Create The Players Config File
            final File file = new File(mProjectDirFile, mPlayersFileName);
            // Check The Players Config File
            if (file.exists()) {
                // Parse The Players Config File
                if (XMLUtilities.parseFromXMLFile(mPlayersConfig, file)) {
                    // Print Some Information 
                    mLogger.message("Success: Parsing Players Configuration '"
                            + file.getAbsolutePath() + "':\n" + mPlayersConfig.toString());
                    // Get The Individual Players
                    for (final ConfigEntry entry : mPlayersConfig.getEntryList()) {
                        // Get Name And File
                        final String key = ((String) entry.getKey());
                        final String val = ((String) entry.getVal());
                        // Get The Config File
                        final File base = new File(val);
                        // Check The Config File
                        if (base.exists()) {
                            // Get Plugin Config File
                            final ConfigData data = new ConfigData("Player");
                            // Parse The Config File
                            if (XMLUtilities.parseFromXMLFile(data, base)) {
                                // Print Some Information
                                mLogger.message("Success: Parsing Player Configuration '"
                                        + base.getAbsolutePath() + "':\n" + data.toString());
                                // Get Plugin Class Name
                                final String name = data.property("class");
                                // Check Plugin Class Name
                                if (name != null) {
                                    // Try To Load Plugin
                                    try {
                                        // Find The Class
                                        final Class clazz = Class.forName(name);
                                        // Get The Method
                                        final Method method = clazz.getMethod("getInstance", ProjectData.class, ConfigData.class);
                                        // Call The Method
                                        final Player player = (Player) method.invoke(null, this, data);
                                        // Add The Plugin
                                        mPlayerMap.put(key, player);
                                        // Print Some Information 
                                        mLogger.message("Success: Registering Player Name '"
                                                + key + "' With Player Object '" + player + "' And Config:\n" + data);
                                    } catch (Exception exc) {
                                        // Print Some Information 
                                        mLogger.failure("Failure: Cannot Initialize Player Of Class '" + name + "'");
                                        // Print Some Information 
                                        mLogger.failure(exc.toString());
                                    }
                                } else {
                                    // Print Some Information 
                                    mLogger.failure("Failure: Cannot Find Player Classpath Attribute In \n"
                                            + data.toString());
                                }
                            } else {
                                // Print Some Information 
                                mLogger.failure("Failure: Cannot Parse Player Configuration '"
                                        + base.getAbsolutePath() + "'");
                            }
                        } else {
                            // Print Some Information 
                            mLogger.failure("Failure: Cannot Find Player Configuration '"
                                    + base.getAbsolutePath() + "'");
                        }
                    }
                } else {
                    // Print Some Information 
                    mLogger.failure("Failure: Cannot Parse Players Configuration '"
                            + file.getAbsolutePath() + "'");
                }
            } else {
                // Print Some Information 
                mLogger.failure("Failure: Cannot Find Players Configuration '"
                        + file.getAbsolutePath() + "'");
            }
        } else {
            // Print Some Information 
            mLogger.failure("Failure: Invalid Players Configuration Filename");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void savePlayers() {
        // Check The Players Config File Name
        if (mPlayersFileName != null) {
            // Create The Players Config File
            final File file = new File(mProjectDirFile, mPlayersFileName);
            // Check The Players Config File
            if (file.exists()) {
                // Write The Players Config File
                if (XMLUtilities.writeToXMLFile(mPlayersConfig, file)) {
                    // Print Some Information
                    mLogger.failure("Success: Writing Players Configuration '"
                            + file.getAbsolutePath() + "':\n" + mPlayersConfig.toString());
                    // TODO: Save Player List
                } else {
                    // Print Some Information
                    mLogger.failure("Failure: Cannot Write Players Configuration '"
                            + file.getAbsolutePath() + "'");
                }
            } else {
                // Print Some Information
                mLogger.failure("Failure: Cannot Find Players Configuration '"
                        + file.getAbsolutePath() + "'");
            }
        } else {
            // Print Some Information 
            mLogger.failure("Failure: Invalid Players Configuration Filename");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void loadPlugins() {
        // Check The Plugins Config File Name
        if (mPluginsFileName != null) {
            // Create The Plugins Config File
            final File file = new File(mProjectDirFile, mPluginsFileName);
            // Check The Plugins Config File
            if (file.exists()) {
                // Parse The Plugins Config File
                if (XMLUtilities.parseFromXMLFile(mPluginsConfig, file)) {
                    // Print Some Information 
                    mLogger.message("Success: Parsing Plugins Configuration '"
                            + file.getAbsolutePath() + "':\n" + mPluginsConfig.toString());
                    // Get The Individual Plugins
                    for (final ConfigEntry entry : mPluginsConfig.getEntryList()) {
                        // Get Name And File
                        final String key = ((String) entry.getKey());
                        final String val = ((String) entry.getVal());
                        // Get The Config File
                        final File base = new File(val);
                        // Check The Config File
                        if (base.exists()) {
                            // Get Plugin Config File
                            final ConfigData data = new ConfigData("Plugin");
                            // Parse The Config File
                            if (XMLUtilities.parseFromXMLFile(data, base)) {
                                // Print Some Information
                                mLogger.message("Success: Parsing Plugin Configuration '"
                                        + base.getAbsolutePath() + "':\n" + data.toString());
                                // Get Plugin Class Name
                                final String name = data.property("class");
                                // Check Plugin Class Name
                                if (name != null) {
                                    // Try To Load Plugin
                                    try {
                                        // Find The Class
                                        final Class clazz = Class.forName(name);
                                        // Get The Method
                                        final Method method = clazz.getMethod("getInstance", ProjectData.class, ConfigData.class);
                                        // Call The Method
                                        final Plugin plugin = (Plugin) method.invoke(null, this, data);
                                        // Add The Plugin
                                        mPluginMap.put(key, plugin);
                                        // Print Some Information 
                                        mLogger.message("Success: Registering Plugin Name '"
                                                + key + "' With Plugin Object '" + plugin + "' And Config:\n" + data);
                                    } catch (Exception exc) {
                                        // Print Some Information 
                                        mLogger.failure("Failure: Cannot Initialize Plugin Of Class '" + name + "'");
                                        // Print Some Information 
                                        mLogger.failure(exc.toString());
                                    }
                                } else {
                                    // Print Some Information 
                                    mLogger.failure("Failure: Cannot Find Plugin Classpath Attribute In \n"
                                            + data.toString());
                                }
                            } else {
                                // Print Some Information 
                                mLogger.failure("Failure: Cannot Parse Plugin Configuration '"
                                        + base.getAbsolutePath() + "'");
                            }
                        } else {
                            // Print Some Information 
                            mLogger.failure("Failure: Cannot Find Plugin Configuration '"
                                    + base.getAbsolutePath() + "'");
                        }
                    }
                } else {
                    // Print Some Information 
                    mLogger.failure("Failure: Cannot Parse Players Configuration '"
                            + file.getAbsolutePath() + "'");
                }
            } else {
                // Print Some Information 
                mLogger.failure("Failure: Cannot Find Players Configuration '"
                        + file.getAbsolutePath() + "'");
            }
        } else {
            // Print Some Information 
            mLogger.failure("Failure: Invalid Plugins Configuration Filename");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void savePlugins() {
        // Check The Plugins Config File Name
        if (mPluginsFileName != null) {
            // Create The Plugins Config File
            final File file = new File(mProjectDirFile, mPluginsFileName);
            // Check The Plugins Config File
            if (file.exists()) {
                // Write The Plugins Config File
                if (XMLUtilities.writeToXMLFile(mPluginsConfig, file)) {
                    // Print Some Information
                    mLogger.message("Success: Writing Plugins Configuration '"
                            + file.getAbsolutePath() + "':\n" + mPluginsConfig.toString());
                    // TODO: Save Plugin List
                } else {
                    // Print Some Information
                    mLogger.failure("Failure: Cannot Write Plugins Configuration '"
                            + file.getAbsolutePath() + "'");
                }
            } else {
                // Print Some Information
                mLogger.failure("Failure: Cannot Find Plugins Configuration '"
                        + file.getAbsolutePath() + "'");
            }
        } else {
            // Print Some Information 
            mLogger.failure("Failure: Invalid Plugins Configuration Filename");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void loadSceneFlow() {
        // Check The SceneFlow File Name
        if (mSceneFlowFileName != null) {
            // Create The SceneFlow File
            final File file = new File(mProjectDirFile, mSceneFlowFileName);
            // Check The SceneFlow File
            if (file.exists()) {
                // Parse The SceneFlow File
                if (XMLUtilities.parseFromXMLFile(mSceneFlow, file)) {
                    // Postprocessing Steps
                    mSceneFlow.establishStartNodes();
                    mSceneFlow.establishTargetNodes();
                    mSceneFlow.establishAltStartNodes();
                    // Print Some Information
                    mLogger.message("Success: Parsing SceneFlow File '"
                            + file.getAbsolutePath() + "':\n" + mSceneFlow.toString());
                } else {
                    // Print Some Information
                    mLogger.failure("Failure: Cannot Parse SceneFlow File '"
                            + file.getAbsolutePath() + "'");
                }
            } else {
                // Print Some Information
                mLogger.failure("Failure: Cannot Find SceneFlow File '"
                        + file.getAbsolutePath() + "'");
            }
        } else {
            // Print Some Information 
            mLogger.failure("Failure: Invalid SceneFlow Filename");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void saveSceneFlow() {
        // Check The SceneFlow File Name
        if (mSceneFlowFileName != null) {
            // Create The SceneFlow File
            final File file = new File(mProjectDirFile, mSceneFlowFileName);
            // Check The SceneFlow File
            if (file.exists()) {
                // Write The SceneFlow File
                if (XMLUtilities.writeToXMLFile(mSceneFlow, file)) {
                    // Print Some Information
                    mLogger.message("Success: Writing SceneFlow File '"
                            + file.getAbsolutePath() + "':\n" + mSceneFlow.toString());
                } else {
                    // Print Some Information
                    mLogger.failure("Failure: Cannot Write SceneFlow File '"
                            + file.getAbsolutePath() + "'");
                }
            } else {
                // Print Some Information
                mLogger.failure("Failure: Cannot Find SceneFlow File '"
                        + file.getAbsolutePath() + "'");
            }
        } else {
            // Print Some Information 
            mLogger.failure("Failure: Invalid SceneFlow Filename");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void loadSceneScript() {
        // Check The SceneScript File Name
        if (mSceneScriptFileName != null) {
            // Create The SceneScript File
            final File file = new File(mProjectDirFile, mSceneScriptFileName);
            // Check The SceneScript File
            if (file.exists()) {
                // Parse The SceneScript File
                if (XMLUtilities.parseFromXMLFile(mSceneScript, file)) {
                    // Print Some Information
                    mLogger.message("Success: Parsing SceneScript File '"
                            + file.getAbsolutePath() + "':\n" + mSceneScript.toString());
                } else {
                    // Print Some Information
                    mLogger.failure("Failure: Cannot Parse SceneScript File '"
                            + file.getAbsolutePath() + "'");
                }
            } else {
                // Print Some Information
                mLogger.failure("Failure: Cannot Find SceneScript File '"
                        + file.getAbsolutePath() + "'");
            }
        } else {
            // Print Some Information 
            mLogger.failure("Failure: Invalid SceneScript Filename");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void saveSceneScript() {
        // Check The SceneScript File Name
        if (mSceneScriptFileName != null) {
            // Create The SceneScript File
            final File file = new File(mProjectDirFile, mSceneScriptFileName);
            // Check The SceneScript File
            if (file.exists()) {
                // Write The SceneScript File
                if (XMLUtilities.writeToXMLFile(mSceneScript, file)) {
                    // Print Some Information
                    mLogger.message("Success: Writing SceneScript File '"
                            + file.getAbsolutePath() + "':\n" + mSceneScript.toString());
                } else {
                    // Print Some Information
                    mLogger.failure("Failure: Cannot Write SceneScript File '"
                            + file.getAbsolutePath() + "'");
                }
            } else {
                // Print Some Information
                mLogger.failure("Failure: Cannot Find SceneScript File '"
                        + file.getAbsolutePath() + "'");
            }
        } else {
            // Print Some Information 
            mLogger.failure("Failure: Invalid SceneScript Filename");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void loadActicon() {
        // Check The Acticon File Name
        if (mActiconFileName != null) {
            // Create The Acticon File
            final File file = new File(mProjectDirFile, mActiconFileName);
            // Check The Acticon File
            if (file.exists()) {
                // Parse The Acticon File
                if (XMLUtilities.parseFromXMLFile(mActicon, file)) {
                    // Print Some Information
                    mLogger.message("Success: Parsing Acticon File '"
                            + file.getAbsolutePath() + "':\n" + mActicon.toString());
                } else {
                    // Print Some Information
                    mLogger.failure("Failure: Cannot Parse Acticon File '"
                            + file.getAbsolutePath() + "'");
                }
            } else {
                // Print Some Information
                mLogger.failure("Failure: Cannot Find Acticon File '"
                        + file.getAbsolutePath() + "'");
            }
        } else {
            // Print Some Information 
            mLogger.failure("Failure: Invalid Acticon Filename");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void saveActicon() {
        // Check The Acticon File Name
        if (mActiconFileName != null) {
            // Create The Acticon File
            final File file = new File(mProjectDirFile, mActiconFileName);
            // Check The Acticon File
            if (file.exists()) {
                // Write The Acticon File
                if (XMLUtilities.writeToXMLFile(mActicon, file)) {
                    // Print Some Information
                    mLogger.message("Success: Writing Acticon File '"
                            + file.getAbsolutePath() + "':\n" + mActicon.toString());
                } else {
                    // Print Some Information
                    mLogger.failure("Failure: Cannot Write Acticon File '"
                            + file.getAbsolutePath() + "'");
                }
            } else {
                // Print Some Information
                mLogger.failure("Failure: Cannot Find Acticon File '"
                        + file.getAbsolutePath() + "'");
            }
        } else {
            // Print Some Information 
            mLogger.failure("Failure: Invalid Acticon Filename");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void loadGesticon() {
        // Check The Gesticon File Name
        if (mGesticonFileName != null) {
            // Create The Gesticon File
            final File file = new File(mProjectDirFile, mGesticonFileName);
            // Check The Gesticon File
            if (file.exists()) {
                // Parse The Gesticon File
                if (XMLUtilities.parseFromXMLFile(mGesticon, file)) {
                    // Print Some Information
                    mLogger.message("Success: Parsing Gesticon File '"
                            + file.getAbsolutePath() + "':\n" + mGesticon.toString());
                } else {
                    // Print Some Information
                    mLogger.failure("Failure: Cannot Parse Gesticon File '"
                            + file.getAbsolutePath() + "'");
                }
            } else {
                // Print Some Information
                mLogger.failure("Failure: Cannot Find Gesticon File '"
                        + file.getAbsolutePath() + "'");
            }
        } else {
            // Print Some Information 
            mLogger.failure("Failure: Invalid Gesticon Filename");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void saveGesticon() {
        // Check The Gesticon File Name
        if (mGesticonFileName != null) {
            // Create The Gesticon File
            final File file = new File(mProjectDirFile, mGesticonFileName);
            // Check The Gesticon File
            if (file.exists()) {
                // Write The Gesticon File
                if (XMLUtilities.writeToXMLFile(mGesticon, file)) {
                    // Print Some Information
                    mLogger.message("Success: Writing Gesticon File '"
                            + file.getAbsolutePath() + "':\n" + mGesticon.toString());
                } else {
                    // Print Some Information
                    mLogger.failure("Failure: Cannot Write Gesticon File '"
                            + file.getAbsolutePath() + "'");
                }
            } else {
                // Print Some Information
                mLogger.failure("Failure: Cannot Find Gesticon File '"
                        + file.getAbsolutePath() + "'");
            }
        } else {
            // Print Some Information 
            mLogger.failure("Failure: Invalid Gesticon Filename");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void loadVisicon() {
        // Check The Visicon File Name
        if (mVisiconFileName != null) {
            // Create The Visicon File
            final File file = new File(mProjectDirFile, mVisiconFileName);
            // Check The Visicon File
            if (file.exists()) {
                // Parse The Visicon File
                if (XMLUtilities.parseFromXMLFile(mVisicon, file)) {
                    // Print Some Information
                    mLogger.message("Success: Parsing Visicon File '"
                            + file.getAbsolutePath() + "':\n" + mVisicon.toString());
                } else {
                    // Print Some Information
                    mLogger.failure("Failure: Cannot Parse Visicon File '"
                            + file.getAbsolutePath() + "'");
                }
            } else {
                // Print Some Information
                mLogger.failure("Failure: Cannot Find Visicon File '"
                        + file.getAbsolutePath() + "'");
            }
        } else {
            // Print Some Information 
            mLogger.failure("Failure: Invalid Visicon Filename");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void saveVisicon() {
        // Check The Visicon File Name
        if (mVisiconFileName != null) {
            // Create The Visicon File
            final File file = new File(mProjectDirFile, mVisiconFileName);
            // Check The Visicon File
            if (file.exists()) {
                // Write The Visicon File
                if (XMLUtilities.writeToXMLFile(mVisicon, file)) {
                    // Print Some Information
                    mLogger.message("Success: Writing Visicon File '"
                            + file.getAbsolutePath() + "':\n" + mVisicon.toString());
                } else {
                    // Print Some Information
                    mLogger.failure("Failure: Cannot Write Visicon File '"
                            + file.getAbsolutePath() + "'");
                }
            } else {
                // Print Some Information
                mLogger.failure("Failure: Cannot Find Visicon File '"
                        + file.getAbsolutePath() + "'");
            }
        } else {
            // Print Some Information 
            mLogger.failure("Failure: Invalid Visicon Filename");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void loadScenePlayer() {
        // Check The Scene Player File Name
        if (mScenePlayerFileName != null) {
            // Create The Scene Player File
            final File file = new File(mProjectDirFile, mScenePlayerFileName);
            // Check The Scene Player File
            if (file.exists()) {
                // Parse The Scene Player File
                if (XMLUtilities.parseFromXMLFile(mPlayerConfig, file)) {
                    // Print Some Information
                    mLogger.message("Success: Parsing ScenePlayer File '"
                            + file.getAbsolutePath() + "':\n" + mPlayerConfig.toString());
                    // Initialize Scene Player
                    try {
                        // Find The Scene Player Class
                        final Class clazz = Class.forName(mScenePlayerClassName);
                        // Get The Initialization Method
                        final Method method = clazz.getMethod("getInstance", ProjectData.class, ConfigData.class);
                        // Call The Initialization Method
                        final Player player = (Player) method.invoke(null, this, mPlayerConfig);
                        // Add The Scene Player To List
                        mPlayerMap.put("default", player);
                        // Print Some Information 
                        mLogger.message("Success: Registering Player Name '"
                                + "default" + "' With Player Object '" + player + "' And Config:\n" + mPlayerConfig);
                    } catch (Exception exc) {
                        // Print Some Information 
                        mLogger.failure("Failure: Cannot Initialize Player Of Class '" + mScenePlayerClassName + "'");
                        // Print Some Information 
                        mLogger.failure(exc.toString());
                    }

                } else {
                    // Print Some Information
                    mLogger.failure("Failure: Cannot Parse ScenePlayer File '"
                            + file.getAbsolutePath() + "'");
                }
            } else {
                // Print Some Information
                mLogger.failure("Failure: Cannot Find ScenePlayer File '"
                        + file.getAbsolutePath() + "'");
            }
        } else {
            // Print Some Information 
            mLogger.failure("Failure: Invalid ScenePlayer Filename");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void saveScenePlayer() {
        // Check The ScenePlayer File Name
        if (mScenePlayerFileName != null) {
            // Create The ScenePlayer File
            final File file = new File(mProjectDirFile, mScenePlayerFileName);
            // Check The ScenePlayer File
            if (file.exists()) {
                // Write The ScenePlayer File
                if (XMLUtilities.writeToXMLFile(mPlayerConfig, file)) {
                    // Print Some Information
                    mLogger.message("Success: Writing ScenePlayer File '"
                            + file.getAbsolutePath() + "':\n" + mPlayerConfig.toString());
                } else {
                    // Print Some Information
                    mLogger.failure("Failure: Cannot Write ScenePlayer File '"
                            + file.getAbsolutePath() + "'");
                }
            } else {
                // Print Some Information
                mLogger.failure("Failure: Cannot Find ScenePlayer File '"
                        + file.getAbsolutePath() + "'");
            }
        } else {
            // Print Some Information 
            mLogger.failure("Failure: Invalid ScenePlayer Filename");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void launchPlayerList() {
        // Launch All Players
        for (final Player player : mPlayerMap.values()) {
            // Launch The Player
            player.launch();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void unloadPlayerList() {
        // Unload All Players
        for (final Player player : mPlayerMap.values()) {
            // Unload The Player
            player.unload();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void launchPluginList() {
        // Launch All Plugins
        for (final Plugin plugin : mPluginMap.values()) {
            // Launch The Plugin
            plugin.launch();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void unloadPluginList() {
        // Unload All Plugins
        for (final Plugin plugin : mPluginMap.values()) {
            // Unload The Plugin
            plugin.unload();
        }
    }

    // TODO: Does actually not work correct!
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized boolean hasChanged() {
        boolean hasChanged = false;

        if (getSceneInitialHash() != getHashCode()) {
            hasChanged = true;
        }

        return hasChanged;
    }

    public final synchronized void loadDialogueAct() {
        if (mDialogActClassName != null) {
            try {
                Class daClass = Class.forName(mDialogActClassName);

                mDialogueAct = (DialogActInterface) daClass.getConstructor().newInstance();
            } catch (Exception exc) {

                // do nothing
            }
        } else {
            mDialogueAct = new DummyDialogAct();
        }
    }

    public final synchronized void loadDialogPlayer() {
        if (mDialogPlayerClassName != null) {
            try {
                Class daPlayerClass = Class.forName(mDialogPlayerClassName);

                mDialogPlayer
                        = (Player) daPlayerClass.getConstructor(ProjectData.class).newInstance(this);
            } catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | ClassNotFoundException | NoSuchMethodException | SecurityException ex) {
                mDialogPlayer = new DefaultDialogueActPlayer(this);
            }
        } else {
            mDialogPlayer = new DefaultDialogueActPlayer(this);
        }

        mDialogPlayer.launch();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void launchScenePlayer() {
        // Launch The Player
        mScenePlayer.launch();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void unloadScenePlayer() {
        // Unload The Player
        mScenePlayer.unload();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public synchronized void setSceneInitialHash(int value) {
        // TODO: Why Is This Method Public And Called 
        // From Outside and Not In The Constructor?
        mProjectInitialHash = value;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private synchronized int getSceneInitialHash() {
        return mProjectInitialHash;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public synchronized int getHashCode() {
        int hashCode = ((mSceneFlow == null)
                ? 0
                : mSceneFlow.getHashCode());
        // TODO: Why Is The Hash Computed
        // Only Based On The SceneFlow's 
        // Hash And Not Based Also On The 
        // Other Project Data Structures?
        return hashCode;
    }
}
