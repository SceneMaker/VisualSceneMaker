package de.dfki.vsm.players;

import de.dfki.vsm.model.config.ConfigData;
import de.dfki.vsm.model.project.ProjectData;
import de.dfki.vsm.runtime.player.Player;
import de.dfki.vsm.runtime.value.AbstractValue;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public final class TestPlayer implements Player {

    // The Singelton Instance
    public static TestPlayer sInstance = null;
    // The Logger Instance
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();
    // The Project Data
    final ProjectData mProject;
    // The Config Data
    final ConfigData mConfig;

    ////////////////////////////////////////////////////////////////////////////
    public static synchronized TestPlayer getInstance(
            final ProjectData project,
            final ConfigData config) {
        if (sInstance == null) {
            sInstance = new TestPlayer(project, config);
        }
        // Return The Singelton Instance
        return sInstance;
    }

    ////////////////////////////////////////////////////////////////////////////
    public TestPlayer(
            final ProjectData project,
            final ConfigData config) {
        //
        mProject = project;
        mConfig = config;
        // Print Some Information
        mLogger.message("Creating Player '" + this
                + "' With Project '" + mProject.toString()
                + "' And Config\n" + mConfig.toString());
    }

    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void launch() {
        // Print Some Information
        mLogger.message("Launching Player '" + this + "'");
    }

    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void unload() {
        // Print Some Information
        mLogger.message("Unloading Player '" + this + "'");
    }

    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void play(final String name, final LinkedList<AbstractValue> args) {
        // Print Some Information
        mLogger.message("Playing '" + name + "' With Player '" + this + "'");
    }
}
