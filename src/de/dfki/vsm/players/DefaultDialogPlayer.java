package de.dfki.vsm.players;

import de.dfki.vsm.model.project.PlayerConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.runtime.player.Player;
import de.dfki.vsm.runtime.value.AbstractValue;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public class DefaultDialogPlayer implements Player {

    // The singelton player instance
    public static DefaultDialogPlayer sInstance = null;
    // The singelton logger instance
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();
    // The player's runtime project 
    final RunTimeProject mProject;
    // The player's configuration
    final PlayerConfig mConfig;

    // Construct the default dialog player
    private DefaultDialogPlayer(
            final RunTimeProject project,
            final PlayerConfig config) {
        // Initialize the player members
        mProject = project;
        mConfig = config;
    }

    // Get the default dialog player instance
    public static synchronized DefaultDialogPlayer getInstance(
            final RunTimeProject project,
            final PlayerConfig config) {
        if (sInstance == null) {
            sInstance = new DefaultDialogPlayer(project, config);
        }
        return sInstance;
    }

    // Launch the default dialog player
    @Override
    public final boolean launch() {
        // Print some information
        mLogger.message("Launching the default dialog player '" + this + "'");
        // Return true at success
        return true;
    }

    // Unload the default dialog player
    @Override
    public final boolean unload() {
        // Print some information
        mLogger.message("Unloading the default dialog player '" + this + "'");
        // Return true at success
        return true;
    }

    // Play some dialog with the player
    @Override
    public final void play(final String name, final LinkedList<AbstractValue> args) {
        // Print Some Information
        mLogger.message("Playing '" + name + "' with the default dialog player  '" + this + "'");
    }
}
