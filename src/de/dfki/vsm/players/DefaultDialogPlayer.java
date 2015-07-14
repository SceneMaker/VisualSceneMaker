package de.dfki.vsm.players;

import de.dfki.vsm.model.project.PlayerConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.runtime.players.RunTimePlayer;
import de.dfki.vsm.runtime.values.AbstractValue;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public class DefaultDialogPlayer implements RunTimePlayer {

    // The singelton player instance
    public static DefaultDialogPlayer sInstance = null;
    // The singelton logger instance
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();
    // The player's runtime project 
    private RunTimeProject mProject;
    // The project specific config
    private PlayerConfig mPlayerConfig;
    // The project specific name
    private String mPlayerName;

    // Construct the default dialog player
    private DefaultDialogPlayer() {

    }

    // Get the default dialog player instance
    public static synchronized DefaultDialogPlayer getInstance() {
        if (sInstance == null) {
            sInstance = new DefaultDialogPlayer();
        }
        return sInstance;
    }

    // Launch the default dialog player
    @Override
    public final boolean launch(final RunTimeProject project) {
        // Initialize the project
        mProject = project;
        // Initialize the name
        mPlayerName = project.getPlayerName(this);
        // Initialize the config
        mPlayerConfig = project.getPlayerConfig(mPlayerName);
        // Print some information
        mLogger.message("Launching the default dialog player '" + this + "' with configuration:\n" + mPlayerConfig);
        // Return true at success
        return true;
    }

    // Unload the default dialog player
    @Override
    public final boolean unload() {
        // Print some information
        mLogger.message("Unloading the default dialog player '" + this + "' with configuration:\n" + mPlayerConfig);
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
