package de.dfki.vsm.runtime.player;

import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.runtime.interpreter.Process;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public final class PlannedPlayer extends RunTimePlayer {

    // Create the planned player
    public PlannedPlayer(final RunTimeProject project) {
        // Initialize the player
        super(project);
        // Print some information
        mLogger.message("Creating planned player '" + this + "' for project '" + project + "'");
    }

    // Launch the planned player
    @Override
    public final void launch() {
        // Print some information
        mLogger.message("Launching planned player '" + this + "'");
    }

    // Unload the planned player
    @Override
    public final void unload() {
        // Print some information
        mLogger.message("Unloading planned player '" + this + "'");
    }

    @Override
    public final void play(final String name, final LinkedList args) {
        // Get the current process
        final Process process = (Process) Thread.currentThread();
        // Print some information
        mLogger.message("Playing '" + name + "' in process '" + process + "' on planned player '" + this + "'");
    }
}
