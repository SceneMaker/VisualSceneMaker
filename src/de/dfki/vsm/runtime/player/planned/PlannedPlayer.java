package de.dfki.vsm.runtime.player.planned;

import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.runtime.interpreter.Process;
import de.dfki.vsm.runtime.player.ScenePlayer;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public final class PlannedPlayer extends ScenePlayer {

    // Create the planned player
    public PlannedPlayer(final RunTimeProject project) {
        super(project);
        mLogger.message("Creating planned player '" + this + "' for project '" + project + "'");
    }

    // Launch the planned player
    @Override
    public final void launch() {
        mLogger.message("Launching planned player '" + this + "'");
    }

    // Unload the planned player
    @Override
    public final void unload() {
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
