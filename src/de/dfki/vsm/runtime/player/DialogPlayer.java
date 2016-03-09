package de.dfki.vsm.runtime.player;

import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.runtime.interpreter.Process;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public final class DialogPlayer implements AbstractPlayer {

    // The defaut system logger
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    // Create the dialog player
    public DialogPlayer(final RunTimeProject project) {
        mLogger.message("Creating dialog player '" + this + "' for project '" + project + "'");
    }

    // Launch the dialog player
    @Override
    public final void launch() {
        mLogger.message("Launching dialog player '" + this + "'");
    }

    // Unload the dialog player
    @Override
    public final void unload() {
        mLogger.message("Unloading dialog player '" + this + "'");
    }

    @Override
    public final void play(final String name, final LinkedList args) {
        // Get the current process
        final Process process = (Process) Thread.currentThread();
        // Print some information
        mLogger.message("Playing '" + name + "' in process '" + process + "' on dialog player '" + this + "'");
    }
}
