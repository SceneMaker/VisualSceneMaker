package de.dfki.vsm.runtime.player;

import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public final class DialogPlayer implements AbstractPlayer {

    // The defaut system logger
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    public DialogPlayer(final RunTimeProject project) {
    }

    @Override
    public boolean launch() {
        mLogger.message("Launching dialog player");
        return true;
    }

    @Override
    public boolean unload() {
        mLogger.message("Unloading dialog player");

        return true;
    }

    @Override
    public boolean play(final String name, final LinkedList args) {
        return true;
    }
}
