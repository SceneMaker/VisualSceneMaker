package de.dfki.vsm.api;

import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.runtime.values.AbstractValue;

import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public final class VSMDefaultPlayer extends VSMScenePlayer {

    // The Default Scene Player
    public static VSMDefaultPlayer sInstance;

    // Create The Default Scene Player
    private VSMDefaultPlayer() {
        // Print Some Debug Information
        mLogger.message("Creating VSM Default Scene Player");
    }

    // Create The Singelton Instance
    public static synchronized VSMDefaultPlayer getInstance() {
        if (sInstance == null) {
            sInstance = new VSMDefaultPlayer();
        }
        // Return The Singelton Instance
        return sInstance;
    }

    // Launch The Default Scene Player
    @Override
    public final boolean launch(final RunTimeProject project) {
        // Launch 
        if (super.launch(project)) {
            //
            mLogger.message("Launching Default Scene Player");
            //
            return true;
        } else {
            return false;
        }
    }

    // Unload The Default Scene Player
    @Override
    public boolean unload() {
        //
        mLogger.message("Unloading Default Scene Player");
        //
        return super.unload();
    }

    // Handle An Agent Client Connection
    @Override
    public void handle(final VSMAgentClient client) {
    }

    // Play A Scene With This Player
    @Override
    public void play(final String name, final LinkedList<AbstractValue> args) {
        //
        mLogger.message("Playing Scene '" + name + "'");
        mAgentMap.get("reeti").sendString(name);

    }
}
