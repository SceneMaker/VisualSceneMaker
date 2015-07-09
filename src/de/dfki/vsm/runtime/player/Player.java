package de.dfki.vsm.runtime.player;

import de.dfki.vsm.runtime.value.AbstractValue;
import de.dfki.vsm.runtime.plugin.Plugin;
import java.util.LinkedList;

/**
 * @author Not me
 */
public interface Player extends Plugin {

    // Play A Scene With Given Arguments
    public void play(final String name, final LinkedList<AbstractValue> args);

    // A Single Task Of The Scene Player
    public static class Task extends Thread {

        // The Termination Flag
        public boolean mIsDone = false;

        // Construct With A Name
        public Task(final String name) {
            super(name);
        }
    }
}
