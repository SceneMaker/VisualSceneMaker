package de.dfki.vsm.runtime.player;

import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public interface AbstractPlayer {

    public boolean launch();

    public boolean unload();

    public boolean play(final String name, final LinkedList args);
}
