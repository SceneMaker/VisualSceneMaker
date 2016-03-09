package de.dfki.vsm.runtime.player;

import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public interface AbstractPlayer {

    public void launch();

    public void unload();

    public void play(final String name, final LinkedList args);

    
}
