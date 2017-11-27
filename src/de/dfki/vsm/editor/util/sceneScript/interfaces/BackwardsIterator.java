package de.dfki.vsm.editor.util.sceneScript.interfaces;

import java.util.Iterator;

public interface BackwardsIterator extends Iterator{
    boolean hasPrevious();
    Object previous();

}
