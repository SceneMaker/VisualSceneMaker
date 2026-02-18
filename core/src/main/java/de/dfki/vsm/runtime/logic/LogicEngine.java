package de.dfki.vsm.runtime.logic;

import de.dfki.vsm.util.jpl.JPLResult;

/**
 * Abstraction for logic-query backends.
 */
public interface LogicEngine {

    String id();

    boolean isEnabled();

    void load(String source);

    JPLResult query(String query);
}
