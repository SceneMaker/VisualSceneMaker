package de.dfki.vsm.runtime.logic;

/**
 * Abstraction for logic-query backends.
 */
public interface LogicEngine {

    String id();

    boolean isEnabled();

    void load(String source);

    LogicQueryResult query(String query);
}
