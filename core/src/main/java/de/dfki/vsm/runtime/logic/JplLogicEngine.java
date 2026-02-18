package de.dfki.vsm.runtime.logic;

import de.dfki.vsm.util.jpl.JPLEngine;
import de.dfki.vsm.util.jpl.JPLResult;

/**
 * Desktop adapter backed by SWI-Prolog via JPL.
 */
public final class JplLogicEngine implements LogicEngine {

    @Override
    public String id() {
        return "jpl";
    }

    @Override
    public boolean isEnabled() {
        return true;
    }

    @Override
    public void load(final String source) {
        JPLEngine.load(source);
    }

    @Override
    public JPLResult query(final String query) {
        return JPLEngine.query(query);
    }
}
