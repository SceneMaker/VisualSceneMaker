package de.dfki.vsm.runtime.logic;

/**
 * Placeholder engine for platforms where Prolog/JPL is intentionally disabled.
 */
public final class DisabledLogicEngine implements LogicEngine {

    @Override
    public String id() {
        return "disabled";
    }

    @Override
    public boolean isEnabled() {
        return false;
    }

    @Override
    public void load(final String source) {
        // Intentionally disabled.
    }

    @Override
    public LogicQueryResult query(final String query) {
        throw new IllegalStateException("Logic engine is disabled");
    }
}
