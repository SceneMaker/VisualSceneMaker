package de.dfki.vsm.runtime.logic;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Portable query result type that does not expose JPL-specific classes.
 */
public final class LogicQueryResult {

    private static final LogicQueryResult EMPTY = new LogicQueryResult(List.of());

    private final List<Map<String, String>> mSolutions;

    public LogicQueryResult(final List<Map<String, String>> solutions) {
        mSolutions = List.copyOf(Objects.requireNonNull(solutions, "solutions"));
    }

    public static LogicQueryResult empty() {
        return EMPTY;
    }

    public int size() {
        return mSolutions.size();
    }

    public Map<String, String> getFirst() {
        if (mSolutions.isEmpty()) {
            return Collections.emptyMap();
        }
        return mSolutions.get(0);
    }

    public List<Map<String, String>> solutions() {
        return mSolutions;
    }
}
