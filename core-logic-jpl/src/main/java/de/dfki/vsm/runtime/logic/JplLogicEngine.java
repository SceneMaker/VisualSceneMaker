package de.dfki.vsm.runtime.logic;

import de.dfki.vsm.util.jpl.JPLEngine;
import de.dfki.vsm.util.jpl.JPLResult;
import de.dfki.vsm.util.jpl.JPLUtility;
import org.jpl7.Term;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
    public LogicQueryResult query(final String query) {
        final JPLResult clean = JPLEngine.query(query).clean();
        final List<Map<String, String>> solutions = new ArrayList<>(clean.size());
        for (Map<String, Term> solution : clean) {
            final Map<String, String> normalized = new HashMap<>();
            for (Map.Entry<String, Term> entry : solution.entrySet()) {
                normalized.put(entry.getKey(), JPLUtility.convert(entry.getValue().toString()));
            }
            solutions.add(normalized);
        }
        return new LogicQueryResult(solutions);
    }
}
