package de.dfki.vsm.util.jpl;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;
import org.jpl7.Compound;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Variable;

/**
 * @author Gregor Mehlmann
 */
public final class JPLResult extends LinkedList<Map<String, Term>> {

    // The System File Logger
    private static LOGDefaultLogger sLogger
            = LOGDefaultLogger.getInstance();
    // The query 
    final Query mQuery;

    public JPLResult(final Query query) {
        mQuery = query;
    }

    public JPLResult clean() {
        JPLResult copy = new JPLResult(mQuery);
        //
        for (int i = 0; i < size(); i++) {
            final Object[] bindings = get(i).entrySet().toArray();
            final Map<Variable, Variable> substitutions = new HashMap();
            for (int j = 0; j < bindings.length; j++) {
                final Entry<String, Term> binding
                        = (Entry<String, Term>) bindings[j];
                final Term term = binding.getValue();
                if (term instanceof Variable) {
                    final Variable variable = (Variable) term;
                    if (variable.name().startsWith("_")) {
                        final Variable substitute = new Variable(binding.getKey());
                        //sLogger.message("Register replacement ["
                        //        + variable.name() + "/" + substitute.name() + "]");
                        substitutions.put(variable, substitute);
                    }
                }
            }

            // Now clean the terms
            final Map<String, Term> cleans = new HashMap<String, Term>();
            for (int k = 0; k < bindings.length; k++) {
                final Entry<String, Term> binding
                        = (Entry<String, Term>) bindings[k];
                final String var = binding.getKey();
                final Term term = binding.getValue();
                // Replace the substitutions
                final Term clean = replace(term, substitutions);
                //
                //
                if (!clean.toString().equals(var)) {
                    // Add it
                    //sLogger.message("Copying binding " + var + "->" + term.toString());
                    cleans.put(var, clean);

                }
            }
            copy.add(cleans);

        }

        //sLogger.message("New Result : " + copy.toString());
        return copy;
    }

    public Term replace(final Term term, final Map replacements) {
        //sLogger.message("Cleaning term " + term.toString());
        Term result = term;

        if (term instanceof Variable) {
            if (replacements.containsKey(term)) {
                //sLogger.message("Performing replacement [" + term.name() + "/"
                //        + ((Term) replacements.get((Variable) term)).name() + "]");
                result = (Term) replacements.get((Variable) term);
            }
        } else if (term instanceof Compound) {
            final String name = ((Compound) term).name();
            final Term[] args = ((Compound) term).args();
            //
            for (int i = 0; i < args.length; i++) {
                args[i] = cleanup(args[i], replacements);
            }
            //
            result = new Compound(name, args);

        }
        //sLogger.message("Result is term " + result.toString());

        return result;
    }

    public static Term cleanup(final Term term, final Map replacements) {
        //sLogger.message("Cleaning term " + term.toString());
        if (term instanceof Variable) {
            //sLogger.message("Performing replacement [" + term.name() + "/"
            //        + ((Term) replacements.get((Variable) term)).name() + "]");
            if (replacements.containsKey(term)) {
                return (Term) replacements.get((Variable) term);
            } else {
                return term;
            }
        } else if (term instanceof Compound) {
            final String name = ((Compound) term).name();
            final Term[] args = ((Compound) term).args();
            //
            for (int i = 0; i < args.length; i++) {
                args[i] = cleanup(args[i], replacements);
            }
            //
            return new Compound(name, args);
        } else {
            return term;
        }
    }

    public final Map<Variable, Variable> getDTMs() {
        // Get the DTM variable var_substs first
        final Map<Variable, Variable> substitutions = new HashMap();
        for (int i = 0; i < size(); i++) {
            final Object[] bindings = get(i).entrySet().toArray();
            for (int j = 0; j < bindings.length; j++) {
                final Entry<String, Term> binding
                        = (Entry<String, Term>) bindings[j];
                final Term term = binding.getValue();
                // Replace the DTM variable
                if (term instanceof Variable) {
                    final Variable variable = (Variable) term;
                    // Check if the variable starts with _
                    if (variable.name().startsWith("_")) {
                        // Substitute the variable with the key
                        final Variable substitute = new Variable(binding.getKey());
                        //sLogger.message("Found replacement ["
                        //        + variable.name() + "/ " + substitute.name() + "]");
                        // Add the substitution then
                        substitutions.put(variable, substitute);
                    }
                }
            }
        }
        return substitutions;
    }

    public final String getText() {
        // Get the DTM variable var_substs first
        final Map<Variable, Variable> substitutions = getDTMs();
        // Now construct the string representation
        final StringBuilder builder = new StringBuilder();
        if (!isEmpty()) {
            for (int i = 0; i < size(); i++) {
                final Map<String, Term> solution = get(i);
                if (!solution.isEmpty()) {
                    final Object[] bindings = solution.entrySet().toArray();
                    for (int j = 0; j < bindings.length; j++) {
                        final Entry<String, Term> entry
                                = (Entry<String, Term>) bindings[j];
                        final String variable = entry.getKey();
                        //System.err.println(binding.getValue());
                        // Cleanup the term with var_substs
                        final Term term = cleanup(entry.getValue(), substitutions);

                        //System.err.println(term.args());
                        // Convert list and pair appearances
                        final String binding = JPLUtility.convert(term.toString());
                        // Exclude variable self bindings now
                        if (!binding.equals(variable)) {
                            builder.append(variable).append(" = ")
                                    .append(binding);
                            if (j < bindings.length - 1) {
                                builder.append(", ");
                            }
                        }
                    }
                    if (i < size() - 1) {
                        builder.append(";").append("\n");
                    } else {
                        builder.append(".");
                    }
                } else {
                    builder.append("true.");
                }
            }
        } else {
            builder.append("false.");
        }
        return builder.toString();
    }

    @Override
    public final String toString() {
        final StringBuilder builder = new StringBuilder();
        if (!isEmpty()) {
            for (int i = 0; i < size(); i++) {
                final Map<String, Term> solution = get(i);
                if (!solution.isEmpty()) {
                    final Object[] bindings = solution.entrySet().toArray();
                    for (int j = 0; j < bindings.length; j++) {
                        final Entry<String, Term> entry
                                = (Entry<String, Term>) bindings[j];
                        final String variable = entry.getKey();
                        final Term term = entry.getValue();
                        //
                        builder.append(variable).append(" = ")
                                .append(term.toString());
                        if (j < bindings.length - 1) {
                            builder.append(", ");
                        }
                    }
                    if (i < size() - 1) {
                        builder.append(";").append("\n");
                    } else {
                        builder.append(".");
                    }
                } else {
                    builder.append("true.");
                }
            }
        } else {
            builder.append("false.");
        }
        return builder.toString();
    }
}
