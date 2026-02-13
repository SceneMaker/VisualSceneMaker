package de.dfki.vsm.runtime.interpreter;

import de.dfki.vsm.model.sceneflow.glue.command.Assignment;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.*;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.ContainsList;
import de.dfki.vsm.model.sceneflow.glue.command.expression.LiteralExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.record.ArrayExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.record.StructExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.variable.ArrayVariable;
import de.dfki.vsm.model.sceneflow.glue.command.expression.variable.MemberVariable;
import de.dfki.vsm.model.sceneflow.glue.command.expression.variable.SimpleVariable;

import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Set;

/**
 * Extracts the set of variable names referenced by an expression tree.
 * Used for guard dependency tracking: a guard only needs re-evaluation
 * when one of its referenced variables has changed.
 *
 * Returns null for "opaque" expressions (function calls, state queries,
 * random/timeout queries, history lookups, Prolog queries) that may depend
 * on external state and must always be re-evaluated.
 */
final class GuardDependencyExtractor {

    // Cache: expression identity → variable dependencies (null = opaque).
    // IdentityHashMap because Expression objects don't override equals/hashCode.
    private static final IdentityHashMap<Expression, Object> sCache = new IdentityHashMap<>();

    // Sentinel for "opaque" cached result (since null is a valid map value
    // but we can't distinguish "not cached" from "cached as opaque" with containsKey alone).
    private static final Object OPAQUE = new Object();

    /**
     * Returns the set of variable names referenced by the expression,
     * or null if the expression has opaque dependencies (must always re-evaluate).
     */
    @SuppressWarnings("unchecked")
    static Set<String> getDependencies(Expression expr) {
        Object cached = sCache.get(expr);
        if (cached == OPAQUE) return null;
        if (cached != null) return (Set<String>) cached;
        // Not yet cached — compute
        Set<String> deps = new HashSet<>();
        if (extract(expr, deps)) {
            sCache.put(expr, deps);
            return deps;
        } else {
            sCache.put(expr, OPAQUE);
            return null;
        }
    }

    /**
     * Recursively extracts variable references. Returns true if all
     * dependencies are known variable references, false if opaque.
     */
    private static boolean extract(Expression expr, Set<String> out) {
        // Literals — no dependencies
        if (expr instanceof LiteralExpression) {
            return true;
        }
        // Variable references
        if (expr instanceof SimpleVariable) {
            out.add(((SimpleVariable) expr).getName());
            return true;
        }
        if (expr instanceof MemberVariable) {
            out.add(((MemberVariable) expr).getName());
            return true;
        }
        if (expr instanceof ArrayVariable) {
            out.add(((ArrayVariable) expr).getName());
            return extract(((ArrayVariable) expr).getExpression(), out);
        }
        // Binary, unary, ternary, paren — recurse
        if (expr instanceof BinaryExpression) {
            return extract(((BinaryExpression) expr).getLeftExp(), out)
                && extract(((BinaryExpression) expr).getRightExp(), out);
        }
        if (expr instanceof UnaryExpression) {
            return extract(((UnaryExpression) expr).getExp(), out);
        }
        if (expr instanceof TernaryExpression) {
            return extract(((TernaryExpression) expr).getCondition(), out)
                && extract(((TernaryExpression) expr).getThenExp(), out)
                && extract(((TernaryExpression) expr).getElseExp(), out);
        }
        if (expr instanceof ParenExpression) {
            return extract(((ParenExpression) expr).getExp(), out);
        }
        // Collection constructors — recurse into elements
        if (expr instanceof ArrayExpression) {
            for (Expression e : ((ArrayExpression) expr).getExpList()) {
                if (!extract(e, out)) return false;
            }
            return true;
        }
        if (expr instanceof StructExpression) {
            for (Assignment a : ((StructExpression) expr).getExpList()) {
                if (!extract(a.getInitExpression(), out)) return false;
            }
            return true;
        }
        // ContainsList — has list and item sub-expressions
        if (expr instanceof ContainsList) {
            return extract(((ContainsList) expr).getListExp(), out)
                && extract(((ContainsList) expr).getItemExp(), out);
        }
        // Everything else is opaque: CallingExpression, ConstructExpression,
        // InStateQuery, RandomQuery, TimeoutQuery, PrologQuery,
        // HistoryValueOf, HistoryContains, HistoryRunTimeOf
        return false;
    }

    static void clearCache() {
        sCache.clear();
    }
}
