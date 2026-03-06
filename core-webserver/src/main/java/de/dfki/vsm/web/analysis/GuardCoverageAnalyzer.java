package de.dfki.vsm.web.analysis;

import de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.ParenExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.UnaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.BoolLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.variable.SimpleVariable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

final class GuardCoverageAnalyzer {
    enum CoverageKind {
        NONE,
        TRUE_BRANCH,
        FULL_BOOLEAN_COVERAGE,
        PARTIAL,
        UNSUPPORTED
    }

    static final class CoverageResult {
        private final CoverageKind kind;

        CoverageResult(CoverageKind kind) {
            this.kind = kind;
        }

        CoverageKind getKind() {
            return kind;
        }
    }

    CoverageResult analyze(List<GuargedEdge> edges) {
        if (edges == null || edges.isEmpty()) {
            return new CoverageResult(CoverageKind.NONE);
        }

        Map<String, BooleanCoverage> variableCoverage = new HashMap<>();
        boolean sawSupportedGuard = false;
        boolean sawUnsupportedGuard = false;

        for (GuargedEdge edge : edges) {
            GuardMeaning meaning = extractMeaning(edge != null ? edge.getCondition() : null);
            if (meaning == GuardMeaning.TRUE_LITERAL) {
                return new CoverageResult(CoverageKind.TRUE_BRANCH);
            }
            if (meaning instanceof VariableTruth variableTruth) {
                sawSupportedGuard = true;
                BooleanCoverage coverage = variableCoverage.computeIfAbsent(variableTruth.name(), ignored -> new BooleanCoverage());
                coverage.include(variableTruth.truthValue());
                if (coverage.isComplete()) {
                    return new CoverageResult(CoverageKind.FULL_BOOLEAN_COVERAGE);
                }
                continue;
            }
            if (meaning == GuardMeaning.FALSE_LITERAL) {
                sawSupportedGuard = true;
                continue;
            }
            sawUnsupportedGuard = true;
        }

        if (sawSupportedGuard) {
            return new CoverageResult(CoverageKind.PARTIAL);
        }
        if (sawUnsupportedGuard) {
            return new CoverageResult(CoverageKind.UNSUPPORTED);
        }
        return new CoverageResult(CoverageKind.NONE);
    }

    private GuardMeaning extractMeaning(Expression expression) {
        Expression normalized = unwrap(expression);
        if (normalized instanceof BoolLiteral literal) {
            return literal.getValue() ? GuardMeaning.TRUE_LITERAL : GuardMeaning.FALSE_LITERAL;
        }
        if (normalized instanceof SimpleVariable variable) {
            return new VariableTruth(variable.getName(), true);
        }
        if (normalized instanceof UnaryExpression unary && unary.getOperator() == UnaryExpression.UnaryOp.Not) {
            Expression inner = unwrap(unary.getExp());
            if (inner instanceof SimpleVariable variable) {
                return new VariableTruth(variable.getName(), false);
            }
            return GuardMeaning.UNSUPPORTED;
        }
        if (normalized instanceof BinaryExpression binary && binary.getOperator() == BinaryExpression.BinaryOp.Eq) {
            VariableTruth truth = extractBinaryEq(binary.getLeftExp(), binary.getRightExp());
            if (truth != null) {
                return truth;
            }
            truth = extractBinaryEq(binary.getRightExp(), binary.getLeftExp());
            return truth != null ? truth : GuardMeaning.UNSUPPORTED;
        }
        return GuardMeaning.UNSUPPORTED;
    }

    private VariableTruth extractBinaryEq(Expression variableCandidate, Expression literalCandidate) {
        Expression normalizedVariable = unwrap(variableCandidate);
        Expression normalizedLiteral = unwrap(literalCandidate);
        if (normalizedVariable instanceof SimpleVariable variable && normalizedLiteral instanceof BoolLiteral literal) {
            return new VariableTruth(variable.getName(), literal.getValue());
        }
        return null;
    }

    private Expression unwrap(Expression expression) {
        Expression current = expression;
        while (current instanceof ParenExpression paren && paren.getExp() != null) {
            current = paren.getExp();
        }
        return current;
    }

    private sealed interface GuardMeaning permits VariableTruth, GuardMeaning.Marker {
        GuardMeaning TRUE_LITERAL = new Marker();
        GuardMeaning FALSE_LITERAL = new Marker();
        GuardMeaning UNSUPPORTED = new Marker();

        final class Marker implements GuardMeaning {
            private Marker() {
            }
        }
    }

    private record VariableTruth(String name, boolean truthValue) implements GuardMeaning {
    }

    private static final class BooleanCoverage {
        private boolean hasTrue;
        private boolean hasFalse;

        void include(boolean value) {
            if (value) {
                hasTrue = true;
            } else {
                hasFalse = true;
            }
        }

        boolean isComplete() {
            return hasTrue && hasFalse;
        }
    }
}
