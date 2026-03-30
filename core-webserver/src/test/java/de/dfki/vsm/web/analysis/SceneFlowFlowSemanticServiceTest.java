package de.dfki.vsm.web.analysis;

import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.edge.EpsilonEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.UnaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.BoolLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.variable.SimpleVariable;
import de.dfki.vsm.util.tpl.Tuple;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.assertEquals;

class SceneFlowFlowSemanticServiceTest {
    private final SceneFlowFlowSemanticService service = new SceneFlowFlowSemanticService();

    @Test
    void classifiesNodeWithoutOutgoingEdgesAsDefiniteEnd() {
        SuperNode root = superNode("root");
        BasicNode node = node("n1");
        attachChild(root, node, true);

        assertKind(root, node, FlowSemanticKind.DEFINITE_END);
    }

    @Test
    void treatsInterruptOnlyNodeAsDefiniteEnd() {
        SuperNode root = superNode("root");
        BasicNode source = node("source");
        BasicNode target = node("target");
        attachChild(root, source, true);
        attachChild(root, target, false);
        addInterruptEdge(source, target, new BoolLiteral(true));

        assertKind(root, source, FlowSemanticKind.DEFINITE_END);
    }

    @Test
    void treatsDefaultEdgeAsGuaranteedContinuation() {
        SuperNode root = superNode("root");
        BasicNode source = node("source");
        BasicNode target = node("target");
        attachChild(root, source, true);
        attachChild(root, target, false);
        addEpsilonEdge(source, target);

        assertKind(root, source, FlowSemanticKind.NOT_END);
    }

    @Test
    void treatsConditionalTrueBranchAsGuaranteedContinuation() {
        SuperNode root = superNode("root");
        BasicNode source = node("source");
        BasicNode target = node("target");
        attachChild(root, source, true);
        attachChild(root, target, false);
        addConditionalEdge(source, target, new BoolLiteral(true));

        assertKind(root, source, FlowSemanticKind.NOT_END);
    }

    @Test
    void treatsComplementaryBooleanConditionsAsCovered() {
        SuperNode root = superNode("root");
        BasicNode source = node("source");
        BasicNode trueTarget = node("trueTarget");
        BasicNode falseTarget = node("falseTarget");
        attachChild(root, source, true);
        attachChild(root, trueTarget, false);
        attachChild(root, falseTarget, false);
        addConditionalEdge(source, trueTarget, new SimpleVariable("flag"));
        addConditionalEdge(source, falseTarget, new UnaryExpression(new SimpleVariable("flag"), UnaryExpression.UnaryOp.Not));

        assertKind(root, source, FlowSemanticKind.NOT_END);
    }

    @Test
    void marksSingleConditionalBranchAsPotentialEnd() {
        SuperNode root = superNode("root");
        BasicNode source = node("source");
        BasicNode target = node("target");
        attachChild(root, source, true);
        attachChild(root, target, false);
        addConditionalEdge(source, target, new SimpleVariable("flag"));

        assertKind(root, source, FlowSemanticKind.POTENTIAL_END);
    }

    @Test
    void marksUnsupportedConditionalCoverageAsPotentialEnd() {
        SuperNode root = superNode("root");
        BasicNode source = node("source");
        BasicNode target = node("target");
        attachChild(root, source, true);
        attachChild(root, target, false);
        addConditionalEdge(source, target, new BinaryExpression(
                new SimpleVariable("a"),
                BinaryExpression.BinaryOp.AndAnd,
                new SimpleVariable("b")));

        assertKind(root, source, FlowSemanticKind.POTENTIAL_END);
    }

    @Test
    void marksInterruptedSuperNodeWithInternalDefiniteEndAsDefiniteEnd() {
        SuperNode root = superNode("root");
        SuperNode nested = superNode("nested");
        BasicNode afterInterrupt = node("afterInterrupt");
        attachChild(root, nested, true);
        attachChild(root, afterInterrupt, false);
        addInterruptEdge(nested, afterInterrupt, new BoolLiteral(true));

        BasicNode internalStart = node("internalStart");
        attachChild(nested, internalStart, true);

        assertKind(root, nested, FlowSemanticKind.DEFINITE_END);
    }

    @Test
    void marksSuperNodeWithInternalPotentialEndAsPotentialEnd() {
        SuperNode root = superNode("root");
        SuperNode nested = superNode("nested");
        attachChild(root, nested, true);

        BasicNode internalStart = node("internalStart");
        BasicNode internalTarget = node("internalTarget");
        attachChild(nested, internalStart, true);
        attachChild(nested, internalTarget, false);
        addConditionalEdge(internalStart, internalTarget, new SimpleVariable("flag"));

        assertKind(root, nested, FlowSemanticKind.POTENTIAL_END);
    }

    @Test
    void treatsSuperNodeWithGuaranteedInternalContinuationAsNotEnd() {
        SuperNode root = superNode("root");
        SuperNode nested = superNode("nested");
        attachChild(root, nested, true);

        BasicNode a = node("a");
        BasicNode b = node("b");
        attachChild(nested, a, true);
        attachChild(nested, b, false);
        addEpsilonEdge(a, b);
        addEpsilonEdge(b, a);

        assertKind(root, nested, FlowSemanticKind.NOT_END);
    }

    @Test
    void treatsConcurrentSelfLoopingStartBranchAsPreventingSuperNodeEnd() {
        SuperNode root = superNode("root");
        SuperNode nested = superNode("nested");
        BasicNode afterInterrupt = node("afterInterrupt");
        attachChild(root, nested, true);
        attachChild(root, afterInterrupt, false);
        addInterruptEdge(nested, afterInterrupt, new BoolLiteral(true));

        BasicNode loopingStart = node("loopingStart");
        BasicNode deadEndStart = node("deadEndStart");
        attachChild(nested, loopingStart, true);
        attachChild(nested, deadEndStart, true);
        addTimeoutEdge(loopingStart, loopingStart, 1000);

        assertKind(root, nested, FlowSemanticKind.NOT_END);
    }

    private void assertKind(SuperNode root, BasicNode node, FlowSemanticKind expectedKind) {
        FlowSemanticNodeResult result = service.analyze(root).get(node);
        assertEquals(expectedKind, result != null ? result.getKind() : null);
    }

    private SuperNode superNode(String id) {
        SuperNode node = new SuperNode();
        node.setId(id);
        node.setName(id);
        return node;
    }

    private BasicNode node(String id) {
        BasicNode node = new BasicNode();
        node.setId(id);
        node.setName(id);
        return node;
    }

    private void attachChild(SuperNode parent, BasicNode child, boolean isStart) {
        child.setParentNode(parent);
        if (child instanceof SuperNode superNode) {
            parent.addSuperNode(superNode);
        } else {
            parent.addNode(child);
        }
        if (isStart) {
            parent.addStartNode(child);
        }
    }

    private void addEpsilonEdge(BasicNode source, BasicNode target) {
        source.setDedge(new EpsilonEdge(
                target.getId(),
                source.getId(),
                target,
                source,
                null,
                new ArrayList<>(),
                new HashMap<Tuple<String, BasicNode>, Tuple<String, BasicNode>>()
        ));
    }

    private void addConditionalEdge(BasicNode source, BasicNode target, Expression condition) {
        source.addCEdge(new GuargedEdge(
                target.getId(),
                source.getId(),
                target,
                source,
                null,
                new ArrayList<>(),
                new HashMap<Tuple<String, BasicNode>, Tuple<String, BasicNode>>(),
                condition
        ));
    }

    private void addInterruptEdge(BasicNode source, BasicNode target, Expression condition) {
        source.addIEdge(new InterruptEdge(
                target.getId(),
                source.getId(),
                target,
                source,
                null,
                new ArrayList<>(),
                new HashMap<Tuple<String, BasicNode>, Tuple<String, BasicNode>>(),
                condition
        ));
    }

    private void addTimeoutEdge(BasicNode source, BasicNode target, int timeoutMs) {
        source.setDedge(new TimeoutEdge(
                target.getId(),
                source.getId(),
                target,
                source,
                null,
                new ArrayList<>(),
                new HashMap<Tuple<String, BasicNode>, Tuple<String, BasicNode>>(),
                timeoutMs
        ));
    }
}
