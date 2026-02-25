package de.dfki.vsm.sceneflow.ir;

import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.xml.XMLUtilities;
import org.json.JSONArray;
import org.json.JSONObject;

import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

public final class SceneFlowNarrativeExplainer {

    public JSONObject explain(final Path sceneFlowPath) throws SceneFlowIrCompileException {
        final SceneFlow flow = loadSceneFlow(sceneFlowPath);
        final Map<String, BasicNode> nodeById = indexNodes(flow);
        final JSONArray patterns = new JSONArray();
        final JSONArray summary = new JSONArray();

        for (SuperNode superNode : collectSuperNodes(flow)) {
            final JSONObject waitPattern = detectConstrainedActivityWaitPattern(superNode, nodeById);
            if (waitPattern == null) {
                continue;
            }
            patterns.put(waitPattern);
            summary.put(waitPattern.optString("description", ""));
        }
        for (BasicNode node : collectBasicNodes(flow)) {
            final JSONObject interruptWaitPattern = detectNodeInterruptWaitPattern(node, nodeById);
            if (interruptWaitPattern != null) {
                patterns.put(interruptWaitPattern);
                summary.put(interruptWaitPattern.optString("description", ""));
            }

            final JSONObject timeoutRetryPattern = detectTimeoutRetryOrEscalationPattern(node, nodeById);
            if (timeoutRetryPattern != null) {
                patterns.put(timeoutRetryPattern);
                summary.put(timeoutRetryPattern.optString("description", ""));
                continue;
            }

            final JSONObject guardedWaitPattern = detectNodeGuardedWaitPattern(node, nodeById);
            if (guardedWaitPattern != null) {
                patterns.put(guardedWaitPattern);
                summary.put(guardedWaitPattern.optString("description", ""));
            }
        }

        if (summary.isEmpty()) {
            summary.put("No constrained-activity wait pattern detected.");
        }

        return new JSONObject()
                .put("generatedAt", Instant.now().toString())
                .put("sceneFlowPath", sceneFlowPath.toAbsolutePath().toString())
                .put("sceneFlowId", nonBlank(flow.getId(), "SceneFlow"))
                .put("sceneFlowName", nonBlank(flow.getName(), nonBlank(flow.getId(), "SceneFlow")))
                .put("summary", summary)
                .put("patterns", patterns);
    }

    private SceneFlow loadSceneFlow(final Path sceneFlowXmlPath) throws SceneFlowIrCompileException {
        final SceneFlow sceneFlow = new SceneFlow();
        if (!XMLUtilities.parseFromXMLFile(sceneFlow, sceneFlowXmlPath.toFile())) {
            throw new SceneFlowIrCompileException("Cannot parse SceneFlow XML file: " + sceneFlowXmlPath);
        }
        sceneFlow.establishStartNodes();
        sceneFlow.establishTargetNodes();
        sceneFlow.establishAltStartNodes();
        return sceneFlow;
    }

    private JSONObject detectConstrainedActivityWaitPattern(
            final SuperNode superNode,
            final Map<String, BasicNode> nodeById) {
        final Set<String> scopeNodeIds = collectScopeNodeIds(superNode);
        final WaitLoopEvidence waitLoop = detectWaitLoop(superNode, scopeNodeIds);
        if (waitLoop == null) {
            return null;
        }

        final List<InterruptEdgeEvidence> exits = detectInterruptExits(superNode, scopeNodeIds, nodeById);
        if (exits.isEmpty()) {
            return null;
        }

        final JSONArray exitJson = new JSONArray();
        final List<String> conditions = new ArrayList<>();
        for (InterruptEdgeEvidence exit : exits) {
            exitJson.put(new JSONObject()
                    .put("sourceId", superNode.getId())
                    .put("targetId", exit.targetId())
                    .put("targetName", exit.targetName())
                    .put("condition", exit.condition()));
            conditions.add(exit.condition() + " -> " + exit.targetId());
        }

        final String description = "Supernode " + superNode.getId() + " (" + quoted(superNode.getName()) + ")"
                + " remains active by "
                + (waitLoop.onSuperNodeSelf()
                ? "a self " + EdgeLabelMapper.toHumanLabel("TEDGE") + " on the supernode"
                : "the internal node " + waitLoop.nodeId() + " with a self "
                + EdgeLabelMapper.toHumanLabel("TEDGE"))
                + " every " + waitLoop.timeoutMs()
                + " ms and exits via " + EdgeLabelMapper.toHumanLabel("IEDGE") + " when "
                + String.join("; ", conditions) + ".";

        return new JSONObject()
                .put("patternType", "constrained_activity_wait_for_interrupt")
                .put("description", description)
                .put("evidence", new JSONObject()
                        .put("superNodeId", superNode.getId())
                        .put("superNodeName", superNode.getName())
                        .put("waitLoop", new JSONObject()
                                .put("nodeId", waitLoop.nodeId())
                                .put("nodeName", waitLoop.nodeName())
                                .put("scope", waitLoop.onSuperNodeSelf() ? "supernode_self" : "internal_node")
                                .put("timeoutMs", waitLoop.timeoutMs()))
                        .put("interruptExits", exitJson));
    }

    private JSONObject detectNodeInterruptWaitPattern(
            final BasicNode node,
            final Map<String, BasicNode> nodeById) {
        if (node == null || node instanceof SuperNode) {
            return null;
        }
        final AbstractEdge defaultEdge = node.getDedge();
        if (!(defaultEdge instanceof TimeoutEdge timeoutEdge)) {
            return null;
        }
        if (!node.getId().equals(timeoutEdge.getTargetUnid())) {
            return null;
        }
        if (node.getIEdgeList() == null || node.getIEdgeList().isEmpty()) {
            return null;
        }

        final JSONArray exits = new JSONArray();
        final List<String> conditions = new ArrayList<>();
        for (InterruptEdge edge : node.getIEdgeList()) {
            final String targetId = edge.getTargetUnid();
            if (targetId == null || targetId.isBlank()) {
                continue;
            }
            final BasicNode target = nodeById.get(targetId);
            final String condition = expressionToText(edge.getCondition());
            exits.put(new JSONObject()
                    .put("sourceId", node.getId())
                    .put("targetId", targetId)
                    .put("targetName", target == null ? "" : target.getName())
                    .put("condition", condition));
            conditions.add(condition + " -> " + targetId);
        }
        if (exits.isEmpty()) {
            return null;
        }

        final SuperNode parent = node.getParentNode();
        final String parentId = parent == null ? "" : parent.getId();
        final String parentName = parent == null ? "" : parent.getName();
        final String description = "Node " + node.getId() + " (" + quoted(node.getName()) + ")"
                + " remains active with a self " + EdgeLabelMapper.toHumanLabel("TEDGE")
                + " every " + timeoutEdge.getTimeout()
                + " ms and reacts via " + EdgeLabelMapper.toHumanLabel("IEDGE") + " when "
                + String.join("; ", conditions) + ".";

        return new JSONObject()
                .put("patternType", "node_interrupt_wait_loop")
                .put("description", description)
                .put("evidence", new JSONObject()
                        .put("nodeId", node.getId())
                        .put("nodeName", node.getName())
                        .put("parentSuperNodeId", parentId)
                        .put("parentSuperNodeName", parentName)
                        .put("waitLoop", new JSONObject()
                                .put("nodeId", node.getId())
                                .put("nodeName", node.getName())
                                .put("scope", "node_self")
                                .put("timeoutMs", timeoutEdge.getTimeout()))
                        .put("interruptExits", exits));
    }

    private JSONObject detectNodeGuardedWaitPattern(
            final BasicNode node,
            final Map<String, BasicNode> nodeById) {
        if (node == null || node instanceof SuperNode) {
            return null;
        }
        final AbstractEdge defaultEdge = node.getDedge();
        if (!(defaultEdge instanceof TimeoutEdge timeoutEdge)) {
            return null;
        }
        if (!node.getId().equals(timeoutEdge.getTargetUnid())) {
            return null;
        }
        if (node.getCEdgeList() == null || node.getCEdgeList().isEmpty()) {
            return null;
        }

        final JSONArray exits = new JSONArray();
        final List<String> conditions = new ArrayList<>();
        for (GuargedEdge edge : node.getCEdgeList()) {
            final String targetId = edge.getTargetUnid();
            if (targetId == null || targetId.isBlank()) {
                continue;
            }
            final BasicNode target = nodeById.get(targetId);
            final String condition = expressionToText(edge.getCondition());
            exits.put(new JSONObject()
                    .put("sourceId", node.getId())
                    .put("targetId", targetId)
                    .put("targetName", target == null ? "" : target.getName())
                    .put("condition", condition));
            conditions.add(condition + " -> " + targetId);
        }
        if (exits.isEmpty()) {
            return null;
        }

        final String description = "Node " + node.getId() + " (" + quoted(node.getName()) + ")"
                + " remains active with a self " + EdgeLabelMapper.toHumanLabel("TEDGE")
                + " every " + timeoutEdge.getTimeout()
                + " ms and proceeds via " + EdgeLabelMapper.toHumanLabel("CEDGE")
                + " when " + String.join("; ", conditions) + ".";
        return new JSONObject()
                .put("patternType", "node_guarded_wait_loop")
                .put("description", description)
                .put("evidence", new JSONObject()
                        .put("nodeId", node.getId())
                        .put("nodeName", node.getName())
                        .put("waitLoop", new JSONObject()
                                .put("nodeId", node.getId())
                                .put("nodeName", node.getName())
                                .put("scope", "node_self")
                                .put("timeoutMs", timeoutEdge.getTimeout()))
                        .put("guardedExits", exits));
    }

    private JSONObject detectTimeoutRetryOrEscalationPattern(
            final BasicNode node,
            final Map<String, BasicNode> nodeById) {
        if (node == null || node instanceof SuperNode) {
            return null;
        }
        final AbstractEdge defaultEdge = node.getDedge();
        if (!(defaultEdge instanceof TimeoutEdge timeoutEdge)) {
            return null;
        }
        if (!node.getId().equals(timeoutEdge.getTargetUnid())) {
            return null;
        }
        if (node.getCEdgeList() == null || node.getCEdgeList().isEmpty()) {
            return null;
        }

        final JSONArray thresholdExits = new JSONArray();
        final List<String> thresholdDescriptions = new ArrayList<>();
        for (GuargedEdge edge : node.getCEdgeList()) {
            final String condition = expressionToText(edge.getCondition());
            final String lower = condition.toLowerCase(Locale.ROOT);
            if (!(lower.contains(">=") || lower.contains(">"))) {
                continue;
            }
            if (!(lower.contains("retry") || lower.contains("attempt") || lower.contains("count") || lower.contains("cnt"))) {
                continue;
            }
            final String targetId = edge.getTargetUnid();
            if (targetId == null || targetId.isBlank()) {
                continue;
            }
            final BasicNode target = nodeById.get(targetId);
            thresholdExits.put(new JSONObject()
                    .put("sourceId", node.getId())
                    .put("targetId", targetId)
                    .put("targetName", target == null ? "" : target.getName())
                    .put("condition", condition));
            thresholdDescriptions.add(condition + " -> " + targetId);
        }
        if (thresholdExits.isEmpty()) {
            return null;
        }

        final String description = "Node " + node.getId() + " (" + quoted(node.getName()) + ")"
                + " implements timeout retry/escalation: it stays active with a self "
                + EdgeLabelMapper.toHumanLabel("TEDGE") + " every "
                + timeoutEdge.getTimeout() + " ms and exits when threshold guard(s) hold: "
                + String.join("; ", thresholdDescriptions) + ".";
        return new JSONObject()
                .put("patternType", "timeout_retry_or_escalation")
                .put("description", description)
                .put("evidence", new JSONObject()
                        .put("nodeId", node.getId())
                        .put("nodeName", node.getName())
                        .put("waitLoop", new JSONObject()
                                .put("nodeId", node.getId())
                                .put("nodeName", node.getName())
                                .put("scope", "node_self")
                                .put("timeoutMs", timeoutEdge.getTimeout()))
                        .put("thresholdExits", thresholdExits));
    }

    private WaitLoopEvidence detectWaitLoop(final SuperNode superNode, final Set<String> scopeNodeIds) {
        final AbstractEdge selfEdge = superNode.getDedge();
        if (selfEdge instanceof TimeoutEdge timeoutEdge && superNode.getId().equals(timeoutEdge.getTargetUnid())) {
            return new WaitLoopEvidence(superNode.getId(), superNode.getName(), timeoutEdge.getTimeout(), true);
        }
        for (BasicNode node : collectNodes(superNode)) {
            final AbstractEdge edge = node.getDedge();
            if (!(edge instanceof TimeoutEdge timeoutEdge)) {
                continue;
            }
            final String target = timeoutEdge.getTargetUnid();
            if (!node.getId().equals(target)) {
                continue;
            }
            if (!scopeNodeIds.contains(node.getId())) {
                continue;
            }
            return new WaitLoopEvidence(node.getId(), node.getName(), timeoutEdge.getTimeout(), false);
        }
        return null;
    }

    private List<InterruptEdgeEvidence> detectInterruptExits(
            final SuperNode superNode,
            final Set<String> scopeNodeIds,
            final Map<String, BasicNode> nodeById) {
        final List<InterruptEdgeEvidence> exits = new ArrayList<>();
        for (InterruptEdge edge : superNode.getIEdgeList()) {
            final String targetId = edge.getTargetUnid();
            if (targetId == null || targetId.isBlank()) {
                continue;
            }
            if (scopeNodeIds.contains(targetId)) {
                continue;
            }
            final BasicNode target = nodeById.get(targetId);
            exits.add(new InterruptEdgeEvidence(
                    targetId,
                    target == null ? "" : target.getName(),
                    expressionToText(edge.getCondition())));
        }
        return exits;
    }

    private String expressionToText(final Expression expression) {
        if (expression == null) {
            return "<no condition>";
        }
        final String concrete = expression.getConcreteSyntax();
        if (concrete != null && !concrete.isBlank()) {
            return concrete;
        }
        final String formatted = expression.getFormattedSyntax();
        if (formatted != null && !formatted.isBlank()) {
            return formatted;
        }
        final String abstractSyntax = expression.getAbstractSyntax();
        if (abstractSyntax != null && !abstractSyntax.isBlank()) {
            return abstractSyntax;
        }
        return "<unknown condition>";
    }

    private Map<String, BasicNode> indexNodes(final SceneFlow flow) {
        final Map<String, BasicNode> byId = new LinkedHashMap<>();
        for (BasicNode node : collectNodes(flow)) {
            if (node.getId() != null && !node.getId().isBlank()) {
                byId.put(node.getId(), node);
            }
        }
        byId.put(flow.getId(), flow);
        return byId;
    }

    private List<SuperNode> collectSuperNodes(final SuperNode root) {
        final List<SuperNode> out = new ArrayList<>();
        collectSuperNodesRecursive(root, out);
        return out;
    }

    private void collectSuperNodesRecursive(final SuperNode current, final List<SuperNode> out) {
        out.add(current);
        for (SuperNode child : current.getSuperNodeList()) {
            collectSuperNodesRecursive(child, out);
        }
    }

    private List<BasicNode> collectNodes(final SuperNode root) {
        final List<BasicNode> out = new ArrayList<>();
        collectNodesRecursive(root, out);
        return out;
    }

    private List<BasicNode> collectBasicNodes(final SuperNode root) {
        final List<BasicNode> out = new ArrayList<>();
        collectBasicNodesRecursive(root, out);
        return out;
    }

    private void collectNodesRecursive(final SuperNode current, final List<BasicNode> out) {
        out.addAll(current.getNodeList());
        for (SuperNode child : current.getSuperNodeList()) {
            out.add(child);
            collectNodesRecursive(child, out);
        }
    }

    private void collectBasicNodesRecursive(final SuperNode current, final List<BasicNode> out) {
        out.addAll(current.getNodeList());
        for (SuperNode child : current.getSuperNodeList()) {
            collectBasicNodesRecursive(child, out);
        }
    }

    private Set<String> collectScopeNodeIds(final SuperNode superNode) {
        final Set<String> ids = new HashSet<>();
        for (BasicNode node : collectNodes(superNode)) {
            if (node.getId() != null && !node.getId().isBlank()) {
                ids.add(node.getId());
            }
        }
        return ids;
    }

    private String quoted(final String value) {
        return "\"" + (value == null ? "" : value) + "\"";
    }

    private String nonBlank(final String value, final String fallback) {
        if (value == null || value.isBlank()) {
            return fallback;
        }
        return value;
    }

    private record WaitLoopEvidence(String nodeId, String nodeName, long timeoutMs, boolean onSuperNodeSelf) {
    }

    private record InterruptEdgeEvidence(String targetId, String targetName, String condition) {
    }
}
