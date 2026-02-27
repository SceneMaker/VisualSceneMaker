package de.dfki.vsm.sceneflow.ir;

import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge;
import de.dfki.vsm.model.sceneflow.glue.command.Assignment;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.xml.XMLUtilities;
import org.json.JSONArray;
import org.json.JSONObject;

import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

public final class SceneFlowNarrativeExplainer {

    public JSONObject explain(final Path sceneFlowPath) throws SceneFlowIrCompileException {
        return explain(sceneFlowPath, new NarrativeStyle(false));
    }

    public JSONObject explain(final Path sceneFlowPath, final NarrativeStyle style) throws SceneFlowIrCompileException {
        final NarrativeStyle effectiveStyle = style == null ? new NarrativeStyle(false) : style;
        final SceneFlow flow = loadSceneFlow(sceneFlowPath);
        final Map<String, BasicNode> nodeById = indexNodes(flow);
        final JSONArray patterns = new JSONArray();
        final JSONArray summary = new JSONArray();
        final FlowOverview overview = buildFlowOverview(flow, nodeById, effectiveStyle);

        if (overview.activeCount() > 0) {
            summary.put("This sceneflow consists of " + overview.activeCount()
                    + " active flows, defined by the start nodes: "
                    + joinAsList(overview.activeLabels()) + ".");
        } else {
            summary.put("This sceneflow has no active flows.");
        }
        if (overview.inactiveCount() > 0) {
            summary.put("Overview of non-active flows: "
                    + joinAsList(overview.inactiveLabels()) + ".");
        } else {
            summary.put("There are no non-active flows.");
        }
        summary.put("A flow describes a sequence of actions in a particular configuration represented by nodes and their connections.");
        summary.put("Actions are executed by agents and grouped into input, processing, and output categories.");
        summary.put("Active flows are started when \"Play\" is pressed.");
        summary.put("Non-active flows remain idle.");

        for (SuperNode superNode : collectSuperNodes(flow)) {
            final JSONObject waitPattern = detectConstrainedActivityWaitPattern(superNode, nodeById, effectiveStyle);
            if (waitPattern == null) {
                continue;
            }
            patterns.put(waitPattern);
            summary.put(waitPattern.optString("description", ""));
        }
        for (BasicNode node : collectBasicNodes(flow)) {
            final JSONObject interruptWaitPattern = detectNodeInterruptWaitPattern(node, nodeById, effectiveStyle);
            if (interruptWaitPattern != null) {
                patterns.put(interruptWaitPattern);
                summary.put(interruptWaitPattern.optString("description", ""));
            }

            final JSONObject timeoutRetryPattern = detectTimeoutRetryOrEscalationPattern(node, nodeById, effectiveStyle);
            if (timeoutRetryPattern != null) {
                patterns.put(timeoutRetryPattern);
                summary.put(timeoutRetryPattern.optString("description", ""));
                continue;
            }

            final JSONObject guardedWaitPattern = detectNodeGuardedWaitPattern(node, nodeById, effectiveStyle);
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
                .put("globalSummary", new JSONObject()
                        .put("activeFlowCount", overview.activeCount())
                        .put("inactiveFlowCount", overview.inactiveCount()))
                .put("concepts", new JSONArray()
                        .put("A flow describes a sequence of actions in a particular configuration represented by nodes and their connections.")
                        .put("Actions are executed by agents and grouped into input, processing, and output categories.")
                        .put("Input agents acquire information from outside the SceneFlow and update variables in the background, for example ASR or facial-expression recognition.")
                        .put("Processing agents transform or interpret data to support decision-making, for example LLMs, affect simulation, user modeling, or logging.")
                        .put("Output agents control external presentation or actuation channels, for example socially interactive agents, text-to-speech, or 3D environments.")
                        .put("Active flows are started when \"Play\" is pressed.")
                        .put("Non-active flows remain idle."))
                .put("activeFlows", overview.activeFlows())
                .put("inactiveFlows", overview.inactiveFlows())
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
            final Map<String, BasicNode> nodeById,
            final NarrativeStyle style) {
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
        final List<String> exitClauses = new ArrayList<>();
        for (InterruptEdgeEvidence exit : exits) {
            exitJson.put(new JSONObject()
                    .put("sourceId", superNode.getId())
                    .put("targetId", exit.targetId())
                    .put("targetName", exit.targetName())
                    .put("condition", exit.condition()));
            conditions.add(exit.condition() + " -> " + exit.targetId());
            final String targetLabel = label("Node", exit.targetName(), exit.targetId(), style);
            final String eventName = extractEventName(exit.condition());
            if (!eventName.isBlank()) {
                exitClauses.add("the event \"" + eventName + "\" occurs and " + targetLabel + " is activated");
            } else {
                exitClauses.add("the condition \"" + exit.condition() + "\" holds and " + targetLabel + " is activated");
            }
        }

        final String supernodeLabel = label("Supernode", superNode.getName(), superNode.getId(), style);
        final String firstSentence = supernodeLabel + " waits until " + joinWithOr(exitClauses) + ".";
        final String secondSentence = waitLoop.onSuperNodeSelf()
                ? "The supernode is kept alive by a self " + EdgeLabelMapper.toHumanLabel("TEDGE")
                + " every " + waitLoop.timeoutMs() + " ms."
                : "The supernode is kept alive by a minimal internal flow consisting of "
                + label("node", waitLoop.nodeName(), waitLoop.nodeId(), style)
                + " with a self " + EdgeLabelMapper.toHumanLabel("TEDGE")
                + " every " + waitLoop.timeoutMs() + " ms.";
        final String description = firstSentence + " " + secondSentence;

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
            final Map<String, BasicNode> nodeById,
            final NarrativeStyle style) {
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
        final List<String> exitClauses = new ArrayList<>();
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
            final String targetLabel = label(
                    "node",
                    target == null ? "" : target.getName(),
                    targetId,
                    style,
                    true);
            final String eventName = extractEventName(condition);
            if (!eventName.isBlank()) {
                exitClauses.add("the event \"" + eventName + "\" occurs and " + targetLabel + " is activated");
            } else {
                exitClauses.add("the condition \"" + condition + "\" holds and " + targetLabel + " is activated");
            }
        }
        if (exits.isEmpty()) {
            return null;
        }

        final SuperNode parent = node.getParentNode();
        final String parentId = parent == null ? "" : parent.getId();
        final String parentName = parent == null ? "" : parent.getName();
        final String description = label("Node", node.getName(), node.getId(), style)
                + " waits in a timed loop with a self " + EdgeLabelMapper.toHumanLabel("TEDGE")
                + " every " + timeoutEdge.getTimeout() + " ms. "
                + "It reacts via " + EdgeLabelMapper.toHumanLabel("IEDGE")
                + " when " + joinWithOr(exitClauses) + ".";

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
            final Map<String, BasicNode> nodeById,
            final NarrativeStyle style) {
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

        final String sourceLabel = label("Node", node.getName(), node.getId(), style, true);
        final String firstSentence = sourceLabel + " is evaluated in a timed loop with a self "
                + EdgeLabelMapper.toHumanLabel("TEDGE") + " every " + timeoutEdge.getTimeout() + " ms.";
        final List<String> clauses = new ArrayList<>();
        for (int i = 0; i < exits.length(); i++) {
            final JSONObject exit = exits.getJSONObject(i);
            final String condition = exit.optString("condition", "");
            final String variable = simpleBooleanVariable(condition);
            final String targetLabel = label(
                    "node",
                    exit.optString("targetName", ""),
                    exit.optString("targetId", ""),
                    style,
                    true);
            if (!variable.isBlank()) {
                clauses.add("If the variable \"" + variable + "\" is true during a loop cycle, "
                        + targetLabel + " is activated.");
            } else {
                clauses.add("If the condition \"" + condition + "\" is true during a loop cycle, "
                        + targetLabel + " is activated.");
            }
        }
        final String commandSentence = commandSentence(node, style);
        final String description = firstSentence + " " + String.join(" ", clauses)
                + (commandSentence.isBlank() ? "" : " " + commandSentence);
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
            final Map<String, BasicNode> nodeById,
            final NarrativeStyle style) {
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
        final List<String> thresholdClauses = new ArrayList<>();
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
            final String targetLabel = label("node", target == null ? "" : target.getName(), targetId, style, true);
            thresholdClauses.add("the threshold condition \"" + condition + "\" holds and " + targetLabel + " is activated");
        }
        if (thresholdExits.isEmpty()) {
            return null;
        }

        final String description = label("Node", node.getName(), node.getId(), style)
                + " implements timeout retry/escalation: it stays active with a self "
                + EdgeLabelMapper.toHumanLabel("TEDGE") + " every "
                + timeoutEdge.getTimeout() + " ms and exits when "
                + joinWithOr(thresholdClauses) + ".";
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

    private String label(
            final String kind,
            final String name,
            final String id,
            final NarrativeStyle style) {
        return label(kind, name, id, style, false);
    }

    private String label(
            final String kind,
            final String name,
            final String id,
            final NarrativeStyle style,
            final boolean forceId) {
        final String base = (kind == null ? "" : kind + " ") + quoted(nonBlank(name, nonBlank(id, "unknown")));
        if ((forceId || (style != null && style.includeIds())) && id != null && !id.isBlank()) {
            return base + " (" + id + ")";
        }
        return base;
    }

    private String extractEventName(final String condition) {
        if (condition == null || condition.isBlank()) {
            return "";
        }
        final java.util.regex.Matcher m = java.util.regex.Pattern.compile("\"([^\"]+)\"").matcher(condition);
        if (m.find()) {
            return m.group(1).trim();
        }
        return "";
    }

    private String joinWithOr(final List<String> parts) {
        if (parts == null || parts.isEmpty()) {
            return "a configured interrupt condition occurs";
        }
        if (parts.size() == 1) {
            return parts.get(0);
        }
        return joinWithConjunction(parts, "or");
    }

    private String joinWithConjunction(final List<String> parts, final String conjunction) {
        if (parts == null || parts.isEmpty()) {
            return "";
        }
        if (parts.size() == 1) {
            return parts.get(0);
        }
        if (parts.size() == 2) {
            return parts.get(0) + " " + conjunction + " " + parts.get(1);
        }
        final String prefix = String.join(", ", parts.subList(0, parts.size() - 1));
        return prefix + ", " + conjunction + " " + parts.get(parts.size() - 1);
    }

    private String simpleBooleanVariable(final String condition) {
        if (condition == null) {
            return "";
        }
        final String trimmed = condition.trim();
        if (trimmed.matches("[A-Za-z_][A-Za-z0-9_]*")) {
            return trimmed;
        }
        return "";
    }

    private String commandSentence(final BasicNode node, final NarrativeStyle style) {
        if (node == null || node.getCmdList() == null || node.getCmdList().isEmpty()) {
            return "";
        }
        final List<String> summarized = new ArrayList<>();
        for (Command command : node.getCmdList()) {
            if (command == null) {
                continue;
            }
            if (command instanceof Assignment assignment) {
                summarized.add("assignment " + quoted(nonBlank(assignment.getConcreteSyntax(), "expression")));
                continue;
            }
            final String simpleName = command.getClass().getSimpleName();
            if ("PlayActionActivity".equals(simpleName)) {
                summarized.add("PlayAction");
                continue;
            }
            if ("PlayScenesActivity".equals(simpleName)) {
                summarized.add("PlayScene");
                continue;
            }
            if ("StopActionActivity".equals(simpleName)) {
                summarized.add("StopAction");
                continue;
            }
            final String concrete = nonBlank(command.getConcreteSyntax(), simpleName);
            summarized.add(concrete);
        }
        if (summarized.isEmpty()) {
            return "";
        }
        return "Before evaluating the conditional edge, commands of "
                + label("node", node.getName(), node.getId(), style, true)
                + " are processed: " + String.join(", ", summarized) + ".";
    }

    private String nonBlank(final String value, final String fallback) {
        if (value == null || value.isBlank()) {
            return fallback;
        }
        return value;
    }

    private FlowOverview buildFlowOverview(
            final SceneFlow flow,
            final Map<String, BasicNode> nodeById,
            final NarrativeStyle style) {
        final List<String> activeIds = new ArrayList<>(flow.getStartNodeMap().keySet());
        activeIds.sort(Comparator.naturalOrder());
        final JSONArray activeFlows = new JSONArray();
        final List<String> activeLabels = new ArrayList<>();
        for (String id : activeIds) {
            final BasicNode node = nodeById.get(id);
            final String name = node == null ? id : nonBlank(node.getName(), id);
            final String type = node instanceof SuperNode ? "supernode" : "node";
            final String label = label(type, name, id, style, true);
            activeLabels.add(label);
            activeFlows.put(new JSONObject()
                    .put("id", id)
                    .put("name", name)
                    .put("type", type));
        }

        final Set<String> activeSet = new HashSet<>(activeIds);
        final List<String> inactiveIds = new ArrayList<>();
        for (BasicNode node : flow.getNodeList()) {
            if (node.getId() != null && !node.getId().isBlank() && !activeSet.contains(node.getId())) {
                inactiveIds.add(node.getId());
            }
        }
        for (SuperNode node : flow.getSuperNodeList()) {
            if (node.getId() != null && !node.getId().isBlank() && !activeSet.contains(node.getId())) {
                inactiveIds.add(node.getId());
            }
        }
        inactiveIds.sort(Comparator.naturalOrder());
        final JSONArray inactiveFlows = new JSONArray();
        final List<String> inactiveLabels = new ArrayList<>();
        for (String id : inactiveIds) {
            final BasicNode node = nodeById.get(id);
            final String name = node == null ? id : nonBlank(node.getName(), id);
            final String type = node instanceof SuperNode ? "supernode" : "node";
            final String label = label(type, name, id, style, true);
            inactiveLabels.add(label);
            inactiveFlows.put(new JSONObject()
                    .put("id", id)
                    .put("name", name)
                    .put("type", type));
        }

        return new FlowOverview(activeFlows, inactiveFlows, activeLabels, inactiveLabels, activeLabels.size(), inactiveLabels.size());
    }

    private String joinAsList(final List<String> values) {
        if (values == null || values.isEmpty()) {
            return "none";
        }
        return String.join(", ", values);
    }

    private record WaitLoopEvidence(String nodeId, String nodeName, long timeoutMs, boolean onSuperNodeSelf) {
    }

    private record InterruptEdgeEvidence(String targetId, String targetName, String condition) {
    }

    private record FlowOverview(
            JSONArray activeFlows,
            JSONArray inactiveFlows,
            List<String> activeLabels,
            List<String> inactiveLabels,
            int activeCount,
            int inactiveCount) {
    }

    public record NarrativeStyle(boolean includeIds) {
    }
}
