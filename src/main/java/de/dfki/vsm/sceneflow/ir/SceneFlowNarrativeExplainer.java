package de.dfki.vsm.sceneflow.ir;

import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.EpsilonEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.ForkingEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.RandomEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge;
import de.dfki.vsm.model.sceneflow.glue.command.Assignment;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.record.StructExpression;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.PlayScenesActivity;
import de.dfki.vsm.util.xml.XMLUtilities;
import org.json.JSONArray;
import org.json.JSONObject;

import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

public final class SceneFlowNarrativeExplainer {

    public JSONObject explain(final Path sceneFlowPath) throws SceneFlowIrCompileException {
        return explain(sceneFlowPath, new NarrativeStyle(false, "reader-friendly"));
    }

    public JSONObject explain(final Path sceneFlowPath, final NarrativeStyle style) throws SceneFlowIrCompileException {
        final NarrativeStyle effectiveStyle = style == null ? new NarrativeStyle(false, "reader-friendly") : style;
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
        // Composite shapes are detected before the per-node pass so the findings they subsume can be
        // suppressed there. A question that waits for its answer should read as one thing, not as a
        // transition plus an unexplained polling loop.
        final Set<String> hopsAlreadyNarrated = new LinkedHashSet<>();
        final Set<String> waitLoopsAlreadyNarrated = new LinkedHashSet<>();
        final Set<String> claimedByAskAndWait = new LinkedHashSet<>();
        for (JSONObject asking : detectAskAndWaitPatterns(flow, nodeById, effectiveStyle,
                hopsAlreadyNarrated, waitLoopsAlreadyNarrated, claimedByAskAndWait)) {
            patterns.put(asking);
            summary.put(asking.optString("description", ""));
        }

        // Chains are detected next, and must not absorb nodes an ask-and-wait already accounts for.
        for (JSONObject chain : detectSequenceChains(flow, nodeById, effectiveStyle,
                hopsAlreadyNarrated, claimedByAskAndWait)) {
            patterns.put(chain);
            summary.put(chain.optString("description", ""));
        }

        for (BasicNode node : collectBasicNodes(flow)) {
            final JSONObject interruptWaitPattern = detectNodeInterruptWaitPattern(node, nodeById, effectiveStyle);
            if (interruptWaitPattern != null) {
                patterns.put(interruptWaitPattern);
                summary.put(interruptWaitPattern.optString("description", ""));
            }

            final JSONObject forkPattern = detectForkParallelPattern(node, nodeById, effectiveStyle);
            if (forkPattern != null) {
                patterns.put(forkPattern);
                summary.put(forkPattern.optString("description", ""));
            }

            final JSONObject probabilisticPattern = detectProbabilisticChoicePattern(node, nodeById, effectiveStyle);
            if (probabilisticPattern != null) {
                patterns.put(probabilisticPattern);
                summary.put(probabilisticPattern.optString("description", ""));
            }

            final JSONObject timeoutRetryPattern = detectTimeoutRetryOrEscalationPattern(node, nodeById, effectiveStyle);
            if (timeoutRetryPattern != null) {
                patterns.put(timeoutRetryPattern);
                summary.put(timeoutRetryPattern.optString("description", ""));
                continue;
            }

            if (waitLoopsAlreadyNarrated.contains(node.getId())) {
                // Already narrated as the waiting half of an ask-and-wait.
                continue;
            }
            final JSONObject guardedWaitPattern = detectNodeGuardedWaitPattern(node, nodeById, effectiveStyle);
            if (guardedWaitPattern != null) {
                patterns.put(guardedWaitPattern);
                summary.put(guardedWaitPattern.optString("description", ""));
                continue;
            }

            final JSONObject conditionalChoicePattern = detectConditionalChoicePattern(node, nodeById, effectiveStyle);
            if (conditionalChoicePattern != null) {
                patterns.put(conditionalChoicePattern);
                summary.put(conditionalChoicePattern.optString("description", ""));
            }

            if (hopsAlreadyNarrated.contains(node.getId())) {
                // Already narrated as part of a sequence or an ask-and-wait.
                continue;
            }
            final JSONObject unconditionalTransitionPattern = detectUnconditionalTransitionPattern(node, nodeById, effectiveStyle);
            if (unconditionalTransitionPattern != null) {
                patterns.put(unconditionalTransitionPattern);
                summary.put(unconditionalTransitionPattern.optString("description", ""));
            }
        }

        if (summary.isEmpty()) {
            summary.put("No constrained-activity wait pattern detected.");
        }

        return new JSONObject()
                .put("generatedAt", Instant.now().toString())
                .put("sceneFlowPath", sceneFlowPath.toAbsolutePath().toString())
                .put("audience", effectiveStyle.audience())
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
                .put("patternInventory", buildPatternInventory(patterns))
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
        final String firstSentence = supernodeLabel + " waits and exits via "
                + edgeLabel("IEDGE", style) + " when " + joinWithOr(exitClauses) + ".";
        final String secondSentence = waitLoop.onSuperNodeSelf()
                ? "The supernode is kept alive by a self " + edgeLabel("TEDGE", style)
                + " every " + waitLoop.timeoutMs() + " ms."
                : "The supernode is kept alive by a minimal internal flow consisting of "
                + label("node", waitLoop.nodeName(), waitLoop.nodeId(), style)
                + " with a self " + edgeLabel("TEDGE", style)
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
                + " waits in a timed loop with a self " + edgeLabel("TEDGE", style)
                + " every " + timeoutEdge.getTimeout() + " ms. "
                + "It reacts via " + edgeLabel("IEDGE", style)
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
                + edgeLabel("TEDGE", style) + " every " + timeoutEdge.getTimeout() + " ms.";
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

    private JSONObject detectForkParallelPattern(
            final BasicNode node,
            final Map<String, BasicNode> nodeById,
            final NarrativeStyle style) {
        if (node == null || node instanceof SuperNode || node.getFEdgeList() == null || node.getFEdgeList().isEmpty()) {
            return null;
        }
        final JSONArray branches = new JSONArray();
        final List<String> branchTargets = new ArrayList<>();
        for (ForkingEdge edge : node.getFEdgeList()) {
            final String targetId = edge.getTargetUnid();
            if (targetId == null || targetId.isBlank()) {
                continue;
            }
            final BasicNode target = nodeById.get(targetId);
            final String targetLabel = label("node", target == null ? "" : target.getName(), targetId, style, true);
            branchTargets.add(targetLabel);
            branches.put(new JSONObject()
                    .put("sourceId", node.getId())
                    .put("targetId", targetId)
                    .put("targetName", target == null ? "" : target.getName()));
        }
        if (branches.isEmpty()) {
            return null;
        }

        final String description = label("Node", node.getName(), node.getId(), style)
                + " starts parallel branches via " + edgeLabel("FEDGE", style)
                + " to " + joinWithConjunction(branchTargets, "and") + ".";
        return new JSONObject()
                .put("patternType", "fork_parallel_branches")
                .put("description", description)
                .put("evidence", new JSONObject()
                        .put("nodeId", node.getId())
                        .put("nodeName", node.getName())
                        .put("branches", branches));
    }

    private JSONObject detectProbabilisticChoicePattern(
            final BasicNode node,
            final Map<String, BasicNode> nodeById,
            final NarrativeStyle style) {
        if (node == null || node instanceof SuperNode || node.getPEdgeList() == null || node.getPEdgeList().isEmpty()) {
            return null;
        }
        final JSONArray branches = new JSONArray();
        final List<String> branchDescriptions = new ArrayList<>();
        for (RandomEdge edge : node.getPEdgeList()) {
            final String targetId = edge.getTargetUnid();
            if (targetId == null || targetId.isBlank()) {
                continue;
            }
            final BasicNode target = nodeById.get(targetId);
            final String targetLabel = label("node", target == null ? "" : target.getName(), targetId, style, true);
            final int probability = edge.getProbability();
            branchDescriptions.add(targetLabel + " (" + probability + "%)");
            branches.put(new JSONObject()
                    .put("sourceId", node.getId())
                    .put("targetId", targetId)
                    .put("targetName", target == null ? "" : target.getName())
                    .put("probability", probability));
        }
        if (branches.isEmpty()) {
            return null;
        }

        final String description = label("Node", node.getName(), node.getId(), style)
                + " selects one branch via " + edgeLabel("PEDGE", style)
                + ": " + joinWithConjunction(branchDescriptions, "or") + ".";
        return new JSONObject()
                .put("patternType", "probabilistic_choice")
                .put("description", description)
                .put("evidence", new JSONObject()
                        .put("nodeId", node.getId())
                        .put("nodeName", node.getName())
                        .put("probabilisticBranches", branches));
    }

    private JSONObject detectConditionalChoicePattern(
            final BasicNode node,
            final Map<String, BasicNode> nodeById,
            final NarrativeStyle style) {
        if (node == null || node instanceof SuperNode || node.getCEdgeList() == null || node.getCEdgeList().isEmpty()) {
            return null;
        }

        final JSONArray branches = new JSONArray();
        final List<String> conditionClauses = new ArrayList<>();
        for (GuargedEdge edge : node.getCEdgeList()) {
            final String targetId = edge.getTargetUnid();
            if (targetId == null || targetId.isBlank()) {
                continue;
            }
            final String condition = expressionToText(edge.getCondition());
            final BasicNode target = nodeById.get(targetId);
            final String targetLabel = label("node", target == null ? "" : target.getName(), targetId, style, true);
            conditionClauses.add(conditionActivationClause(condition, targetLabel));
            branches.put(new JSONObject()
                    .put("sourceId", node.getId())
                    .put("targetId", targetId)
                    .put("targetName", target == null ? "" : target.getName())
                    .put("condition", condition));
        }
        if (branches.isEmpty()) {
            return null;
        }

        final String sourceLabel = label("Node", node.getName(), node.getId(), style, true);
        final String actionSummary = summarizeNodeCommands(node);
        final String intro = actionSummary.isBlank()
                ? "The conditions of the outgoing " + edgeLabel("CEDGE", style)
                + "s are " + conditionalEvaluationWindow(node, style) + "."
                : "After processing the actions of " + sourceLabel + ": "
                + actionSummary + ", the conditions of the outgoing "
                + edgeLabel("CEDGE", style) + "s are "
                + conditionalEvaluationWindow(node, style) + ".";
        final String checks = joinWithSemicolon(conditionClauses);
        final String fallback = fallbackTransitionClause(node, nodeById, style, conditionClauses.size());
        final String description = intro + " " + checks + (fallback.isBlank() ? "" : " " + fallback);
        return new JSONObject()
                .put("patternType", "conditional_choice")
                .put("description", description)
                .put("evidence", new JSONObject()
                        .put("nodeId", node.getId())
                        .put("nodeName", node.getName())
                        .put("conditionalBranches", branches));
    }

    /**
     * Finds a question that waits for its own answer: a node that asks, a node that waits, and a node
     * that keeps what arrived.
     *
     * <p>The discriminator is the reset. A node that sets a variable to empty and then hands to a node
     * that waits for that same variable to become non-empty is unmistakably asking and waiting, and no
     * other shape does that. Matching on "a node with commands followed by a polling node" would also
     * catch every unrelated wait that happens to follow an action.
     *
     * <p>Reported instead of the guarded wait loop and the transition into it, which are the same
     * thing described one edge at a time.
     *
     * @param hopsAlreadyNarrated collects the asking node, whose hop this subsumes
     * @param waitLoopsAlreadyNarrated collects the waiting node, whose polling loop this subsumes
     * @param claimed collects all three nodes, so a sequence chain does not absorb them
     */
    private List<JSONObject> detectAskAndWaitPatterns(
            final SceneFlow flow,
            final Map<String, BasicNode> nodeById,
            final NarrativeStyle style,
            final Set<String> hopsAlreadyNarrated,
            final Set<String> waitLoopsAlreadyNarrated,
            final Set<String> claimed) {
        final List<JSONObject> found = new ArrayList<>();
        final List<BasicNode> nodes = collectBasicNodes(flow);

        for (BasicNode wait : nodes) {
            if (wait instanceof SuperNode || !wait.getCmdList().isEmpty()) {
                continue;
            }
            if (!(wait.getDedge() instanceof TimeoutEdge poll)
                    || !wait.getId().equals(poll.getTargetUnid())
                    || isEmpty(wait.getCEdgeList())) {
                continue;
            }
            final GuargedEdge exit = wait.getCEdgeList().get(0);
            final String condition = expressionToText(exit.getCondition());
            final String channel = channelAwaitedIn(condition);
            if (channel.isEmpty()) {
                continue;
            }

            final BasicNode ask = askingNodeFor(wait, channel, nodes);
            if (ask == null) {
                continue;
            }
            final BasicNode store = nodeById.get(exit.getTargetUnid());

            found.add(describeAskAndWait(ask, wait, store, channel, condition, poll.getTimeout(), style));
            hopsAlreadyNarrated.add(ask.getId());
            waitLoopsAlreadyNarrated.add(wait.getId());
            claimed.add(ask.getId());
            claimed.add(wait.getId());
            if (store != null) {
                claimed.add(store.getId());
            }
        }
        return found;
    }

    /** The variable a guard waits to become non-empty, or empty when the guard is something else. */
    private String channelAwaitedIn(final String condition) {
        if (condition == null || !condition.contains("!=")) {
            return "";
        }
        final String[] sides = condition.split("!=", 2);
        final String left = sides[0].trim();
        final String right = sides[1].trim();
        if (!right.equals("\"\"") || left.isEmpty() || !left.matches("[A-Za-z_][A-Za-z0-9_]*")) {
            return "";
        }
        return left;
    }

    /** The node that hands to this wait node after clearing the very variable it waits on. */
    private BasicNode askingNodeFor(
            final BasicNode wait, final String channel, final List<BasicNode> nodes) {
        for (BasicNode candidate : nodes) {
            if (candidate instanceof SuperNode || candidate.getCmdList().isEmpty()) {
                continue;
            }
            if (!(candidate.getDedge() instanceof EpsilonEdge hop)
                    || !wait.getId().equals(hop.getTargetUnid())) {
                continue;
            }
            for (Command command : candidate.getCmdList()) {
                final String text = command == null ? "" : nonBlank(command.getConcreteSyntax(), "");
                final String normalised = text.replace(" ", "");
                if (normalised.equals(channel + "=\"\"")) {
                    return candidate;
                }
            }
        }
        return null;
    }

    private JSONObject describeAskAndWait(
            final BasicNode ask,
            final BasicNode wait,
            final BasicNode store,
            final String channel,
            final String condition,
            final long pollIntervalMs,
            final NarrativeStyle style) {
        final String scene = scenePlayedBy(ask);
        final StringBuilder description = new StringBuilder()
                .append(label("Node", ask.getName(), ask.getId(), style))
                .append(scene.isEmpty() ? " asks a question" : " plays " + quoted(scene) + " to ask")
                .append(" and then waits for an answer in ")
                .append(quoted(channel))
                .append(", checking every ")
                .append(pollIntervalMs)
                .append(" ms.");
        // A node existing where the answer arrives is not the same as one that does something with
        // it. There are three cases and they matter to an author: the answer is copied somewhere of
        // its own, it is used straight away and then no longer needed, or it is simply dropped.
        final String handling = answerHandling(store, channel);
        switch (handling) {
            case "kept" -> description.append(" Once an answer arrives it is kept at ")
                    .append(label("node", store.getName(), store.getId(), style, true))
                    .append(", so the next question cannot overwrite it.");
            case "used" -> description.append(" Once an answer arrives it is used straight away at ")
                    .append(label("node", store.getName(), store.getId(), style, true))
                    .append(", so it is never needed again.");
            default -> description.append(" Nothing keeps the answer, so the next question will "
                    + "overwrite it before it can be used.");
        }

        return new JSONObject()
                .put("patternType", "ask_and_wait")
                .put("description", description.toString())
                .put("evidence", new JSONObject()
                        .put("askNodeId", ask.getId())
                        .put("waitNodeId", wait.getId())
                        .put("storeNodeId", store == null ? "" : store.getId())
                        .put("answerHandling", answerHandling(store, channel))
                        .put("channel", channel)
                        .put("condition", condition)
                        .put("questionScene", scene)
                        .put("pollIntervalMs", pollIntervalMs));
    }

    /**
     * What becomes of the answer: {@code kept} in a variable of its own, {@code used} straight away as
     * an argument, or {@code dropped}.
     */
    private String answerHandling(final BasicNode store, final String channel) {
        if (store == null) {
            return "dropped";
        }
        if (copiesFrom(store, channel)) {
            return "kept";
        }
        return passesAsArgument(store, channel) ? "used" : "dropped";
    }

    /** Whether a node hands the channel straight to a scene, as doc/IntakeInterview does with its summary. */
    private boolean passesAsArgument(final BasicNode node, final String channel) {
        for (Command command : node.getCmdList()) {
            if (!(command instanceof PlayScenesActivity play)) {
                continue;
            }
            for (Expression argument : play.getArgList()) {
                if (!(argument instanceof StructExpression struct)) {
                    continue;
                }
                for (Assignment field : struct.getExpList()) {
                    final Expression value = field.getInitExpression();
                    if (value != null && channel.equals(nonBlank(value.getConcreteSyntax(), "").trim())) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /** Whether a node copies the channel into something of its own. */
    private boolean copiesFrom(final BasicNode node, final String channel) {
        for (Command command : node.getCmdList()) {
            if (!(command instanceof Assignment assignment)) {
                continue;
            }
            final Expression value = assignment.getInitExpression();
            if (value != null && channel.equals(nonBlank(value.getConcreteSyntax(), "").trim())) {
                return true;
            }
        }
        return false;
    }

    /** The scene a node plays, when it plays exactly one, else empty. */
    private String scenePlayedBy(final BasicNode node) {
        for (Command command : node.getCmdList()) {
            final String text = command == null ? "" : nonBlank(command.getConcreteSyntax(), "");
            final java.util.regex.Matcher matcher =
                    java.util.regex.Pattern.compile("^PlayScene\\s*\\(\\s*\"([^\"]*)\"").matcher(text.trim());
            if (matcher.find()) {
                return matcher.group(1);
            }
        }
        return "";
    }

    /**
     * Finds maximal runs of nodes joined by unconditional edges, so that a sequence reads as one
     * finding rather than as a scatter of separate transitions.
     *
     * <p>A run continues from one node to the next only when the step is unambiguous: the node's
     * default edge is an epsilon edge, the node carries no other outgoing edge that could divert
     * the flow, and the target is reachable from nowhere else. A node with a guard, a fork or an
     * interrupt therefore ends the run rather than extending it, because what follows it is no
     * longer simply "the next step".
     *
     * <p>Runs of two nodes are left to {@code unconditional_transition}. One hop is a transition;
     * three nodes in a row are a sequence.
     *
     * @param sequencedHopSources collects every node whose hop this subsumes, so the caller can
     *                            suppress the per-hop finding
     */
    private List<JSONObject> detectSequenceChains(
            final SceneFlow flow,
            final Map<String, BasicNode> nodeById,
            final NarrativeStyle style,
            final Set<String> sequencedHopSources,
            final Set<String> alreadyClaimed) {
        final List<JSONObject> chains = new ArrayList<>();
        final Map<String, Integer> inDegree = countIncomingEdges(flow);
        final Set<String> consumed = new LinkedHashSet<>(alreadyClaimed);

        for (BasicNode node : collectBasicNodes(flow)) {
            if (consumed.contains(node.getId())) {
                continue;
            }
            final List<BasicNode> run = walkChainFrom(node, nodeById, inDegree, consumed);
            if (run.size() < 3) {
                continue;
            }
            for (int i = 0; i < run.size(); i++) {
                consumed.add(run.get(i).getId());
                if (i < run.size() - 1) {
                    sequencedHopSources.add(run.get(i).getId());
                }
            }
            chains.add(describeChain(run, style));
        }
        return chains;
    }

    private List<BasicNode> walkChainFrom(
            final BasicNode head,
            final Map<String, BasicNode> nodeById,
            final Map<String, Integer> inDegree,
            final Set<String> consumed) {
        final List<BasicNode> run = new ArrayList<>();
        final Set<String> visited = new LinkedHashSet<>();
        BasicNode current = head;
        while (current != null && visited.add(current.getId())) {
            run.add(current);
            final BasicNode next = nextChainStep(current, nodeById, inDegree);
            if (next == null || consumed.contains(next.getId()) || visited.contains(next.getId())) {
                break;
            }
            current = next;
        }
        return run;
    }

    /** The single unambiguous successor of a step, or null when the flow could go elsewhere. */
    private BasicNode nextChainStep(
            final BasicNode node,
            final Map<String, BasicNode> nodeById,
            final Map<String, Integer> inDegree) {
        if (node == null || node instanceof SuperNode) {
            return null;
        }
        if (!isEmpty(node.getCEdgeList()) || !isEmpty(node.getPEdgeList())
                || !isEmpty(node.getFEdgeList()) || !isEmpty(node.getIEdgeList())) {
            return null;
        }
        if (!(node.getDedge() instanceof EpsilonEdge epsilonEdge)) {
            return null;
        }
        final String targetId = epsilonEdge.getTargetUnid();
        if (targetId == null || targetId.isBlank() || targetId.equals(node.getId())) {
            return null;
        }
        final BasicNode target = nodeById.get(targetId);
        if (target == null || target instanceof SuperNode) {
            return null;
        }
        if (inDegree.getOrDefault(targetId, 0) != 1) {
            // Something else also leads here, so this is a meeting point rather than a next step.
            return null;
        }
        return target;
    }

    private boolean isEmpty(final List<?> list) {
        return list == null || list.isEmpty();
    }

    private Map<String, Integer> countIncomingEdges(final SuperNode root) {
        final Map<String, Integer> inDegree = new LinkedHashMap<>();
        for (BasicNode node : collectBasicNodes(root)) {
            for (AbstractEdge edge : node.getEdgeList()) {
                final String targetId = edge == null ? null : edge.getTargetUnid();
                if (targetId != null && !targetId.isBlank()) {
                    inDegree.merge(targetId, 1, Integer::sum);
                }
            }
        }
        for (SuperNode superNode : collectSuperNodes(root)) {
            for (AbstractEdge edge : superNode.getEdgeList()) {
                final String targetId = edge == null ? null : edge.getTargetUnid();
                if (targetId != null && !targetId.isBlank()) {
                    inDegree.merge(targetId, 1, Integer::sum);
                }
            }
        }
        return inDegree;
    }

    private JSONObject describeChain(final List<BasicNode> run, final NarrativeStyle style) {
        final JSONArray steps = new JSONArray();
        final List<String> stepLabels = new ArrayList<>();
        for (BasicNode step : run) {
            steps.put(new JSONObject()
                    .put("nodeId", step.getId())
                    .put("nodeName", nonBlank(step.getName(), step.getId())));
            stepLabels.add(label(null, step.getName(), step.getId(), style));
        }

        final String description = "A sequence of " + run.size() + " steps runs in a fixed order: "
                + joinWithThen(stepLabels) + ". Each step begins once the previous one has finished, "
                + "and the steps are joined by " + edgeLabel("EEDGE", style) + "s.";

        return new JSONObject()
                .put("patternType", "sequence")
                .put("description", description)
                .put("evidence", new JSONObject()
                        .put("stepCount", run.size())
                        .put("firstNodeId", run.get(0).getId())
                        .put("lastNodeId", run.get(run.size() - 1).getId())
                        .put("edgeType", "EEDGE")
                        .put("steps", steps));
    }

    private String joinWithThen(final List<String> parts) {
        final StringBuilder out = new StringBuilder();
        for (int i = 0; i < parts.size(); i++) {
            if (i > 0) {
                out.append(i == 1 ? ", then " : ", then ");
            }
            out.append(parts.get(i));
        }
        return out.toString();
    }

    private JSONObject detectUnconditionalTransitionPattern(
            final BasicNode node,
            final Map<String, BasicNode> nodeById,
            final NarrativeStyle style) {
        if (node == null || node instanceof SuperNode) {
            return null;
        }
        if (node.getCEdgeList() != null && !node.getCEdgeList().isEmpty()) {
            // For guarded branching nodes, the E-edge is the explicit fallback and is narrated there.
            return null;
        }
        final AbstractEdge edge = node.getDedge();
        if (!(edge instanceof EpsilonEdge epsilonEdge)) {
            return null;
        }
        final String targetId = epsilonEdge.getTargetUnid();
        if (targetId == null || targetId.isBlank() || targetId.equals(node.getId())) {
            return null;
        }
        final BasicNode target = nodeById.get(targetId);
        final String targetLabel = label("node", target == null ? "" : target.getName(), targetId, style, true);

        final String description = label("Node", node.getName(), node.getId(), style)
                + " proceeds via " + edgeLabel("EEDGE", style)
                + " to " + targetLabel + ".";
        return new JSONObject()
                .put("patternType", "unconditional_transition")
                .put("description", description)
                .put("evidence", new JSONObject()
                        .put("nodeId", node.getId())
                        .put("nodeName", node.getName())
                        .put("targetId", targetId)
                        .put("targetName", target == null ? "" : target.getName()));
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
                + edgeLabel("TEDGE", style) + " every "
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
        final String actionSummary = summarizeNodeCommands(node);
        if (actionSummary.isBlank()) {
            return "";
        }
        return "Before evaluating the conditional edge, commands of "
                + label("node", node.getName(), node.getId(), style, true)
                + " are processed: " + actionSummary + ".";
    }

    private String summarizeNodeCommands(final BasicNode node) {
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
        return String.join(", ", summarized);
    }

    private String conditionActivationClause(final String condition, final String targetLabel) {
        if (condition == null || condition.isBlank()) {
            return "If an outgoing condition holds, " + targetLabel + " is activated.";
        }
        final java.util.regex.Matcher matcher = java.util.regex.Pattern
                .compile("^\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*==\\s*(.+?)\\s*$")
                .matcher(condition);
        if (matcher.matches()) {
            final String variable = matcher.group(1).trim();
            String value = matcher.group(2).trim();
            if (value.startsWith("\"") && value.endsWith("\"") && value.length() >= 2) {
                value = value.substring(1, value.length() - 1);
                return "If " + variable + " equals \"" + value + "\", " + targetLabel + " is activated.";
            }
            return "If the value of " + variable + " is " + value + ", " + targetLabel + " is activated.";
        }
        return "If the condition \"" + condition + "\" is met, " + targetLabel + " is activated.";
    }

    private String fallbackTransitionClause(
            final BasicNode node,
            final Map<String, BasicNode> nodeById,
            final NarrativeStyle style,
            final int conditionCount) {
        final AbstractEdge dedge = node.getDedge();
        final String conditionRef = conditionCount == 1 ? "this condition" : "these conditions";
        final String beVerb = conditionCount == 1 ? "is" : "are";

        if (dedge instanceof EpsilonEdge epsilonEdge) {
            final String targetId = epsilonEdge.getTargetUnid();
            if (targetId == null || targetId.isBlank() || targetId.equals(node.getId())) {
                return "";
            }
            final BasicNode target = nodeById.get(targetId);
            final String targetLabel = label("node", target == null ? "" : target.getName(), targetId, style, true);
            return "If " + conditionRef + " " + beVerb + " not met, " + targetLabel
                    + " is activated via the " + edgeLabel("EEDGE", style) + ".";
        }

        if (dedge instanceof TimeoutEdge timeoutEdge) {
            final String targetId = timeoutEdge.getTargetUnid();
            if (targetId == null || targetId.isBlank() || targetId.equals(node.getId())) {
                return "";
            }
            final BasicNode target = nodeById.get(targetId);
            final String targetLabel = label("node", target == null ? "" : target.getName(), targetId, style, true);
            return "If " + conditionRef + " " + beVerb + " not met within that timeframe, " + targetLabel
                    + " is activated via the " + edgeLabel("TEDGE", style) + ".";
        }

        return "";
    }

    private String conditionalEvaluationWindow(final BasicNode node, final NarrativeStyle style) {
        final AbstractEdge dedge = node.getDedge();
        if (dedge instanceof TimeoutEdge timeoutEdge && !node.getId().equals(timeoutEdge.getTargetUnid())) {
            return "evaluated within the timeframe defined by the outgoing "
                    + edgeLabel("TEDGE", style) + " (" + timeoutEdge.getTimeout() + " ms)";
        }
        return "evaluated immediately";
    }

    private String fallbackUnconditionalClause(
            final BasicNode node,
            final Map<String, BasicNode> nodeById,
            final NarrativeStyle style) {
        final AbstractEdge dedge = node.getDedge();
        if (!(dedge instanceof EpsilonEdge epsilonEdge)) {
            return "";
        }
        final String targetId = epsilonEdge.getTargetUnid();
        if (targetId == null || targetId.isBlank() || targetId.equals(node.getId())) {
            return "";
        }
        final BasicNode target = nodeById.get(targetId);
        final String targetLabel = label("node", target == null ? "" : target.getName(), targetId, style, true);
        return "If none of these conditions is met, " + targetLabel
                + " is activated via the " + edgeLabel("EEDGE", style) + ".";
    }

    private String edgeLabel(final String canonicalEdge, final NarrativeStyle style) {
        final String human = EdgeLabelMapper.toHumanLabel(canonicalEdge);
        if (style != null && style.isTechnicalAudience()) {
            return human + " (" + EdgeLabelMapper.canonicalize(canonicalEdge) + ")";
        }
        return human;
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

    private String joinWithSemicolon(final List<String> values) {
        if (values == null || values.isEmpty()) {
            return "";
        }
        return String.join(" ", values);
    }

    private JSONArray buildPatternInventory(final JSONArray patterns) {
        final Map<String, Integer> counts = new LinkedHashMap<>();
        final Map<String, Set<String>> idsByType = new LinkedHashMap<>();
        for (int i = 0; i < patterns.length(); i++) {
            final JSONObject pattern = patterns.optJSONObject(i);
            if (pattern == null) {
                continue;
            }
            final String patternType = pattern.optString("patternType", "").trim();
            if (patternType.isBlank()) {
                continue;
            }
            counts.put(patternType, counts.getOrDefault(patternType, 0) + 1);
            final Set<String> ids = idsByType.computeIfAbsent(patternType, key -> new HashSet<>());
            ids.addAll(extractIdsFromEvidence(pattern.optJSONObject("evidence")));
        }
        final List<String> keys = new ArrayList<>(counts.keySet());
        keys.sort(Comparator.naturalOrder());
        final JSONArray inventory = new JSONArray();
        for (String key : keys) {
            final List<String> ids = new ArrayList<>(idsByType.getOrDefault(key, Set.of()));
            ids.sort(Comparator.naturalOrder());
            inventory.put(new JSONObject()
                    .put("patternType", key)
                    .put("count", counts.get(key))
                    .put("ids", new JSONArray(ids)));
        }
        return inventory;
    }

    private Set<String> extractIdsFromEvidence(final JSONObject evidence) {
        final Set<String> ids = new HashSet<>();
        if (evidence == null) {
            return ids;
        }
        for (String key : evidence.keySet()) {
            final Object value = evidence.get(key);
            if (value instanceof String str) {
                if (key.endsWith("Id") && !str.isBlank()) {
                    ids.add(str);
                }
                continue;
            }
            if (value instanceof JSONObject nested) {
                ids.addAll(extractIdsFromEvidence(nested));
                continue;
            }
            if (value instanceof JSONArray array) {
                for (int i = 0; i < array.length(); i++) {
                    final Object item = array.get(i);
                    if (item instanceof JSONObject nestedItem) {
                        ids.addAll(extractIdsFromEvidence(nestedItem));
                    }
                }
            }
        }
        return ids;
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

    public record NarrativeStyle(boolean includeIds, String audience) {
        public NarrativeStyle(boolean includeIds) {
            this(includeIds, "reader-friendly");
        }

        public NarrativeStyle {
            if (audience == null || audience.isBlank()) {
                audience = "reader-friendly";
            }
        }

        public boolean isTechnicalAudience() {
            return "technical".equalsIgnoreCase(audience);
        }
    }
}
