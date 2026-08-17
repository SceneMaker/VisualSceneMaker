package de.dfki.vsm.sceneflow.ir;

import org.json.JSONArray;
import org.json.JSONObject;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class SceneFlowIrTemplateLibrary {
    static final String PATTERN_CATALOG_FILE = "interactive-design-pattern-catalog.json";
    private final List<CatalogPattern> catalogPatterns;

    public SceneFlowIrTemplateLibrary() {
        this(null);
    }

    SceneFlowIrTemplateLibrary(final Path catalogPath) {
        this.catalogPatterns = loadCatalogPatterns(catalogPath);
    }

    public List<JSONObject> generateCandidates(final String situation, final JSONObject snapshot) {
        return generateCandidates(situation, snapshot, ConstraintResolutionMode.PERMISSIVE);
    }

    public List<JSONObject> generateCandidates(
            final String situation,
            final JSONObject snapshot,
            final ConstraintResolutionMode constraintResolutionMode) {
        final String prompt = situation == null ? "" : situation.trim();
        final String lower = prompt.toLowerCase(Locale.ROOT);
        final String rootId = resolveRootId(snapshot);
        final String eventVar = resolveEventVariable(snapshot);
        final ConstraintResolutionMode mode = constraintResolutionMode == null
                ? ConstraintResolutionMode.PERMISSIVE
                : constraintResolutionMode;

        final List<JSONObject> candidates = new ArrayList<>();
        // Offered before the other waiting predicates, which match on "wait" and "until" alone and
        // would otherwise answer "wait until Xenia is ready" with a wait for a button press.
        final List<ReadinessSignal> awaited = readinessSignalsNamedIn(lower, snapshot);
        final boolean waitUntilReady = !awaited.isEmpty() && looksLikeWaitUntilReady(lower);
        if (waitUntilReady) {
            candidates.add(waitUntilReadyTemplate(prompt, rootId, snapshot, awaited, null));
        }
        final boolean askAndWait = !waitUntilReady && looksLikeAskAndWait(lower);
        if (askAndWait) {
            candidates.add(askAndWaitTemplate(prompt, rootId, snapshot));
        }
        // The wait predicate matches on "wait" and "until" alone, so without this an asking
        // situation would also produce a bare wait supernode that asks nothing and stores nothing.
        if (!askAndWait && !waitUntilReady && looksLikeWaitForEvent(lower)) {
            final ConstrainedActivitySpec spec = constrainedActivitySpec(prompt, rootId, eventVar, snapshot, mode);
            candidates.add(constrainedActivityTemplate(spec, snapshot));
        }
        if (looksLikeTimeoutRetry(lower)) {
            candidates.add(timeoutRetryTemplate(prompt, rootId));
        }
        if (looksLikeCommandOnCondition(lower, eventVar)) {
            candidates.add(commandOnConditionTemplate(prompt, rootId, eventVar));
        }
        // Offered before the blanket fallback. A situation such as "first greet the visitor, then
        // explain the study" used to fall through to the constrained-activity template and produce
        // an unrelated wait supernode, because no predicate matched and the fallback is
        // unconditional.
        final List<SequenceStep> steps = splitIntoSteps(prompt, snapshot);
        if (steps.size() >= 2) {
            candidates.add(sequenceTemplate(new SequenceSpec(prompt, rootId, steps), snapshot));
        }
        // Deliberately no blanket fallback. A situation this library does not recognise returns an
        // empty list so the caller can report an honest miss, rather than being answered with a
        // constrained-activity wait template that has nothing to do with what was asked. Callers
        // explain the miss with recognisedSituationHints().
        return candidates;
    }

    /**
     * What this library recognises, phrased for whoever has to act on a miss. Kept next to the
     * predicates above so the two cannot drift apart.
     */
    public static List<String> recognisedSituationHints() {
        return List.of(
                "Waiting until the agents are ready before anything starts: mention ready or "
                        + "connected, and name an agent if only one should be waited for.",
                "Waiting for something to happen: mention waiting, until, or a button being pressed.",
                "Retrying after a delay: mention retry, or timeout together with again.",
                "Acting on a condition: mention if or when.",
                "A sequence of steps: mention first, then, after that, or finally.",
                "Asking someone something and waiting for their answer: mention asking, and answer "
                        + "or reply.");
    }

    private boolean looksLikeWaitForEvent(final String lower) {
        return lower.contains("wait") || lower.contains("until") || lower.contains("pressed");
    }

    /**
     * Recognises asking someone something and waiting for their answer.
     *
     * <p>Needs both halves. An asking cue alone is usually just a step in a sequence, and a waiting
     * cue alone is the constrained-activity wait.
     */
    private boolean looksLikeAskAndWait(final String lower) {
        final boolean asks = lower.contains("ask") || lower.contains("question")
                || lower.contains("prompt");
        final boolean waits = lower.contains("answer") || lower.contains("reply")
                || lower.contains("response") || lower.contains("wait") || lower.contains("until");
        return asks && waits;
    }

    /**
     * Recognises waiting for the agents themselves rather than for something a person does.
     *
     * <p>Needs a readiness cue. A bare "wait until" is the constrained-activity wait, and an agent
     * name alone is usually a step that speaks rather than a gate in front of one.
     */
    private boolean looksLikeWaitUntilReady(final String lower) {
        final boolean readiness = lower.contains("ready") || lower.contains("connect")
                || lower.contains("available");
        final boolean waits = lower.contains("wait") || lower.contains("until")
                || lower.contains("before") || lower.contains("start");
        return readiness && waits;
    }

    /**
     * The readiness signals a situation is about, which is all of them unless it names some.
     *
     * <p>Waiting for every agent is the safe default, and it is what the two-character example
     * project does. Naming one is how an author asks for less.
     */
    private List<ReadinessSignal> readinessSignalsNamedIn(
            final String lower, final JSONObject snapshot) {
        final List<ReadinessSignal> all = readinessSignals(snapshot);
        final List<ReadinessSignal> named = new ArrayList<>();
        for (ReadinessSignal signal : all) {
            final String agent = signal.agentName().toLowerCase(Locale.ROOT);
            if (!agent.isBlank() && lower.contains(agent)) {
                named.add(signal);
            }
        }
        return named.isEmpty() ? all : named;
    }

    /**
     * A gate the flow waits in until every agent it needs can act.
     *
     * <p>{@code continuationNodeId} is what the flow would otherwise have started with, so the gate
     * goes in front of it rather than beside it. Passing null means nothing follows yet, and an empty
     * node is created for the author to carry on from.
     */
    /**
     * The gate on its own, to be put in front of a flow that is being generated anyway.
     *
     * <p>{@code reservedIds} are the ids that flow is about to use. They are not in the snapshot yet,
     * and a gate allocating over them would collide with nodes that do not exist so far.
     *
     * @return null when this project has nothing that reports readiness, in which case there is
     *         nothing to wait for and a gate would never open
     */
    JSONObject readinessGateFor(
            final String situation,
            final String rootId,
            final JSONObject snapshot,
            final String continuationNodeId,
            final Set<String> reservedIds) {
        final List<ReadinessSignal> signals = readinessSignals(snapshot);
        if (signals.isEmpty()) {
            return null;
        }
        return waitUntilReadyTemplate(situation, rootId, snapshot, signals, continuationNodeId, reservedIds);
    }

    private JSONObject waitUntilReadyTemplate(
            final String situation,
            final String rootId,
            final JSONObject snapshot,
            final List<ReadinessSignal> awaited,
            final String continuationNodeId) {
        return waitUntilReadyTemplate(situation, rootId, snapshot, awaited, continuationNodeId, Set.of());
    }

    private JSONObject waitUntilReadyTemplate(
            final String situation,
            final String rootId,
            final JSONObject snapshot,
            final List<ReadinessSignal> awaited,
            final String continuationNodeId,
            final Set<String> reservedIds) {
        final Set<String> existingNodeIds = new LinkedHashSet<>(snapshotNodeIds(snapshot, rootId));
        existingNodeIds.addAll(reservedIds);
        final String superNodeId = "S" + nextIdIndex(existingNodeIds, "S", 100);
        final int nextNodeIdx = nextIdIndex(existingNodeIds, "N", 1000);
        final String waitNodeId = "N" + nextNodeIdx;
        final String afterNodeId = continuationNodeId == null || continuationNodeId.isBlank()
                ? "N" + (nextNodeIdx + 1)
                : continuationNodeId;

        final StringBuilder condition = new StringBuilder();
        final JSONArray waitedFor = new JSONArray();
        for (ReadinessSignal signal : awaited) {
            if (condition.length() > 0) {
                condition.append(" && ");
            }
            condition.append(signal.variable());
            waitedFor.put(new JSONObject()
                    .put("agent", signal.agentName())
                    .put("variable", signal.variable())
                    .put("meansCanAct", signal.meansCanAct()));
        }

        final JSONArray operations = new JSONArray()
                .put(new JSONObject()
                        .put("op", "create_supernode")
                        .put("opId", "gate-" + superNodeId)
                        .put("reason", "Somewhere for the flow to be while it waits.")
                        .put("parentSuperNodeId", rootId)
                        .put("superNodeId", superNodeId)
                        .put("name", "WaitUntilReady")
                        .put("isStartNode", true))
                .put(new JSONObject()
                        .put("op", "create_node")
                        .put("opId", "gate-wait-" + waitNodeId)
                        .put("reason", "The flow has to occupy a node while the gate is closed.")
                        .put("parentSuperNodeId", superNodeId)
                        .put("nodeId", waitNodeId)
                        .put("name", "Waiting")
                        .put("isStartNode", true))
                .put(new JSONObject()
                        .put("op", "create_edge")
                        .put("opId", "gate-tick-" + waitNodeId)
                        .put("reason", "Keeps the flow inside the gate. The interval decides nothing "
                                + "about how quickly readiness is noticed.")
                        .put("edgeId", "WaitUntilReady_Tick")
                        .put("edgeType", "TEDGE")
                        .put("sourceNodeId", waitNodeId)
                        .put("targetNodeId", waitNodeId)
                        .put("payload", new JSONObject().put("timeoutMs", 1000)));

        if (continuationNodeId == null || continuationNodeId.isBlank()) {
            operations.put(new JSONObject()
                    .put("op", "create_node")
                    .put("opId", "gate-after-" + afterNodeId)
                    .put("reason", "Where the flow carries on once everything is ready.")
                    .put("parentSuperNodeId", rootId)
                    .put("nodeId", afterNodeId)
                    .put("name", "Ready"));
        }

        operations.put(new JSONObject()
                .put("op", "create_edge")
                .put("opId", "gate-release-" + superNodeId)
                .put("reason", "On the supernode rather than on the waiting node, so it fires the "
                        + "moment every agent is ready instead of at the next tick.")
                .put("edgeId", "WaitUntilReady_Release")
                .put("edgeType", "IEDGE")
                .put("sourceNodeId", superNodeId)
                .put("targetNodeId", afterNodeId)
                .put("payload", new JSONObject().put("conditionText", condition.toString())));

        final JSONArray assumptions = new JSONArray();
        assumptions.put(awaited.size() == 1
                ? "The flow waits for " + awaited.get(0).agentName() + " and starts as soon as it "
                        + "is ready."
                : "The flow waits for all " + awaited.size() + " agents and starts only once every "
                        + "one of them is ready. This is one condition rather than several separate "
                        + "waits.");
        for (ReadinessSignal signal : awaited) {
            if (!signal.meansCanAct()) {
                assumptions.put(signal.agentName() + " reports only that it has connected, not that "
                        + "it can act yet. Speaking too early can fail without any sign of it.");
            }
        }
        assumptions.put("Nothing says how long the wait may take. A gate whose condition never "
                + "becomes true looks exactly like one that is merely slow.");
        assumptions.put("The waiting node does nothing. Put a scene inside the gate if the person "
                + "should be told that something is coming.");

        final PatternSelection selection = selectPattern(Map.of("situation.shape", "wait-until-ready"));
        return new JSONObject()
                .put("irVersion", "1.0")
                .put("mode", "patch")
                .put("metadata", metadata("template-wait-until-ready", situation)
                        .put("interactiveDesignPattern", new JSONObject()
                                .put("selectedPatternId", selection.patternId())
                                .put("selectionReason", selection.reason())
                                .put("resolvedMeta", new JSONObject()
                                        .put("situation", new JSONObject().put("shape", "wait-until-ready"))
                                        .put("agentCount", awaited.size())))
                        .put("waitsFor", waitedFor)
                        .put("gateSuperNodeId", superNodeId)
                        .put("continuationNodeId", afterNodeId))
                .put("assumptions", assumptions)
                .put("operations", operations);
    }

    private boolean looksLikeTimeoutRetry(final String lower) {
        return lower.contains("retry") || (lower.contains("timeout") && lower.contains("again"));
    }

    private boolean looksLikeCommandOnCondition(final String lower, final String eventVar) {
        return lower.contains("if")
                || lower.contains("when")
                || (eventVar != null && !eventVar.isBlank() && lower.contains(eventVar.toLowerCase(Locale.ROOT)));
    }

    private JSONObject constrainedActivityTemplate(final ConstrainedActivitySpec spec, final JSONObject snapshot) {
        final Set<String> existingNodeIds = snapshotNodeIds(snapshot, spec.rootId());
        final int nextSuperNodeIdx = nextIdIndex(existingNodeIds, "S", 100);
        final int nextNodeIdx = nextIdIndex(existingNodeIds, "N", 1000);

        final String suffix = sanitizeId(spec.constraintLabel());
        final String superNodeId = "S" + nextSuperNodeIdx;
        final String waitNodeId = "N" + nextNodeIdx;
        final String activityNodeId = spec.activityType() == ActivityType.MINIMAL_LIVENESS ? null : "N" + (nextNodeIdx + 1);
        final String policyEdgeId = "WaitPolicy_" + suffix;
        final int nextAfterNodeIdx = activityNodeId == null ? nextNodeIdx + 1 : nextNodeIdx + 2;

        final JSONArray operations = new JSONArray()
                .put(new JSONObject()
                        .put("op", "create_supernode")
                        .put("parentSuperNodeId", spec.rootId())
                        .put("superNodeId", superNodeId)
                        .put("name", "ConstrainedActivity_" + suffix)
                        .put("isStartNode", true))
                .put(new JSONObject()
                        .put("op", "create_node")
                        .put("parentSuperNodeId", superNodeId)
                        .put("nodeId", waitNodeId)
                        .put("name", "Waiting")
                        .put("isStartNode", true));

        if (activityNodeId == null) {
            operations.put(new JSONObject()
                    .put("op", "create_edge")
                    .put("edgeId", policyEdgeId)
                    .put("edgeType", "TEDGE")
                    .put("sourceNodeId", waitNodeId)
                    .put("targetNodeId", waitNodeId)
                    .put("payload", new JSONObject().put("timeoutMs", spec.policyIntervalMs())));
        } else {
            operations.put(new JSONObject()
                    .put("op", "create_node")
                    .put("parentSuperNodeId", superNodeId)
                    .put("nodeId", activityNodeId)
                    .put("name", switch (spec.activityType()) {
                        case REMIND -> "Reminder";
                        case GENERIC_ACTIVITY -> "Activity";
                        case MINIMAL_LIVENESS -> "Waiting";
                    }));
            operations.put(new JSONObject()
                    .put("op", "create_edge")
                    .put("edgeId", policyEdgeId)
                    .put("edgeType", "TEDGE")
                    .put("sourceNodeId", waitNodeId)
                    .put("targetNodeId", activityNodeId)
                    .put("payload", new JSONObject().put("timeoutMs", spec.policyIntervalMs())));
            operations.put(new JSONObject()
                    .put("op", "create_edge")
                    .put("edgeId", "ConstrainedActivity_Back_" + suffix)
                    .put("edgeType", "TEDGE")
                    .put("sourceNodeId", activityNodeId)
                    .put("targetNodeId", waitNodeId)
                    .put("payload", new JSONObject().put("timeoutMs", 1000)));
        }

        return new JSONObject()
                .put("irVersion", "1.0")
                .put("mode", "patch")
                .put("metadata", metadata("template-constrained-activity", spec.situation())
                        .put("constraintResolution", new JSONObject()
                                .put("mode", spec.constraintResolutionMode().name().toLowerCase(Locale.ROOT))
                                .put("resolvedLabels", new JSONArray(spec.constraintLabels()))
                                .put("unresolvedLabels", new JSONArray(spec.unresolvedConstraintLabels())))
                        .put("promptResolution", new JSONObject()
                                .put("activityKind", spec.activityKind())
                                .put("interruptibility", spec.interruptibility())
                                .put("confidence", spec.promptResolutionConfidence())
                                .put("ambiguities", new JSONArray(spec.promptResolutionAmbiguities())))
                        .put("interactiveDesignPattern", new JSONObject()
                                .put("selectedPatternId", spec.selectedPatternId())
                                .put("selectionReason", spec.selectionReason())
                                .put("resolvedMeta", new JSONObject()
                                        .put("constraint", new JSONObject()
                                                .put("variable", spec.constraintVariable())
                                                .put("operator", "==")
                                                .put("value", spec.constraintLabel())
                                                .put("values", new JSONArray(spec.constraintLabels())))
                                        .put("constrainedActivity", new JSONObject()
                                                .put("kind", spec.activityKind())
                                                .put("parameters", new JSONObject()))
                                        .put("policy", new JSONObject()
                                                .put("intervalMs", spec.policyIntervalMs())
                                                .put("interruptibility", spec.interruptibility())
                                                .put("maxRepeats", JSONObject.NULL))
                                        .put("completion", new JSONObject()
                                                .put("targetNodeStrategy", "create_after_node")))))
                .put("assumptions", new JSONArray()
                        .put("interactive-design-pattern: constrained-activity")
                        .put("selected-pattern-id: " + spec.selectedPatternId())
                        .put("Constraint variable " + spec.constraintVariable() + " exists or will be auto-created.")
                        .put("Resolved activity kind: " + spec.activityKind()))
                .put("operations", appendConstraintExitOps(
                        operations,
                        spec,
                        superNodeId,
                        nextAfterNodeIdx));
    }

    private JSONObject timeoutRetryTemplate(final String situation, final String rootId) {
        final String superNodeId = "TimeoutRetry";
        final String nodeId = "RetryLoop";
        final String afterNodeId = "AfterRetry";
        return new JSONObject()
                .put("irVersion", "1.0")
                .put("mode", "patch")
                .put("metadata", metadata("template-timeout-retry", situation))
                .put("operations", new JSONArray()
                        .put(new JSONObject()
                                .put("op", "add_variable_definition")
                                .put("ownerNodeId", rootId)
                                .put("varDef", new JSONObject()
                                        .put("name", "retryCounter")
                                        .put("type", "Int")
                                        .put("expression", "0")))
                        .put(new JSONObject()
                                .put("op", "create_supernode")
                                .put("parentSuperNodeId", rootId)
                                .put("superNodeId", superNodeId)
                                .put("name", superNodeId))
                        .put(new JSONObject()
                                .put("op", "create_node")
                                .put("parentSuperNodeId", superNodeId)
                                .put("nodeId", nodeId)
                                .put("name", nodeId))
                        .put(new JSONObject()
                                .put("op", "create_edge")
                                .put("edgeId", "RetryLoopTimeout")
                                .put("edgeType", "TEDGE")
                                .put("sourceNodeId", nodeId)
                                .put("targetNodeId", nodeId)
                                .put("payload", new JSONObject().put("timeoutMs", 1000)))
                        .put(new JSONObject()
                                .put("op", "create_node")
                                .put("parentSuperNodeId", rootId)
                                .put("nodeId", afterNodeId)
                                .put("name", afterNodeId))
                        .put(new JSONObject()
                                .put("op", "create_edge")
                                .put("edgeId", "RetryExit")
                                .put("edgeType", "CEDGE")
                                .put("sourceNodeId", superNodeId)
                                .put("targetNodeId", afterNodeId)
                                .put("payload", new JSONObject().put("conditionText", "retryCounter >= 3"))));
    }

    private JSONObject commandOnConditionTemplate(final String situation, final String rootId, final String eventVar) {
        final String nodeId = "ConditionalAction";
        return new JSONObject()
                .put("irVersion", "1.0")
                .put("mode", "patch")
                .put("metadata", metadata("template-command-on-condition", situation))
                .put("operations", new JSONArray()
                        .put(new JSONObject()
                                .put("op", "create_node")
                                .put("parentSuperNodeId", rootId)
                                .put("nodeId", nodeId)
                                .put("name", nodeId))
                        .put(new JSONObject()
                                .put("op", "add_node_command")
                                .put("nodeId", nodeId)
                                .put("commandText", "retryCounter = retryCounter + 1"))
                        .put(new JSONObject()
                                .put("op", "create_edge")
                                .put("edgeId", "ConditionalActionGuard")
                                .put("edgeType", "CEDGE")
                                .put("sourceNodeId", nodeId)
                                .put("targetNodeId", nodeId)
                                .put("payload", new JSONObject().put("conditionText", eventVar + " != \"\""))));
    }

    /**
     * Builds the ask, wait, store shape, modelled on doc/IntakeInterview.
     *
     * <p>Three details decide whether it works, and all three are easy to get wrong. The channel is
     * cleared in the node that asks, never in the one that waits, because a reset in the wait node
     * would run on every poll and the answer would never be seen. The wait node carries no commands,
     * because a guard is only evaluated after a node's commands finish, so anything blocking there
     * delays the check by its own duration. And the answer is copied into a variable of its own,
     * because the channel is shared by every question in a project and the next question clears it.
     */
    private JSONObject askAndWaitTemplate(
            final String situation, final String rootId, final JSONObject snapshot) {
        final Set<String> usedIds = new LinkedHashSet<>(snapshotNodeIds(snapshot, rootId));
        usedIds.addAll(snapshotNodeNames(snapshot));

        final String subject = questionSubject(situation);
        final String askId = uniqueNodeId("Ask" + pascalCase(subject), usedIds);
        final String waitId = uniqueNodeId("Wait" + pascalCase(subject), usedIds);
        final String storeId = uniqueNodeId("Store" + pascalCase(subject), usedIds);

        final Set<String> sceneNames = sceneNames(snapshot);
        final String questionScene = resolveSceneName("ask " + subject, sceneNames);
        final String channel = resolveAnswerChannel(snapshot);
        final String store = uniqueVariableName(snakeCase(subject), channel, snapshot);
        final int intervalMs = extractPollIntervalMs(situation);

        final JSONArray operations = new JSONArray();
        for (String variable : new String[] {channel, store}) {
            if (!snapshotVariableNames(snapshot).contains(variable)) {
                operations.put(new JSONObject()
                        .put("op", "add_variable_definition")
                        .put("opId", "declare-" + variable)
                        .put("reason", variable.equals(channel)
                                ? "Where the answer arrives."
                                : "Where this answer is kept, so the next question cannot overwrite it.")
                        .put("ownerNodeId", rootId)
                        .put("varDef", new JSONObject()
                                .put("name", variable)
                                .put("type", "String")
                                .put("expression", "\"\"")));
            }
        }

        operations.put(node(askId, "Ask the question and clear the answer channel.", rootId, 120, true));
        operations.put(command(askId, "PlayScene(\"" + questionScene + "\")", "Ask the question."));
        operations.put(command(askId, channel + " = \"\"",
                "Clear the channel here rather than in the wait node: a reset that ran on every poll "
                        + "would discard the answer."));

        // No commands on the wait node. A guard is only evaluated once a node's commands have
        // finished, so anything here would delay noticing the answer by its own duration.
        operations.put(node(waitId, "Wait for the answer. Deliberately carries no commands.",
                rootId, 440, false));

        operations.put(node(storeId, "Keep the answer under a name of its own.", rootId, 760, false));
        operations.put(command(storeId, store + " = " + channel,
                "Copy the answer out before the next question clears the shared channel."));

        operations.put(new JSONObject()
                .put("op", "create_edge")
                .put("opId", "ask-to-wait")
                .put("reason", "Start waiting once the question has been asked.")
                .put("edgeId", "Seq" + askId + "To" + waitId)
                .put("edgeType", "EEDGE")
                .put("sourceNodeId", askId)
                .put("targetNodeId", waitId));
        operations.put(new JSONObject()
                .put("op", "create_edge")
                .put("opId", "answer-arrived")
                .put("reason", "Continue as soon as the channel holds something.")
                .put("edgeId", "Answer" + waitId)
                .put("edgeType", "CEDGE")
                .put("sourceNodeId", waitId)
                .put("targetNodeId", storeId)
                .put("payload", new JSONObject().put("conditionText", channel + " != \"\"")));
        operations.put(new JSONObject()
                .put("op", "create_edge")
                .put("opId", "poll")
                .put("reason", "Check again after the interval. This is the worst-case delay before "
                        + "the agent notices an answer.")
                .put("edgeId", "Poll" + waitId)
                .put("edgeType", "TEDGE")
                .put("sourceNodeId", waitId)
                .put("targetNodeId", waitId)
                .put("payload", new JSONObject().put("timeoutMs", intervalMs)));

        final JSONArray assumptions = new JSONArray()
                .put(writesChannel(snapshot, channel)
                        ? "The answer arrives in \"" + channel + "\", which a screen in this project "
                                + "already hands back."
                        : "The answer arrives in \"" + channel + "\". Something has to write it: a "
                                + "screen control carrying sendsVar, or a plugin declaring it under "
                                + "variables.writes. Nothing in this patch provides that.")
                .put("An empty channel means no answer yet, so an empty answer cannot be told apart "
                        + "from silence.")
                .put("Polling notices an answer up to " + intervalMs + " ms late. Reacting immediately "
                        + "would need the channel to be an Event variable and an interrupt edge.")
                .put("The final step has no outgoing edge and therefore ends here.");
        if (!sceneNames.contains(questionScene)) {
            assumptions.put("Scene \"" + questionScene + "\" does not exist yet and has to be written.");
        }

        final PatternSelection selection = selectPattern(Map.of("situation.shape", "ask-and-wait"));
        return new JSONObject()
                .put("irVersion", "1.0")
                .put("mode", "patch")
                .put("metadata", metadata("template-ask-and-wait", situation)
                        .put("interactiveDesignPattern", new JSONObject()
                                .put("selectedPatternId", selection.patternId())
                                .put("selectionReason", selection.reason())
                                .put("resolvedMeta", new JSONObject()
                                        .put("situation", new JSONObject().put("shape", "ask-and-wait"))
                                        .put("policy", new JSONObject().put("pollIntervalMs", intervalMs))))
                        .put("answerChannel", channel)
                        .put("answerStore", store)
                        .put("questionScene", questionScene)
                        .put("scenesToAuthor", sceneNames.contains(questionScene)
                                ? new JSONArray()
                                : new JSONArray().put(questionScene)))
                .put("assumptions", assumptions)
                .put("operations", operations);
    }

    private JSONObject node(final String nodeId, final String reason, final String rootId,
                            final int x, final boolean isStartNode) {
        final JSONObject op = new JSONObject()
                .put("op", "create_node")
                .put("opId", "node-" + nodeId)
                .put("reason", reason)
                .put("parentSuperNodeId", rootId)
                .put("nodeId", nodeId)
                .put("name", nodeId)
                .put("position", new JSONObject().put("x", x).put("y", 340));
        if (isStartNode) {
            op.put("isStartNode", true);
        }
        return op;
    }

    private JSONObject command(final String nodeId, final String commandText, final String reason) {
        return new JSONObject()
                .put("op", "add_node_command")
                .put("opId", "cmd-" + nodeId + "-" + Math.abs(commandText.hashCode()))
                .put("reason", reason)
                .put("nodeId", nodeId)
                .put("commandText", commandText);
    }


    /** Whether anything in the project already hands {@code channel} back to the flow. */
    private boolean writesChannel(final JSONObject snapshot, final String channel) {
        final JSONObject screens = snapshot == null ? null : snapshot.optJSONObject("screens");
        final JSONArray defined = screens == null ? null : screens.optJSONArray("screens");
        for (int i = 0; defined != null && i < defined.length(); i++) {
            final JSONArray writes = defined.optJSONObject(i) == null
                    ? null
                    : defined.getJSONObject(i).optJSONArray("writesVariables");
            for (int j = 0; writes != null && j < writes.length(); j++) {
                if (channel.equals(writes.optString(j, ""))) {
                    return true;
                }
            }
        }
        final JSONObject project = snapshot == null ? null : snapshot.optJSONObject("project");
        final JSONArray plugins = project == null ? null : project.optJSONArray("plugins");
        for (int i = 0; plugins != null && i < plugins.length(); i++) {
            final JSONArray writes = plugins.optJSONObject(i) == null
                    ? null
                    : plugins.getJSONObject(i).optJSONArray("writesVariables");
            for (int j = 0; writes != null && j < writes.length(); j++) {
                final JSONObject entry = writes.optJSONObject(j);
                if (entry != null && channel.equals(entry.optString("boundTo", entry.optString("name", "")))) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * The channel an answer arrives in.
     *
     * <p>Prefers one a screen already writes, since that is a channel the project demonstrably has a
     * way to fill. Falls back to the conventional name, which the assumptions then flag as needing a
     * source.
     */
    private String resolveAnswerChannel(final JSONObject snapshot) {
        final JSONObject screens = snapshot == null ? null : snapshot.optJSONObject("screens");
        final JSONArray defined = screens == null ? null : screens.optJSONArray("screens");
        final Set<String> declared = snapshotVariableNames(snapshot);
        for (int i = 0; defined != null && i < defined.length(); i++) {
            final JSONArray writes = defined.optJSONObject(i) == null
                    ? null
                    : defined.getJSONObject(i).optJSONArray("writesVariables");
            for (int j = 0; writes != null && j < writes.length(); j++) {
                final String candidate = writes.optString(j, "").trim();
                if (!candidate.isEmpty() && declared.contains(candidate)) {
                    return candidate;
                }
            }
        }
        return "user_input";
    }

    private Set<String> snapshotVariableNames(final JSONObject snapshot) {
        final Set<String> names = new LinkedHashSet<>();
        final JSONObject flow = snapshot == null ? null : snapshot.optJSONObject("flow");
        final JSONArray variables = flow == null ? null : flow.optJSONArray("variables");
        for (int i = 0; variables != null && i < variables.length(); i++) {
            final JSONObject variable = variables.optJSONObject(i);
            final String name = variable == null ? "" : variable.optString("name", "").trim();
            if (!name.isEmpty()) {
                names.add(name);
            }
        }
        return names;
    }

    /**
     * A variable of this answer's own.
     *
     * <p>Never an existing variable: reusing one would repurpose something another part of the flow
     * may depend on. And never the channel itself, or the answer would be copied over itself and
     * lost the moment the next question clears it.
     */
    private String uniqueVariableName(final String base, final String channel, final JSONObject snapshot) {
        final String candidate = base.isBlank() ? "answer" : base;
        final Set<String> taken = new LinkedHashSet<>(snapshotVariableNames(snapshot));
        taken.add(channel);
        if (!taken.contains(candidate)) {
            return candidate;
        }
        String suffixed = candidate + "_answer";
        int n = 2;
        while (taken.contains(suffixed)) {
            suffixed = candidate + "_answer" + n++;
        }
        return suffixed;
    }

    /** What is being asked about, used to name the nodes and the variable holding the answer. */
    private String questionSubject(final String situation) {
        final String cleaned = situation == null ? "" : situation
                .replaceAll("(?i)\\b(please|the agent should|the agent|ask(s|ing)?|for|wait(s|ing)?"
                        + "|until|their|his|her|the|a|an|and|then|answer|reply|response|user|person"
                        + "|visitor)\\b", " ")
                .replaceAll("[^A-Za-z0-9 ]", " ")
                .trim();
        return cleaned.isBlank() ? "answer" : cleaned;
    }

    private int extractPollIntervalMs(final String situation) {
        if (situation != null) {
            final Matcher ms = Pattern.compile("(\\d+)\\s*(ms|millisecond)").matcher(
                    situation.toLowerCase(Locale.ROOT));
            if (ms.find()) {
                return Math.max(50, Integer.parseInt(ms.group(1)));
            }
            final Matcher sec = Pattern.compile("(\\d+)\\s*(second|seconds|sec)\\b").matcher(
                    situation.toLowerCase(Locale.ROOT));
            if (sec.find()) {
                return Math.max(50, Integer.parseInt(sec.group(1)) * 1000);
            }
        }
        // What doc/IntakeInterview uses: responsive enough to feel immediate, cheap enough to ignore.
        return 500;
    }

    /**
     * Connectives that mark a step boundary in a described sequence. Ordered longest first so that
     * "and then" wins over "then".
     */
    private static final Pattern STEP_SEPARATOR = Pattern.compile(
            "\\s*(?:[;.]|\\b(?:and\\s+then|after\\s+that|after\\s+which|afterwards|finally|lastly"
                    + "|then|next)\\b)\\s*",
            Pattern.CASE_INSENSITIVE);

    /** A leading ordinal on the first step carries no content of its own. */
    private static final Pattern LEADING_ORDINAL = Pattern.compile(
            "^(?:first(?:ly)?|to\\s+start(?:\\s+with)?|begin\\s+by|start\\s+by)\\b[,:]?\\s*",
            Pattern.CASE_INSENSITIVE);

    private static final Pattern LEADING_FILLER = Pattern.compile(
            "^(?:the\\s+agent\\s+should|the\\s+agent|it\\s+should|please|i\\s+want\\s+(?:to|the\\s+agent\\s+to))"
                    + "\\b[,:]?\\s*",
            Pattern.CASE_INSENSITIVE);

    private static final Set<String> NAME_STOPWORDS = Set.of(
            "the", "a", "an", "to", "for", "of", "and", "with", "that", "this", "then", "some", "any");

    /**
     * Splits a described situation into ordered steps.
     *
     * <p>Returns fewer than two steps when the text does not describe a sequence, which is how the
     * caller decides whether this template applies at all.
     */
    private List<SequenceStep> splitIntoSteps(final String prompt, final JSONObject snapshot) {
        final List<SequenceStep> steps = new ArrayList<>();
        if (prompt == null || prompt.isBlank()) {
            return steps;
        }
        final String withoutFiller = LEADING_FILLER.matcher(prompt.trim()).replaceFirst("");
        final String[] chunks = STEP_SEPARATOR.split(withoutFiller);

        // Seeded with the project's existing node ids so a derived name cannot collide with a node
        // that is already there, which the validator would otherwise report as NODE_DUPLICATE. Also
        // seeded with existing node *names*, because the derived id doubles as the display name and
        // two nodes showing the same name on the canvas is needlessly confusing.
        final Set<String> usedIds = new LinkedHashSet<>(snapshotNodeIds(snapshot, resolveRootId(snapshot)));
        usedIds.addAll(snapshotNodeNames(snapshot));
        final Set<String> sceneNames = sceneNames(snapshot);
        for (final String rawChunk : chunks) {
            final String chunk = LEADING_ORDINAL.matcher(rawChunk.trim()).replaceFirst("").trim();
            if (chunk.length() < 2) {
                continue;
            }
            final String nodeId = uniqueNodeId(pascalCase(chunk), usedIds);
            steps.add(new SequenceStep(chunk, nodeId, resolveSceneName(chunk, sceneNames)));
        }
        return steps;
    }

    /**
     * Picks the scene a step should play. An existing scene whose name matches the step wins;
     * otherwise a name is derived and the scene has to be authored, which the caller records as an
     * assumption and {@code SCENE_REF_UNKNOWN} then reports.
     */
    private String resolveSceneName(final String stepText, final Set<String> sceneNames) {
        final String derived = snakeCase(stepText);
        for (final String candidate : sceneNames) {
            if (candidate.equalsIgnoreCase(derived) || candidate.equalsIgnoreCase(pascalCase(stepText))) {
                return candidate;
            }
        }
        // A shorter derived form catches "greet the visitor" against a scene simply called "greet".
        final String head = snakeCase(firstWords(stepText, 1));
        for (final String candidate : sceneNames) {
            if (candidate.equalsIgnoreCase(head)) {
                return candidate;
            }
        }
        return derived;
    }

    private Set<String> snapshotNodeNames(final JSONObject snapshot) {
        final Set<String> names = new LinkedHashSet<>();
        final JSONObject flow = snapshot == null ? null : snapshot.optJSONObject("flow");
        final JSONArray nodes = flow == null ? null : flow.optJSONArray("nodes");
        for (int i = 0; nodes != null && i < nodes.length(); i++) {
            final JSONObject node = nodes.optJSONObject(i);
            final String name = node == null ? "" : node.optString("name", "").trim();
            if (!name.isEmpty()) {
                names.add(name);
            }
        }
        return names;
    }

    private Set<String> sceneNames(final JSONObject snapshot) {
        final Set<String> names = new LinkedHashSet<>();
        final JSONObject script = snapshot == null ? null : snapshot.optJSONObject("script");
        final JSONArray scenes = script == null ? null : script.optJSONArray("scenes");
        for (int i = 0; scenes != null && i < scenes.length(); i++) {
            final JSONObject scene = scenes.optJSONObject(i);
            final String name = scene == null ? "" : scene.optString("name", "").trim();
            if (!name.isEmpty()) {
                names.add(name);
            }
        }
        return names;
    }

    private JSONObject sequenceTemplate(final SequenceSpec spec, final JSONObject snapshot) {
        final JSONArray operations = new JSONArray();
        final Set<String> sceneNames = sceneNames(snapshot);
        final List<String> scenesToAuthor = new ArrayList<>();

        for (int i = 0; i < spec.steps().size(); i++) {
            final SequenceStep step = spec.steps().get(i);

            final JSONObject createNode = new JSONObject()
                    .put("op", "create_node")
                    .put("opId", "sequence-step-" + (i + 1))
                    .put("reason", "Step " + (i + 1) + " of " + spec.steps().size() + ": " + step.text())
                    .put("parentSuperNodeId", spec.rootId())
                    .put("nodeId", step.nodeId())
                    .put("name", step.nodeId())
                    .put("comment", step.text())
                    // Emitted here rather than left to the pipeline, because the compiler has no
                    // fallback layout and a chain should read left to right.
                    .put("position", new JSONObject()
                            .put("x", 120 + i * 320)
                            .put("y", 340));
            if (i == 0) {
                createNode.put("isStartNode", true);
            }
            operations.put(createNode);

            operations.put(new JSONObject()
                    .put("op", "add_node_command")
                    .put("opId", "sequence-step-" + (i + 1) + "-command")
                    .put("reason", "What step " + (i + 1) + " does.")
                    .put("nodeId", step.nodeId())
                    // Double quotes are required: a single-quoted name is lexed as an identifier and
                    // silently becomes a variable reference.
                    .put("commandText", "PlayScene(\"" + step.sceneName() + "\")"));

            if (!sceneNames.contains(step.sceneName())) {
                scenesToAuthor.add(step.sceneName());
            }

            if (i > 0) {
                final SequenceStep previous = spec.steps().get(i - 1);
                operations.put(new JSONObject()
                        .put("op", "create_edge")
                        .put("opId", "sequence-edge-" + i)
                        .put("reason", "Continue to step " + (i + 1) + " once step " + i + " has finished.")
                        .put("edgeId", "Seq" + previous.nodeId() + "To" + step.nodeId())
                        .put("edgeType", "EEDGE")
                        .put("sourceNodeId", previous.nodeId())
                        .put("targetNodeId", step.nodeId()));
            }
        }

        final JSONArray assumptions = new JSONArray()
                // Assumptions are shown to authors, so they name what a step does rather than the
                // edge type that carries it.
                .put("Each step waits for the one before it. That holds because playing a scene "
                        + "keeps the step busy until the scene has finished. A step that starts a "
                        + "plugin action and does not wait for it would overlap the next step, and "
                        + "would need the plugin to report back when it is done.")
                .put("The first step is marked as a start node, so the sequence begins when the "
                        + "project starts. Attach it to an existing node instead if it should run "
                        + "at a particular point.")
                .put("The final step has no outgoing edge and therefore ends the sequence.");
        if (!scenesToAuthor.isEmpty()) {
            assumptions.put("These scenes do not exist yet and have to be authored: "
                    + String.join(", ", scenesToAuthor) + ".");
        }

        // Attributed through the same catalogue selection as every other template, so the pipeline
        // report can name the pattern this realises and surface its scientific sources.
        final PatternSelection selection = selectPattern(Map.of("situation.shape", "sequence"));

        return new JSONObject()
                .put("irVersion", "1.0")
                .put("mode", "patch")
                .put("metadata", metadata("template-sequence", spec.situation())
                        .put("interactiveDesignPattern", new JSONObject()
                                .put("selectedPatternId", selection.patternId())
                                .put("selectionReason", selection.reason())
                                .put("resolvedMeta", new JSONObject()
                                        .put("situation", new JSONObject().put("shape", "sequence"))
                                        .put("sequence", new JSONObject()
                                                .put("stepCount", spec.steps().size()))))
                        .put("stepCount", spec.steps().size())
                        .put("steps", new JSONArray(spec.steps().stream()
                                .map(step -> new JSONObject()
                                        .put("text", step.text())
                                        .put("nodeId", step.nodeId())
                                        .put("scene", step.sceneName()))
                                .toList()))
                        .put("scenesToAuthor", new JSONArray(scenesToAuthor)))
                .put("assumptions", assumptions)
                .put("operations", operations);
    }

    private String firstWords(final String text, final int count) {
        final String[] words = text.trim().split("\\s+");
        final StringBuilder out = new StringBuilder();
        int taken = 0;
        for (final String word : words) {
            final String cleaned = word.replaceAll("[^A-Za-z0-9]", "");
            if (cleaned.isEmpty() || NAME_STOPWORDS.contains(cleaned.toLowerCase(Locale.ROOT))) {
                continue;
            }
            out.append(out.length() == 0 ? "" : " ").append(cleaned);
            if (++taken == count) {
                break;
            }
        }
        return out.length() == 0 ? text.trim() : out.toString();
    }

    private String pascalCase(final String text) {
        final StringBuilder out = new StringBuilder();
        for (final String word : firstWords(text, 3).split("\\s+")) {
            if (word.isEmpty()) {
                continue;
            }
            out.append(Character.toUpperCase(word.charAt(0)))
                    .append(word.substring(1).toLowerCase(Locale.ROOT));
        }
        return out.length() == 0 ? "Step" : out.toString();
    }

    private String snakeCase(final String text) {
        return firstWords(text, 3).toLowerCase(Locale.ROOT).replaceAll("\\s+", "_");
    }

    private String uniqueNodeId(final String base, final Set<String> used) {
        String candidate = base;
        int suffix = 2;
        while (!used.add(candidate)) {
            candidate = base + suffix++;
        }
        return candidate;
    }

    private JSONObject metadata(final String source, final String situation) {
        return new JSONObject()
                .put("requestId", source + "-" + System.currentTimeMillis())
                .put("source", source)
                .put("situation", situation == null ? "" : situation)
                .put("createdAt", Instant.now().toString());
    }

    private String resolveRootId(final JSONObject snapshot) {
        final JSONObject flow = snapshot == null ? null : snapshot.optJSONObject("flow");
        final String rootId = flow == null ? "" : flow.optString("rootId", "").trim();
        return rootId.isEmpty() ? "SceneFlow" : rootId;
    }

    private String resolveEventVariable(final JSONObject snapshot) {
        final JSONObject flow = snapshot == null ? null : snapshot.optJSONObject("flow");
        final JSONArray variables = flow == null ? null : flow.optJSONArray("variables");
        if (variables != null) {
            for (int i = 0; i < variables.length(); i++) {
                final JSONObject v = variables.optJSONObject(i);
                if (v == null) {
                    continue;
                }
                final String type = v.optString("type", "").toLowerCase(Locale.ROOT);
                if (type.startsWith("event")) {
                    final String name = v.optString("name", "").trim();
                    if (!name.isEmpty()) {
                        return name;
                    }
                }
            }
        }
        return "UIEvent";
    }

    /** An agent, and the variable whose becoming true means it can act. */
    record ReadinessSignal(String agentName, String variable, String description, boolean meansCanAct) {
    }

    /**
     * The agents this project can wait for, and the variable that says so for each.
     *
     * <p>The chain is agent to device to plugin to the variable that plugin instance is bound to.
     * The binding is the part that matters: two characters running on one plugin class declare the
     * same key and bind it to different variables, so the declared name alone cannot tell them apart.
     *
     * <p>Where a plugin reports both connecting and being able to act, the latter wins. charamel-embed
     * sets one when the character page connects and the other only once the model is loaded and audio
     * is unlocked, and speaking in between fails silently.
     */
    List<ReadinessSignal> readinessSignals(final JSONObject snapshot) {
        final JSONObject project = snapshot == null ? null : snapshot.optJSONObject("project");
        final JSONArray plugins = project == null ? null : project.optJSONArray("plugins");
        final JSONArray agents = project == null ? null : project.optJSONArray("agents");
        if (plugins == null) {
            return List.of();
        }

        final Map<String, ReadinessSignal> byPlugin = new LinkedHashMap<>();
        for (int i = 0; i < plugins.length(); i++) {
            final JSONObject plugin = plugins.optJSONObject(i);
            if (plugin == null) {
                continue;
            }
            final JSONArray writes = plugin.optJSONArray("writesVariables");
            ReadinessSignal best = null;
            for (int j = 0; writes != null && j < writes.length(); j++) {
                final JSONObject entry = writes.optJSONObject(j);
                if (entry == null || !"Bool".equalsIgnoreCase(entry.optString("type", ""))) {
                    continue;
                }
                final String variable = entry.optString("boundTo", "").trim();
                if (variable.isEmpty()) {
                    continue;
                }
                final String name = entry.optString("name", "");
                final String description = entry.optString("description", "");
                final String haystack = (name + " " + description).toLowerCase(Locale.ROOT);
                final boolean canAct = haystack.contains("ready");
                final boolean connects = haystack.contains("connect");
                if (!canAct && !connects) {
                    continue;
                }
                if (best == null || (canAct && !best.meansCanAct())) {
                    best = new ReadinessSignal(
                            plugin.optString("name", ""), variable, description, canAct);
                }
            }
            if (best != null) {
                byPlugin.put(plugin.optString("name", ""), best);
            }
        }

        // Name each signal after the agent that speaks through it, since that is what an author
        // recognises. A plugin carrying no agent, a screen for instance, keeps its own name.
        final List<ReadinessSignal> out = new ArrayList<>();
        final Set<String> claimed = new LinkedHashSet<>();
        for (int i = 0; agents != null && i < agents.length(); i++) {
            final JSONObject agent = agents.optJSONObject(i);
            final String device = agent == null ? "" : agent.optString("device", "");
            final ReadinessSignal signal = byPlugin.get(device);
            if (signal == null || !claimed.add(device)) {
                continue;
            }
            out.add(new ReadinessSignal(agent.optString("name", device), signal.variable(),
                    signal.description(), signal.meansCanAct()));
        }
        for (Map.Entry<String, ReadinessSignal> remaining : byPlugin.entrySet()) {
            if (!claimed.contains(remaining.getKey())) {
                out.add(remaining.getValue());
            }
        }
        return out;
    }

    private ConstrainedActivitySpec constrainedActivitySpec(
            final String situation,
            final String rootId,
            final String eventVar,
            final JSONObject snapshot,
            final ConstraintResolutionMode mode) {
        final ConstraintResolution constraintResolution = resolveConstraintLabels(situation, "OkayButtonPressed", mode);
        final List<String> constraintLabels = constraintResolution.resolved();
        final String constraintLabel = constraintLabels.isEmpty() ? "UnresolvedConstraint" : constraintLabels.get(0);
        final int intervalMs = extractPolicyIntervalMs(situation);
        final PromptResolution promptResolution = resolvePromptToMetaModel(situation);
        final ActivityType activityType = inferActivityType(promptResolution.activityKind());
        final PatternSelection selection = selectPattern(
                Map.of("constrainedActivity.kind", promptResolution.activityKind()));
        final String constraintVariable = resolveConstraintVariable(eventVar, snapshot);
        return new ConstrainedActivitySpec(
                situation,
                rootId,
                constraintVariable,
                constraintLabel,
                constraintLabels,
                constraintResolution.unresolved(),
                mode,
                activityType,
                intervalMs,
                promptResolution.activityKind(),
                promptResolution.interruptibility(),
                promptResolution.confidence(),
                promptResolution.ambiguities(),
                selection.patternId(),
                selection.reason());
    }

    private JSONArray appendConstraintExitOps(
            final JSONArray operations,
            final ConstrainedActivitySpec spec,
            final String superNodeId,
            final int firstAfterNodeIndex) {
        int nodeIdx = firstAfterNodeIndex;
        int edgeIdx = 1;
        for (String label : spec.constraintLabels()) {
            final String labelSuffix = sanitizeId(label);
            final String afterNodeId = "N" + nodeIdx++;
            operations.put(new JSONObject()
                    .put("op", "create_node")
                    .put("parentSuperNodeId", spec.rootId())
                    .put("nodeId", afterNodeId)
                    .put("name", "After_" + labelSuffix));
            operations.put(new JSONObject()
                    .put("op", "create_edge")
                    .put("edgeId", "ConstraintSatisfied_" + labelSuffix + "_" + edgeIdx++)
                    .put("edgeType", "IEDGE")
                    .put("sourceNodeId", superNodeId)
                    .put("targetNodeId", afterNodeId)
                    .put("payload", new JSONObject()
                            .put("conditionText", spec.constraintVariable() + " == \"" + label + "\"")));
        }
        return operations;
    }

    private ConstraintResolution resolveConstraintLabels(
            final String text,
            final String fallback,
            final ConstraintResolutionMode mode) {
        final LinkedHashSet<String> resolved = new LinkedHashSet<>();
        final LinkedHashSet<String> unresolved = new LinkedHashSet<>();
        if (text != null) {
            final Matcher quoted = Pattern.compile("\"([^\"]+)\"").matcher(text);
            while (quoted.find()) {
                final String raw = quoted.group(1).trim();
                final String canonical = canonicalButtonLabel(raw);
                if (!canonical.isBlank()) {
                    resolved.add(canonical);
                } else if (!raw.isBlank()) {
                    unresolved.add(raw);
                }
            }
            final String lower = text.toLowerCase(Locale.ROOT);
            if (containsWord(lower, "ok") || containsWord(lower, "okay")) {
                resolved.add("OkayButtonPressed");
            }
            if (containsWord(lower, "cancel") || containsWord(lower, "canel")) {
                resolved.add("CancelButtonPressed");
            }
            if (containsWord(lower, "yes")) {
                resolved.add("YesButtonPressed");
            }
            if (containsWord(lower, "no")) {
                resolved.add("NoButtonPressed");
            }
            final Matcher buttonTokens = Pattern.compile("\\b([A-Za-z][A-Za-z0-9_]*)\\s+button\\b", Pattern.CASE_INSENSITIVE)
                    .matcher(text);
            while (buttonTokens.find()) {
                final String raw = buttonTokens.group(1).trim();
                final String canonical = canonicalButtonLabel(raw);
                if (!canonical.isBlank()) {
                    resolved.add(canonical);
                } else if (!raw.isBlank()) {
                    unresolved.add(raw);
                }
            }
        }
        if (resolved.isEmpty() && mode == ConstraintResolutionMode.PERMISSIVE) {
            resolved.add(fallback);
        }
        return new ConstraintResolution(new ArrayList<>(resolved), new ArrayList<>(unresolved));
    }

    private boolean containsWord(final String lower, final String word) {
        return Pattern.compile("\\b" + Pattern.quote(word) + "\\b").matcher(lower).find();
    }

    private String canonicalButtonLabel(final String value) {
        if (value == null) {
            return "";
        }
        final String trimmed = value.trim();
        if (trimmed.isEmpty()) {
            return "";
        }
        final String lower = trimmed.toLowerCase(Locale.ROOT);
        if (lower.equals("ok") || lower.equals("okay")
                || lower.equals("okaybutton") || lower.equals("okbutton")
                || lower.equals("okaybuttonpressed") || lower.equals("okbuttonpressed")) {
            return "OkayButtonPressed";
        }
        if (lower.equals("cancel") || lower.equals("canel")
                || lower.equals("cancelbutton") || lower.equals("canelbutton")
                || lower.equals("cancelbuttonpressed") || lower.equals("canelbuttonpressed")) {
            return "CancelButtonPressed";
        }
        if (lower.equals("yes") || lower.equals("yesbutton") || lower.equals("yesbuttonpressed")) {
            return "YesButtonPressed";
        }
        if (lower.equals("no") || lower.equals("nobutton") || lower.equals("nobuttonpressed")) {
            return "NoButtonPressed";
        }
        return "";
    }

    private ActivityType inferActivityType(final String activityKind) {
        if ("reminder".equals(activityKind)) {
            return ActivityType.REMIND;
        }
        if ("multimodal_activity".equals(activityKind) || "social_behavior".equals(activityKind)) {
            return ActivityType.GENERIC_ACTIVITY;
        }
        return ActivityType.MINIMAL_LIVENESS;
    }

    private PromptResolution resolvePromptToMetaModel(final String situation) {
        final String lower = situation == null ? "" : situation.toLowerCase(Locale.ROOT);
        final List<String> activitySignals = new ArrayList<>();
        if (lower.contains("remind") || lower.contains("reminder")) {
            activitySignals.add("reminder");
        }
        if (lower.contains("music")
                || lower.contains("picture")
                || lower.contains("image")
                || lower.contains("video")) {
            activitySignals.add("multimodal_activity");
        }
        if (lower.contains("social")
                || lower.contains("agent")
                || lower.contains("watch")
                || lower.contains("gaze")) {
            activitySignals.add("social_behavior");
        }

        final String activityKind = activitySignals.isEmpty() ? "minimal_liveness" : activitySignals.get(0);
        final List<String> ambiguities = new ArrayList<>();
        double confidence;
        if (activitySignals.isEmpty()) {
            confidence = 0.55;
            ambiguities.add("No explicit constrained activity cue detected; defaulted to minimal_liveness.");
        } else if (activitySignals.size() == 1) {
            confidence = 0.95;
        } else {
            confidence = 0.65;
            ambiguities.add("Multiple constrained activity cues detected " + activitySignals
                    + "; selected " + activityKind + " by precedence.");
        }

        final boolean attentionAwareCue = (lower.contains("busy")
                || lower.contains("appropriate moment")
                || lower.contains("do not interrupt")
                || lower.contains("don't interrupt"));
        final boolean explicitImmediateCue = lower.contains("always interrupt")
                || lower.contains("interrupt immediately");
        final String interruptibility = attentionAwareCue ? "attention_aware" : "always";
        if (attentionAwareCue && explicitImmediateCue) {
            confidence = Math.max(0.1, confidence - 0.2);
            ambiguities.add("Conflicting interruptibility cues detected; selected attention_aware.");
        }
        return new PromptResolution(activityKind, interruptibility, confidence, ambiguities);
    }

    /**
     * Chooses the catalogue pattern a candidate realises, matching declared criteria against the meta
     * the template resolved.
     *
     * <p>Matching is per criterion. A key the template did not resolve is ignored, so a pattern that
     * constrains an axis this template knows nothing about is still eligible. A key that was resolved
     * to a value the pattern does not list rejects it outright. A pattern must match at least one
     * criterion, which is what stops an unrelated pattern from matching vacuously.
     *
     * <p>Implemented patterns win over planned ones; among equals, the pattern that matched more
     * criteria wins, then catalogue order. When only planned patterns match, the catalogue's own
     * {@code fallbackTo} decides where to land, so no pattern id is hardcoded here.
     */
    // Package-private for tests: production only ever resolves one criterion today, so
    // multi-criterion matching has no other seam to exercise it through.
    PatternSelection selectPattern(final Map<String, String> resolvedMeta) {
        final List<CatalogPattern> matches = new ArrayList<>();
        final Map<String, Integer> specificity = new LinkedHashMap<>();
        for (CatalogPattern pattern : catalogPatterns) {
            int matched = 0;
            boolean rejected = false;
            for (Map.Entry<String, List<String>> criterion : pattern.criteria().entrySet()) {
                final String resolved = resolvedMeta.get(criterion.getKey());
                if (resolved == null || resolved.isBlank()) {
                    continue;
                }
                if (criterion.getValue().contains(resolved)) {
                    matched++;
                } else {
                    rejected = true;
                    break;
                }
            }
            if (!rejected && matched > 0) {
                matches.add(pattern);
                specificity.put(pattern.id(), matched);
            }
        }
        if (matches.isEmpty()) {
            return new PatternSelection("", "no catalog pattern declares criteria matching " + resolvedMeta);
        }

        CatalogPattern best = null;
        for (CatalogPattern candidate : matches) {
            if (best == null) {
                best = candidate;
                continue;
            }
            final boolean betterStatus = candidate.implemented() && !best.implemented();
            final boolean sameStatus = candidate.implemented() == best.implemented();
            if (betterStatus || (sameStatus && specificity.get(candidate.id()) > specificity.get(best.id()))) {
                best = candidate;
            }
        }

        if (best.implemented()) {
            return new PatternSelection(best.id(),
                    "selected from catalog on " + specificity.get(best.id()) + " matching criterion(s) of "
                            + resolvedMeta);
        }
        final CatalogPattern fallback = patternById(best.fallbackTo());
        if (fallback != null && fallback.implemented()) {
            return new PatternSelection(fallback.id(),
                    "closest match " + best.id() + " is planned rather than implemented, so the catalog's "
                            + "declared fallback was used");
        }
        return new PatternSelection(best.id(),
                "only a planned pattern matches and it declares no implemented fallback");
    }

    private CatalogPattern patternById(final String id) {
        if (id == null || id.isBlank()) {
            return null;
        }
        for (CatalogPattern pattern : catalogPatterns) {
            if (id.equals(pattern.id())) {
                return pattern;
            }
        }
        return null;
    }

    private int extractPolicyIntervalMs(final String situation) {
        if (situation == null || situation.isBlank()) {
            return 1000;
        }
        final String lower = situation.toLowerCase(Locale.ROOT);
        final java.util.regex.Matcher sec = java.util.regex.Pattern.compile("(\\d+)\\s*(second|seconds|sec|s)\\b").matcher(lower);
        if (sec.find()) {
            return Math.max(1, Integer.parseInt(sec.group(1))) * 1000;
        }
        final java.util.regex.Matcher ms = java.util.regex.Pattern.compile("(\\d+)\\s*(millisecond|milliseconds|ms)\\b").matcher(lower);
        if (ms.find()) {
            return Math.max(1, Integer.parseInt(ms.group(1)));
        }
        final java.util.regex.Matcher min = java.util.regex.Pattern.compile("(\\d+)\\s*(minute|minutes|min|m)\\b").matcher(lower);
        if (min.find()) {
            return Math.max(1, Integer.parseInt(min.group(1))) * 60_000;
        }
        return 1000;
    }

    private String resolveConstraintVariable(final String eventVar, final JSONObject snapshot) {
        if (eventVar != null && !eventVar.isBlank()) {
            return eventVar;
        }
        final JSONObject flow = snapshot == null ? null : snapshot.optJSONObject("flow");
        final JSONArray variables = flow == null ? null : flow.optJSONArray("variables");
        if (variables == null) {
            return "event";
        }
        for (int i = 0; i < variables.length(); i++) {
            final JSONObject variable = variables.optJSONObject(i);
            if (variable == null) {
                continue;
            }
            final String name = variable.optString("name", "").trim();
            if (name.isBlank()) {
                continue;
            }
            final String type = variable.optString("type", "").toLowerCase(Locale.ROOT);
            if (type.startsWith("event")) {
                return name;
            }
        }
        return "event";
    }

    private String sanitizeId(final String value) {
        final String id = value.replaceAll("[^A-Za-z0-9_]", "_");
        return id.isEmpty() ? "Value" : id;
    }

    private Set<String> snapshotNodeIds(final JSONObject snapshot, final String rootId) {
        final Set<String> ids = new LinkedHashSet<>();
        if (rootId != null && !rootId.isBlank()) {
            ids.add(rootId);
        }
        final JSONObject flow = snapshot == null ? null : snapshot.optJSONObject("flow");
        final org.json.JSONArray nodes = flow == null ? null : flow.optJSONArray("nodes");
        if (nodes != null) {
            for (int i = 0; i < nodes.length(); i++) {
                final JSONObject node = nodes.optJSONObject(i);
                if (node == null) {
                    continue;
                }
                final String id = node.optString("id", "").trim();
                if (!id.isEmpty()) {
                    ids.add(id);
                }
            }
        }
        return ids;
    }

    private int nextIdIndex(final Set<String> ids, final String prefix, final int fallbackStart) {
        int max = fallbackStart - 1;
        for (String id : ids) {
            if (id == null || !id.startsWith(prefix) || id.length() == prefix.length()) {
                continue;
            }
            final String numeric = id.substring(prefix.length());
            try {
                max = Math.max(max, Integer.parseInt(numeric));
            } catch (NumberFormatException ignored) {
                // ignore non-numeric ids
            }
        }
        return max + 1;
    }

    private enum ActivityType {
        MINIMAL_LIVENESS,
        REMIND,
        GENERIC_ACTIVITY
    }

    private List<CatalogPattern> loadCatalogPatterns(final Path catalogPath) {
        final JSONObject root = AuthoringResources.read(catalogPath, PATTERN_CATALOG_FILE);
        if (root == null) {
            return fallbackCatalogPatterns();
        }
        try {
            final JSONArray patterns = root.optJSONArray("patternLibrary");
            if (patterns == null) {
                return fallbackCatalogPatterns();
            }
            final List<CatalogPattern> parsed = new ArrayList<>();
            for (int i = 0; i < patterns.length(); i++) {
                final JSONObject pattern = patterns.optJSONObject(i);
                if (pattern == null) {
                    continue;
                }
                final String id = pattern.optString("id", "").trim();
                if (id.isBlank()) {
                    continue;
                }
                final boolean implemented =
                        "implemented".equalsIgnoreCase(pattern.optString("status", ""));
                final Map<String, List<String>> criteria = new LinkedHashMap<>();
                final JSONObject supportsMeta = pattern.optJSONObject("supportsMeta");
                if (supportsMeta != null) {
                    for (String key : supportsMeta.keySet()) {
                        // Only arrays are constraints. A scalar such as
                        // "parsed_from_text_or_default" documents where a value comes from and is
                        // not something to match against.
                        if (!(supportsMeta.opt(key) instanceof JSONArray values)) {
                            continue;
                        }
                        final List<String> allowed = new ArrayList<>();
                        for (int j = 0; j < values.length(); j++) {
                            final String value = values.optString(j, "").trim();
                            if (!value.isBlank()) {
                                allowed.add(value);
                            }
                        }
                        if (!allowed.isEmpty()) {
                            criteria.put(key, allowed);
                        }
                    }
                }
                parsed.add(new CatalogPattern(
                        id, implemented, criteria, pattern.optString("fallbackTo", "").trim()));
            }
            return parsed.isEmpty() ? fallbackCatalogPatterns() : parsed;
        } catch (RuntimeException malformed) {
            return fallbackCatalogPatterns();
        }
    }

    private List<CatalogPattern> fallbackCatalogPatterns() {
        return List.of(
                new CatalogPattern("periodic_reminder_while_waiting", true,
                        Map.of("constrainedActivity.kind", List.of("reminder")), ""),
                new CatalogPattern("constrained_activity_base", true,
                        Map.of("constrainedActivity.kind", List.of("minimal_liveness")), "")
        );
    }

    private record ConstrainedActivitySpec(
            String situation,
            String rootId,
            String constraintVariable,
            String constraintLabel,
            List<String> constraintLabels,
            List<String> unresolvedConstraintLabels,
            ConstraintResolutionMode constraintResolutionMode,
            ActivityType activityType,
            int policyIntervalMs,
            String activityKind,
            String interruptibility,
            double promptResolutionConfidence,
            List<String> promptResolutionAmbiguities,
            String selectedPatternId,
            String selectionReason) {
    }

    /** One step of a described sequence, with the node it becomes and the scene it plays. */
    private record SequenceStep(String text, String nodeId, String sceneName) {
    }

    private record SequenceSpec(String situation, String rootId, List<SequenceStep> steps) {
    }

    private record ConstraintResolution(List<String> resolved, List<String> unresolved) {
    }

    private record PromptResolution(
            String activityKind,
            String interruptibility,
            double confidence,
            List<String> ambiguities) {
    }

    record PatternSelection(String patternId, String reason) {
    }

    private record CatalogPattern(
            String id,
            boolean implemented,
            Map<String, List<String>> criteria,
            String fallbackTo) {
    }
}
