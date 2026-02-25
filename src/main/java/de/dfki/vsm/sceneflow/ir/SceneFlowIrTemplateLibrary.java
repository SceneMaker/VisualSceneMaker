package de.dfki.vsm.sceneflow.ir;

import org.json.JSONArray;
import org.json.JSONObject;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class SceneFlowIrTemplateLibrary {
    private static final Path DEFAULT_PATTERN_CATALOG_PATH = Path.of("doc", "interactive-design-pattern-catalog.json");
    private final List<CatalogPattern> catalogPatterns;

    public SceneFlowIrTemplateLibrary() {
        this(DEFAULT_PATTERN_CATALOG_PATH);
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
        if (looksLikeWaitForEvent(lower)) {
            final ConstrainedActivitySpec spec = constrainedActivitySpec(prompt, rootId, eventVar, snapshot, mode);
            candidates.add(constrainedActivityTemplate(spec, snapshot));
        }
        if (looksLikeTimeoutRetry(lower)) {
            candidates.add(timeoutRetryTemplate(prompt, rootId));
        }
        if (looksLikeCommandOnCondition(lower, eventVar)) {
            candidates.add(commandOnConditionTemplate(prompt, rootId, eventVar));
        }
        if (candidates.isEmpty()) {
            final String fallbackPrompt = prompt.isEmpty() ? "Wait for event" : prompt;
            final ConstrainedActivitySpec spec = constrainedActivitySpec(
                    fallbackPrompt, rootId, eventVar, snapshot, mode);
            candidates.add(constrainedActivityTemplate(spec, snapshot));
        }
        return candidates;
    }

    private boolean looksLikeWaitForEvent(final String lower) {
        return lower.contains("wait") || lower.contains("until") || lower.contains("pressed");
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
        final PatternSelection selection = selectPattern(promptResolution.activityKind());
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
        final String activityKind;
        if (lower.contains("remind") || lower.contains("reminder")) {
            activityKind = "reminder";
        } else if (lower.contains("music")
                || lower.contains("picture")
                || lower.contains("image")
                || lower.contains("video")) {
            activityKind = "multimodal_activity";
        } else if (lower.contains("social")
                || lower.contains("agent")
                || lower.contains("watch")
                || lower.contains("gaze")) {
            activityKind = "social_behavior";
        } else {
            activityKind = "minimal_liveness";
        }
        final String interruptibility = (lower.contains("busy")
                || lower.contains("appropriate moment")
                || lower.contains("do not interrupt")
                || lower.contains("don't interrupt"))
                ? "attention_aware"
                : "always";
        return new PromptResolution(activityKind, interruptibility);
    }

    private PatternSelection selectPattern(final String activityKind) {
        for (CatalogPattern pattern : catalogPatterns) {
            if (!pattern.implemented()) {
                continue;
            }
            if (pattern.supportedKinds().contains(activityKind)) {
                return new PatternSelection(
                        pattern.id(),
                        "selected from catalog via constrainedActivity.kind=" + activityKind);
            }
        }
        for (CatalogPattern pattern : catalogPatterns) {
            if (pattern.implemented() && "constrained_activity_base".equals(pattern.id())) {
                return new PatternSelection(
                        pattern.id(),
                        "fallback to base catalog pattern; no implemented match for constrainedActivity.kind=" + activityKind);
            }
        }
        return new PatternSelection("constrained_activity_base", "hardcoded fallback; catalog base pattern unavailable");
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
        if (catalogPath != null) {
            try {
                if (Files.exists(catalogPath)) {
                    final JSONObject root = new JSONObject(Files.readString(catalogPath));
                    final JSONArray patterns = root.optJSONArray("patternLibrary");
                    if (patterns != null) {
                        final List<CatalogPattern> parsed = new ArrayList<>();
                        for (int i = 0; i < patterns.length(); i++) {
                            final JSONObject p = patterns.optJSONObject(i);
                            if (p == null) {
                                continue;
                            }
                            final String id = p.optString("id", "").trim();
                            if (id.isBlank()) {
                                continue;
                            }
                            final boolean implemented = "implemented".equalsIgnoreCase(p.optString("status", ""));
                            final JSONObject supportsMeta = p.optJSONObject("supportsMeta");
                            final List<String> kinds = new ArrayList<>();
                            if (supportsMeta != null) {
                                final Object rawKinds = supportsMeta.opt("constrainedActivity.kind");
                                if (rawKinds instanceof JSONArray arr) {
                                    for (int j = 0; j < arr.length(); j++) {
                                        final String kind = arr.optString(j, "").trim();
                                        if (!kind.isBlank()) {
                                            kinds.add(kind);
                                        }
                                    }
                                } else if (rawKinds instanceof String str && !str.isBlank()) {
                                    kinds.add(str.trim());
                                }
                            }
                            parsed.add(new CatalogPattern(id, implemented, kinds));
                        }
                        if (!parsed.isEmpty()) {
                            return parsed;
                        }
                    }
                }
            } catch (IOException | RuntimeException ignored) {
                // fall through to static fallback patterns
            }
        }
        return fallbackCatalogPatterns();
    }

    private List<CatalogPattern> fallbackCatalogPatterns() {
        return List.of(
                new CatalogPattern("periodic_reminder_while_waiting", true, List.of("reminder")),
                new CatalogPattern("constrained_activity_base", true, List.of("minimal_liveness"))
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
            String selectedPatternId,
            String selectionReason) {
    }

    private record ConstraintResolution(List<String> resolved, List<String> unresolved) {
    }

    private record PromptResolution(String activityKind, String interruptibility) {
    }

    private record PatternSelection(String patternId, String reason) {
    }

    private record CatalogPattern(String id, boolean implemented, List<String> supportedKinds) {
    }
}
