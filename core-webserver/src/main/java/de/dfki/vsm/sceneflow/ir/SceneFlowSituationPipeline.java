package de.dfki.vsm.sceneflow.ir;

import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.util.xml.XMLUtilities;
import org.json.JSONArray;
import org.json.JSONObject;
import org.json.JSONTokener;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class SceneFlowSituationPipeline {
    private static final String INTERACTIVE_PATTERN_CATALOG_FILE = "interactive-design-pattern-catalog.json";
    private static final Pattern IDENTIFIER_PATTERN = Pattern.compile("\\b[A-Za-z_][A-Za-z0-9_]*\\b");
    private static final Set<String> RESERVED_TOKENS = Set.of(
            "true", "false", "null", "and", "or", "not", "in", "if", "then", "else");

    public enum CandidateMode {
        TEMPLATE,
        LLM,
        HYBRID,
        /**
         * Templates first, and a language model only for situations no template recognises.
         *
         * <p>Template output is validated, reproducible and explains itself in the author's words,
         * so it is not something a model should be given the chance to replace. What a model is good
         * for here is the tail: the situations that would otherwise come back as "no pattern
         * recognises this".
         */
        TEMPLATE_THEN_LLM;

        public static CandidateMode from(final String value) {
            if (value == null || value.isBlank()) {
                return TEMPLATE;
            }
            switch (value.trim().toLowerCase(Locale.ROOT)) {
                case "template":
                    return TEMPLATE;
                case "llm":
                    return LLM;
                case "hybrid":
                    return HYBRID;
                case "template-then-llm":
                case "template_then_llm":
                    return TEMPLATE_THEN_LLM;
                default:
                    return TEMPLATE;
            }
        }
    }

    public static final class Settings {
        private final CandidateMode mode;
        private final OutputMode outputMode;
        private final SceneFlowIrLlmCandidateProvider.Config llm;
        private final ConstraintResolutionMode constraintResolutionMode;
        private final boolean readinessGate;

        public Settings(
                final CandidateMode mode,
                final OutputMode outputMode,
                final SceneFlowIrLlmCandidateProvider.Config llm) {
            this(mode, outputMode, llm, ConstraintResolutionMode.PERMISSIVE);
        }

        public Settings(
                final CandidateMode mode,
                final OutputMode outputMode,
                final SceneFlowIrLlmCandidateProvider.Config llm,
                final ConstraintResolutionMode constraintResolutionMode) {
            this(mode, outputMode, llm, constraintResolutionMode, true);
        }

        /**
         * @param readinessGate whether a flow that starts by using an agent gets a wait for that
         *                      agent put in front of it, when the project has something that reports
         *                      readiness and the flow does not already wait. Off is how an author
         *                      says they have handled it elsewhere.
         */
        public Settings(
                final CandidateMode mode,
                final OutputMode outputMode,
                final SceneFlowIrLlmCandidateProvider.Config llm,
                final ConstraintResolutionMode constraintResolutionMode,
                final boolean readinessGate) {
            this.mode = mode == null ? CandidateMode.TEMPLATE : mode;
            this.outputMode = outputMode == null ? OutputMode.PATCH : outputMode;
            this.llm = llm;
            this.constraintResolutionMode = constraintResolutionMode == null
                    ? ConstraintResolutionMode.PERMISSIVE
                    : constraintResolutionMode;
            this.readinessGate = readinessGate;
        }

        public boolean readinessGate() {
            return readinessGate;
        }

        public CandidateMode mode() {
            return mode;
        }

        public OutputMode outputMode() {
            return outputMode;
        }

        public SceneFlowIrLlmCandidateProvider.Config llm() {
            return llm;
        }

        public ConstraintResolutionMode constraintResolutionMode() {
            return constraintResolutionMode;
        }
    }

    public enum OutputMode {
        PATCH,
        STANDALONE;

        public static OutputMode from(final String value) {
            if (value == null || value.isBlank()) {
                return PATCH;
            }
            switch (value.trim().toLowerCase(Locale.ROOT)) {
                case "standalone":
                    return STANDALONE;
                case "patch":
                default:
                    return PATCH;
            }
        }
    }

    public JSONObject run(
            final Path snapshotPath,
            final Path baseSceneFlowPath,
            final Path outputPath,
            final Path reportPath,
            final String situation) throws SceneFlowIrCompileException {
        return run(snapshotPath, baseSceneFlowPath, outputPath, reportPath, situation,
                new Settings(CandidateMode.TEMPLATE, OutputMode.PATCH, null),
                defaultGeneratedProjectDir(outputPath));
    }

    public JSONObject run(
            final Path snapshotPath,
            final Path baseSceneFlowPath,
            final Path outputPath,
            final Path reportPath,
            final String situation,
            final Settings settings) throws SceneFlowIrCompileException {
        return run(snapshotPath, baseSceneFlowPath, outputPath, reportPath, situation, settings,
                defaultGeneratedProjectDir(outputPath));
    }

    public JSONObject run(
            final Path snapshotPath,
            final Path baseSceneFlowPath,
            final Path outputPath,
            final Path reportPath,
            final String situation,
            final Settings settings,
            final Path generatedProjectDir) throws SceneFlowIrCompileException {
        final JSONObject snapshot = readJson(snapshotPath);
        final Settings effectiveSettings = settings == null
                ? new Settings(CandidateMode.TEMPLATE, OutputMode.PATCH, null)
                : settings;
        final SceneFlow baseFlow = loadBaseFlow(baseSceneFlowPath, snapshot, effectiveSettings.outputMode());
        final List<String> generationWarnings = new ArrayList<>();
        final List<JSONObject> candidates = generateCandidates(
                situation, snapshot, effectiveSettings, generationWarnings);
        final Map<String, JSONObject> patternCatalog = loadPatternCatalogById();
        // STANDALONE patches apply against createStandaloneBase()'s fresh, variable-less SceneFlow,
        // not the donor snapshot's own flow — validating a STANDALONE candidate against the donor's
        // variable list makes an "auto-created if missing" variable (ensureConditionVariablesDefined
        // already skips the donor's vars for this same reason, above) look like a duplicate of a
        // variable that, in the flow actually being built, was never declared.
        final JSONObject validationSnapshot = snapshotForValidation(snapshot, effectiveSettings.outputMode());

        final SceneFlowIrSemanticValidator semanticValidator = new SceneFlowIrSemanticValidator();
        final SceneFlowIrCompiler compiler = new SceneFlowIrCompiler();

        final JSONArray attempts = new JSONArray();
        final JSONObject activeRulesSummary = new JSONObject();
        final JSONObject ruleExecutionSummary = new JSONObject();
        JSONObject chosen = null;
        int successAttempt = -1;
        Path generatedProjectPath = null;
        String generatedProjectError = null;

        for (int i = 0; i < candidates.size(); i++) {
            JSONObject candidate = enforceWaitLoopCanonicalShape(
                    new JSONObject(candidates.get(i).toString()), snapshot, situation,
                    effectiveSettings.outputMode());
            if (effectiveSettings.readinessGate()) {
                candidate = prependReadinessGate(candidate, snapshot, situation);
            }
            // Last of the shape passes, so it sees every condition the others added. The readiness
            // gate reads a variable no template mentions, and a condition on a variable nothing
            // declares compiles into a flow that cannot run.
            candidate = ensureConditionVariablesDefined(
                    candidate, snapshot, effectiveSettings.outputMode());
            candidate = assignCreateNodePositions(candidate, snapshot);
            final JSONObject attempt = new JSONObject();
            final String source = candidate.optJSONObject("metadata") != null
                    ? candidate.optJSONObject("metadata").optString("source", "unknown")
                    : "unknown";
            attempt.put("attempt", i + 1);
            attempt.put("templateSource", source);
            attempt.put("candidateSummary", summarizeCandidate(candidate));
            attempt.put("candidate", new JSONObject(candidate.toString()));
            attempt.put("interactiveDesignPattern", summarizePatternSelection(candidate, patternCatalog));
            final JSONObject promptResolution = extractPromptResolution(candidate);
            attempt.put("promptResolution", promptResolution);
            if (promptResolution.optDouble("confidence", 1.0d) < 0.8d) {
                generationWarnings.add("Attempt " + (i + 1)
                        + " has low prompt-resolution confidence ("
                        + promptResolution.optDouble("confidence", 1.0d) + ").");
            }
            final JSONArray activeSemanticRules = semanticValidator.describeActiveRules(candidate, validationSnapshot);
            attempt.put("activeSemanticRules", activeSemanticRules);
            accumulateActiveRuleSummary(activeRulesSummary, activeSemanticRules, i + 1);
            final JSONObject constraintResolution = extractConstraintResolution(candidate, effectiveSettings.constraintResolutionMode());
            attempt.put("constraintResolution", constraintResolution);

            if ("strict".equalsIgnoreCase(constraintResolution.optString("mode", ""))
                    && constraintResolution.optJSONArray("unresolvedLabels") != null
                    && constraintResolution.optJSONArray("unresolvedLabels").length() > 0) {
                final JSONArray unresolved = constraintResolution.getJSONArray("unresolvedLabels");
                final JSONArray issues = new JSONArray();
                for (int u = 0; u < unresolved.length(); u++) {
                    final String label = unresolved.optString(u, "").trim();
                    if (label.isEmpty()) {
                        continue;
                    }
                    issues.put(new JSONObject()
                            .put("code", "UNRESOLVED_CONSTRAINT_LABEL")
                            .put("path", "/metadata/constraintResolution/unresolvedLabels/" + u)
                            .put("message", "Unresolved constraint label: " + label)
                            .put("severity", "error"));
                }
                attempt.put("semanticIssues", issues);
                attempt.put("semanticErrorCount", issues.length());
                attempt.put("semanticWarningCount", 0);
                attempt.put("semanticRuleExecution", new JSONArray());
                attempt.put("status", "semantic_rejected");
                attempts.put(attempt);
                continue;
            }

            final SemanticValidationResult semantic = semanticValidator.validate(candidate, validationSnapshot);
            final JSONArray semanticRuleExecution = semanticValidator.describeRuleExecution(candidate, validationSnapshot, semantic);
            attempt.put("semanticRuleExecution", semanticRuleExecution);
            accumulateRuleExecutionSummary(ruleExecutionSummary, semanticRuleExecution);
            final JSONArray issues = new JSONArray();
            int semanticErrorCount = 0;
            int semanticWarningCount = 0;
            for (SemanticIssue issue : semantic.getIssues()) {
                final String severity = issue.getSeverity() == null ? "error" : issue.getSeverity();
                if ("warning".equalsIgnoreCase(severity)) {
                    semanticWarningCount++;
                } else {
                    semanticErrorCount++;
                }
                issues.put(new JSONObject()
                        .put("code", issue.getCode())
                        .put("path", issue.getPath())
                        .put("message", issue.getMessage())
                        .put("severity", severity));
            }
            attempt.put("semanticIssues", issues);
            attempt.put("semanticErrorCount", semanticErrorCount);
            attempt.put("semanticWarningCount", semanticWarningCount);
            if (semantic.hasErrors()) {
                attempt.put("status", "semantic_rejected");
                attempts.put(attempt);
                continue;
            }

            try {
                final SceneFlow compiled = compiler.compilePatch(candidate, baseFlow);
                if (!XMLUtilities.writeToXMLFile(compiled, outputPath.toFile(), "UTF-8")) {
                    throw new SceneFlowIrCompileException("Cannot write compiled SceneFlow XML to " + outputPath);
                }
                attempt.put("status", "accepted");
                attempts.put(attempt);
                chosen = candidate;
                successAttempt = i + 1;
                try {
                    generatedProjectPath = createGeneratedProject(snapshot, outputPath, generatedProjectDir);
                } catch (SceneFlowIrCompileException exc) {
                    generatedProjectError = exc.getMessage();
                }
                break;
            } catch (SceneFlowIrCompileException exc) {
                attempt.put("status", "compile_rejected");
                attempt.put("compileError", exc.getMessage());
                attempts.put(attempt);
            }
        }

        final JSONObject report = new JSONObject()
                .put("pipelineVersion", "1.0")
                .put("generatedAt", Instant.now().toString())
                .put("situation", situation == null ? "" : situation)
                .put("snapshotPath", snapshotPath.toAbsolutePath().toString())
                .put("sceneFlowPath", baseSceneFlowPath.toAbsolutePath().toString())
                .put("outputPath", outputPath.toAbsolutePath().toString())
                .put("candidateMode", effectiveSettings.mode().name().toLowerCase(Locale.ROOT))
                .put("outputMode", effectiveSettings.outputMode().name().toLowerCase(Locale.ROOT))
                .put("availableGraphConfig", summarizeSnapshotGraph(snapshot))
                .put("interactivePatternCatalog", INTERACTIVE_PATTERN_CATALOG_FILE)
                .put("activeSemanticRulesSummary", activeRulesSummary)
                .put("semanticRuleExecutionSummary", ruleExecutionSummary)
                .put("executedRuleCount", ruleExecutionSummary.optInt("executedRuleCount", 0))
                .put("violatedRuleCount", ruleExecutionSummary.optInt("violatedRuleCount", 0))
                .put("generatedProjectPath", generatedProjectPath == null
                        ? JSONObject.NULL
                        : generatedProjectPath.toAbsolutePath().toString())
                .put("generatedProjectError", generatedProjectError == null
                        ? JSONObject.NULL
                        : generatedProjectError)
                .put("attemptCount", attempts.length())
                .put("attempts", attempts)
                .put("generationWarnings", new JSONArray(generationWarnings));

        if (chosen != null) {
            final JSONObject metadata = chosen.optJSONObject("metadata");
            report.put("status", "success");
            report.put("successAttempt", successAttempt);
            report.put("chosenTemplate", metadata == null ? "unknown" : metadata.optString("source", "unknown"));
            report.put("assumptions", chosen.optJSONArray("assumptions") == null
                    ? new JSONArray()
                    : chosen.optJSONArray("assumptions"));
        } else if (candidates.isEmpty()) {
            // Distinct from "failed": nothing was even attempted, because no template recognises
            // this situation. Reporting it as a failure would suggest the request was understood and
            // could not be built, which is a different and more misleading thing to tell an author.
            report.put("status", "no_pattern_matched");
            report.put("successAttempt", JSONObject.NULL);
            report.put("chosenTemplate", JSONObject.NULL);
            report.put("assumptions", new JSONArray());
            report.put("noMatch", new JSONObject()
                    .put("reason", "No pattern recognises this situation, so nothing was generated.")
                    .put("recognisedSituations",
                            new JSONArray(SceneFlowIrTemplateLibrary.recognisedSituationHints())));
        } else {
            report.put("status", "failed");
            report.put("successAttempt", JSONObject.NULL);
            report.put("chosenTemplate", JSONObject.NULL);
            report.put("assumptions", new JSONArray());
        }

        writeJson(reportPath, report);
        return report;
    }

    private List<JSONObject> generateCandidates(
            final String situation,
            final JSONObject snapshot,
            final Settings settings,
            final List<String> warnings) throws SceneFlowIrCompileException {
        final SceneFlowIrTemplateLibrary templateLibrary = new SceneFlowIrTemplateLibrary();
        final SceneFlowIrLlmCandidateProvider llmProvider = new SceneFlowIrLlmCandidateProvider();

        switch (settings.mode()) {
            case TEMPLATE:
                return templateLibrary.generateCandidates(situation, snapshot, settings.constraintResolutionMode());
            case TEMPLATE_THEN_LLM:
                final List<JSONObject> fromTemplates = templateLibrary.generateCandidates(
                        situation, snapshot, settings.constraintResolutionMode());
                if (!fromTemplates.isEmpty()) {
                    return fromTemplates;
                }
                try {
                    return llmProvider.generateCandidates(
                            situation, snapshot, settings.llm(), settings.outputMode());
                } catch (SceneFlowIrCompileException exc) {
                    warnings.add("No pattern recognises this, and the language model could not be "
                            + "reached either: " + exc.getMessage());
                    return List.of();
                }
            case LLM:
                return llmProvider.generateCandidates(situation, snapshot, settings.llm(), settings.outputMode());
            case HYBRID:
                final List<JSONObject> merged = new ArrayList<>();
                try {
                    merged.addAll(llmProvider.generateCandidates(
                            situation, snapshot, settings.llm(), settings.outputMode()));
                } catch (SceneFlowIrCompileException exc) {
                    warnings.add("LLM generation unavailable, falling back to template candidates: " + exc.getMessage());
                }
                merged.addAll(templateLibrary.generateCandidates(
                        situation, snapshot, settings.constraintResolutionMode()));
                return merged;
            default:
                return templateLibrary.generateCandidates(situation, snapshot, settings.constraintResolutionMode());
        }
    }

    private JSONObject extractConstraintResolution(
            final JSONObject candidate,
            final ConstraintResolutionMode fallbackMode) {
        final JSONObject metadata = candidate == null ? null : candidate.optJSONObject("metadata");
        final JSONObject fromMetadata = metadata == null ? null : metadata.optJSONObject("constraintResolution");
        if (fromMetadata != null) {
            return new JSONObject(fromMetadata.toString());
        }
        return new JSONObject()
                .put("mode", (fallbackMode == null ? ConstraintResolutionMode.PERMISSIVE : fallbackMode)
                        .name().toLowerCase(Locale.ROOT))
                .put("resolvedLabels", new JSONArray())
                .put("unresolvedLabels", new JSONArray());
    }

    private JSONObject extractPromptResolution(final JSONObject candidate) {
        final JSONObject metadata = candidate == null ? null : candidate.optJSONObject("metadata");
        final JSONObject fromMetadata = metadata == null ? null : metadata.optJSONObject("promptResolution");
        if (fromMetadata != null) {
            return new JSONObject(fromMetadata.toString());
        }
        return new JSONObject()
                .put("activityKind", "unknown")
                .put("interruptibility", "unknown")
                .put("confidence", 1.0d)
                .put("ambiguities", new JSONArray());
    }

    private JSONObject readJson(final Path path) throws SceneFlowIrCompileException {
        try (var reader = Files.newBufferedReader(path)) {
            return new JSONObject(new JSONTokener(reader));
        } catch (IOException exc) {
            throw new SceneFlowIrCompileException("Cannot read JSON file: " + path, exc);
        }
    }

    private void writeJson(final Path path, final JSONObject json) throws SceneFlowIrCompileException {
        try {
            if (path.getParent() != null) {
                Files.createDirectories(path.getParent());
            }
            Files.writeString(path, json.toString(2) + System.lineSeparator());
        } catch (IOException exc) {
            throw new SceneFlowIrCompileException("Cannot write report JSON file: " + path, exc);
        }
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

    private SceneFlow loadBaseFlow(
            final Path sceneFlowXmlPath,
            final JSONObject snapshot,
            final OutputMode outputMode) throws SceneFlowIrCompileException {
        if (outputMode == OutputMode.PATCH) {
            return loadSceneFlow(sceneFlowXmlPath);
        }
        return createStandaloneBase(snapshot);
    }

    // See the comment where this is called: STANDALONE candidates are validated against an empty
    // variable list because that's what createStandaloneBase() actually builds against, regardless
    // of what the donor snapshot happens to declare.
    private JSONObject snapshotForValidation(final JSONObject snapshot, final OutputMode outputMode) {
        if (outputMode != OutputMode.STANDALONE || snapshot == null) {
            return snapshot;
        }
        final JSONObject copy = new JSONObject(snapshot.toString());
        final JSONObject flow = copy.optJSONObject("flow");
        if (flow != null) {
            flow.put("variables", new JSONArray());
        }
        return copy;
    }

    private SceneFlow createStandaloneBase(final JSONObject snapshot) {
        final JSONObject flow = snapshot == null ? null : snapshot.optJSONObject("flow");
        final String rootId = flow == null ? "" : flow.optString("rootId", "").trim();
        final String effectiveRootId = rootId.isEmpty() ? "SceneFlow" : rootId;
        final SceneFlow sceneFlow = new SceneFlow();
        sceneFlow.setId(effectiveRootId);
        sceneFlow.setName(effectiveRootId);
        return sceneFlow;
    }

    private JSONObject summarizeCandidate(final JSONObject candidate) {
        final JSONObject summary = new JSONObject();
        final JSONArray nodeOps = new JSONArray();
        final JSONArray edgeOps = new JSONArray();
        JSONArray operations = candidate.optJSONArray("operations");
        if (operations != null) {
            for (int i = 0; i < operations.length(); i++) {
                final JSONObject op = operations.optJSONObject(i);
                if (op == null) {
                    continue;
                }
                final String opName = op.optString("op", "");
                final JSONObject opSummary = new JSONObject().put("op", opName);
                copyIfPresent(op, opSummary, Set.of(
                        "nodeId", "superNodeId", "parentSuperNodeId",
                        "edgeId", "edgeType", "sourceNodeId", "targetNodeId",
                        "ownerNodeId", "index", "commandText"));
                if (op.has("payload") && op.optJSONObject("payload") != null) {
                    final JSONObject payloadSummary = new JSONObject();
                    copyIfPresent(op.getJSONObject("payload"), payloadSummary, Set.of(
                            "conditionText", "timeoutMs", "probability"));
                    if (!payloadSummary.isEmpty()) {
                        opSummary.put("payload", payloadSummary);
                    }
                }
                if (opName.contains("edge")) {
                    edgeOps.put(opSummary);
                } else {
                    nodeOps.put(opSummary);
                }
            }
        }
        summary.put("nodeOps", nodeOps);
        summary.put("edgeOps", edgeOps);
        return summary;
    }

    private JSONObject summarizeSnapshotGraph(final JSONObject snapshot) {
        final JSONObject out = new JSONObject();
        final JSONObject flow = snapshot == null ? null : snapshot.optJSONObject("flow");
        if (flow == null) {
            return out;
        }
        out.put("rootId", flow.optString("rootId", ""));
        out.put("startNodeIds", flow.optJSONArray("startNodeIds") == null ? new JSONArray() : flow.optJSONArray("startNodeIds"));
        out.put("allowedEdgeTypes",
                flow.optJSONArray("allowedEdgeTypes") == null ? new JSONArray() : flow.optJSONArray("allowedEdgeTypes"));

        final JSONArray nodesSummary = new JSONArray();
        final JSONArray nodes = flow.optJSONArray("nodes");
        if (nodes != null) {
            for (int i = 0; i < nodes.length(); i++) {
                final JSONObject node = nodes.optJSONObject(i);
                if (node == null) {
                    continue;
                }
                final JSONObject item = new JSONObject();
                copyIfPresent(node, item, Set.of("id", "name", "parentSuperNodeId", "isSuperNode", "startNodeIds"));
                nodesSummary.put(item);
            }
        }
        out.put("nodes", nodesSummary);

        final JSONArray edgesSummary = new JSONArray();
        final JSONArray edges = flow.optJSONArray("edges");
        if (edges != null) {
            for (int i = 0; i < edges.length(); i++) {
                final JSONObject edge = edges.optJSONObject(i);
                if (edge == null) {
                    continue;
                }
                final JSONObject item = new JSONObject();
                item.put("index", i);
                item.put("syntheticEdgeRef", "edge@" + i);
                copyIfPresent(edge, item, Set.of(
                        "id", "type", "sourceNodeId", "targetNodeId",
                        "conditionText", "timeoutMs", "probability"));
                edgesSummary.put(item);
            }
        }
        out.put("edges", edgesSummary);
        return out;
    }

    private void copyIfPresent(final JSONObject from, final JSONObject to, final Set<String> keys) {
        for (String key : keys) {
            if (from.has(key)) {
                to.put(key, from.opt(key));
            }
        }
    }

    private void accumulateActiveRuleSummary(
            final JSONObject summary,
            final JSONArray activeSemanticRules,
            final int attemptNumber) {
        if (activeSemanticRules == null) {
            return;
        }
        for (int i = 0; i < activeSemanticRules.length(); i++) {
            final JSONObject rule = activeSemanticRules.optJSONObject(i);
            if (rule == null || !rule.optBoolean("active", false)) {
                continue;
            }
            final String id = rule.optString("id", "").trim();
            if (id.isEmpty()) {
                continue;
            }
            final JSONObject entry = summary.optJSONObject(id) != null
                    ? summary.getJSONObject(id)
                    : new JSONObject()
                    .put("scope", rule.optString("scope", ""))
                    .put("severity", rule.optString("severity", "error"))
                    .put("enabled", rule.optBoolean("enabled", true))
                    .put("activeCount", 0)
                    .put("attempts", new JSONArray())
                    .put("activationReasons", new JSONArray());
            entry.put("activeCount", entry.optInt("activeCount", 0) + 1);
            entry.getJSONArray("attempts").put(attemptNumber);
            final String reason = rule.optString("activationReason", "").trim();
            if (!reason.isEmpty()) {
                entry.getJSONArray("activationReasons").put(reason);
            }
            summary.put(id, entry);
        }
    }

    private void accumulateRuleExecutionSummary(
            final JSONObject summary,
            final JSONArray semanticRuleExecution) {
        if (semanticRuleExecution == null) {
            return;
        }
        int executed = summary.optInt("executedRuleCount", 0);
        int violated = summary.optInt("violatedRuleCount", 0);
        final JSONArray perRule = summary.optJSONArray("perRule") != null
                ? summary.getJSONArray("perRule")
                : new JSONArray();
        for (int i = 0; i < semanticRuleExecution.length(); i++) {
            final JSONObject item = semanticRuleExecution.optJSONObject(i);
            if (item == null) {
                continue;
            }
            if (item.optBoolean("executed", false)) {
                executed += 1;
            }
            if (item.optInt("violatedCount", 0) > 0) {
                violated += 1;
            }
            perRule.put(new JSONObject()
                    .put("id", item.optString("id", ""))
                    .put("severity", item.optString("severity", "error"))
                    .put("enabled", item.optBoolean("enabled", true))
                    .put("executed", item.optBoolean("executed", false))
                    .put("violatedCount", item.optInt("violatedCount", 0)));
        }
        summary.put("executedRuleCount", executed);
        summary.put("violatedRuleCount", violated);
        summary.put("perRule", perRule);
    }

    private Map<String, JSONObject> loadPatternCatalogById() {
        final Map<String, JSONObject> byId = new HashMap<>();
        try {
            final JSONObject catalog = AuthoringResources.read(null, INTERACTIVE_PATTERN_CATALOG_FILE);
            if (catalog == null) {
                return byId;
            }
            final JSONArray patterns = catalog.optJSONArray("patternLibrary");
            if (patterns == null) {
                return byId;
            }
            for (int i = 0; i < patterns.length(); i++) {
                final JSONObject pattern = patterns.optJSONObject(i);
                if (pattern == null) {
                    continue;
                }
                final String id = pattern.optString("id", "").trim();
                if (!id.isEmpty()) {
                    byId.put(id, pattern);
                }
            }
        } catch (RuntimeException ignored) {
            return byId;
        }
        return byId;
    }

    private JSONObject summarizePatternSelection(
            final JSONObject candidate,
            final Map<String, JSONObject> patternCatalog) {
        final JSONObject metadata = candidate == null ? null : candidate.optJSONObject("metadata");
        final JSONObject idp = metadata == null ? null : metadata.optJSONObject("interactiveDesignPattern");
        if (idp == null) {
            return new JSONObject()
                    .put("available", false)
                    .put("catalogEntry", JSONObject.NULL)
                    .put("scientificSources", new JSONArray());
        }
        final JSONObject out = new JSONObject().put("available", true);
        final String selectedId = idp.optString("selectedPatternId", "").trim();
        if (!selectedId.isEmpty()) {
            out.put("selectedPatternId", selectedId);
        }
        if (!idp.optString("selectionReason", "").isBlank()) {
            out.put("selectionReason", idp.optString("selectionReason"));
        }
        if (idp.optJSONObject("resolvedMeta") != null) {
            out.put("resolvedMeta", new JSONObject(idp.optJSONObject("resolvedMeta").toString()));
        }
        if (!selectedId.isEmpty() && patternCatalog.containsKey(selectedId)) {
            out.put("catalogEntry", new JSONObject(patternCatalog.get(selectedId).toString()));
            final JSONArray sources = patternCatalog.get(selectedId).optJSONArray("scientificSources");
            out.put("scientificSources", sources == null ? new JSONArray() : new JSONArray(sources.toString()));
        } else {
            out.put("catalogEntry", JSONObject.NULL);
            out.put("scientificSources", new JSONArray());
        }
        return out;
    }

    private JSONObject ensureConditionVariablesDefined(
            final JSONObject candidate,
            final JSONObject snapshot,
            final OutputMode outputMode) {
        JSONArray operations = candidate.optJSONArray("operations");
        if (operations == null || operations.length() == 0) {
            return candidate;
        }

        final String rootId = resolveRootId(snapshot);
        final Set<String> knownVariables = new LinkedHashSet<>();
        if (outputMode == OutputMode.PATCH) {
            knownVariables.addAll(snapshotVariableNames(snapshot));
        }
        knownVariables.addAll(operationDefinedVariables(operations));

        final Set<String> neededVariables = new LinkedHashSet<>();
        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null) {
                continue;
            }
            final String opName = op.optString("op", "");
            if (!"create_edge".equals(opName) && !"update_edge".equals(opName)) {
                continue;
            }
            final String edgeType = op.optString("edgeType", "");
            if (!"CEDGE".equals(edgeType) && !"IEDGE".equals(edgeType)) {
                continue;
            }
            final JSONObject payload = op.optJSONObject("payload");
            final String conditionText = payload == null ? "" : payload.optString("conditionText", "");
            for (String variable : extractConditionVariables(conditionText)) {
                if (!knownVariables.contains(variable)) {
                    neededVariables.add(variable);
                    knownVariables.add(variable);
                }
            }
        }

        if (neededVariables.isEmpty()) {
            return candidate;
        }

        final JSONArray prefixOps = new JSONArray();
        for (String variable : neededVariables) {
            final String type = declaredVariableType(snapshot, variable);
            final JSONObject varDef = new JSONObject()
                    .put("name", variable)
                    .put("type", type);
            if ("Event".equals(type)) {
                varDef.put("eventElementType", "*");
                varDef.put("eventCapacity", 10);
            } else {
                // A variable a condition reads is read before anything writes it, so it needs a
                // starting value. The one that means "nothing yet" for its type.
                varDef.put("expression", switch (type) {
                    case "Bool" -> "false";
                    case "Int" -> "0";
                    case "Float" -> "0.0";
                    default -> "\"\"";
                });
            }
            prefixOps.put(new JSONObject()
                    .put("op", "add_variable_definition")
                    .put("ownerNodeId", rootId)
                    .put("varDef", varDef));
        }

        final JSONArray merged = new JSONArray();
        for (int i = 0; i < prefixOps.length(); i++) {
            merged.put(prefixOps.get(i));
        }
        for (int i = 0; i < operations.length(); i++) {
            merged.put(operations.get(i));
        }
        candidate.put("operations", merged);

        JSONArray assumptions = candidate.optJSONArray("assumptions");
        if (assumptions == null) {
            assumptions = new JSONArray();
            candidate.put("assumptions", assumptions);
        }
        for (String variable : neededVariables) {
            assumptions.put("Auto-created variable " + variable + " for edge conditions.");
        }
        return candidate;
    }

    private String resolveRootId(final JSONObject snapshot) {
        final JSONObject flow = snapshot == null ? null : snapshot.optJSONObject("flow");
        final String rootId = flow == null ? "" : flow.optString("rootId", "").trim();
        return rootId.isEmpty() ? "SceneFlow" : rootId;
    }

    private Set<String> snapshotVariableNames(final JSONObject snapshot) {
        final Set<String> vars = new LinkedHashSet<>();
        final JSONObject flow = snapshot == null ? null : snapshot.optJSONObject("flow");
        final JSONArray variables = flow == null ? null : flow.optJSONArray("variables");
        if (variables == null) {
            return vars;
        }
        for (int i = 0; i < variables.length(); i++) {
            final JSONObject var = variables.optJSONObject(i);
            if (var == null) {
                continue;
            }
            final String name = var.optString("name", "").trim();
            if (!name.isEmpty()) {
                vars.add(name);
            }
        }
        return vars;
    }

    private Set<String> operationDefinedVariables(final JSONArray operations) {
        final Set<String> vars = new LinkedHashSet<>();
        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null || !"add_variable_definition".equals(op.optString("op", ""))) {
                continue;
            }
            final JSONObject varDef = op.optJSONObject("varDef");
            final String name = varDef == null ? "" : varDef.optString("name", "").trim();
            if (!name.isEmpty()) {
                vars.add(name);
            }
        }
        return vars;
    }

    private Set<String> extractConditionVariables(final String conditionText) {
        final Set<String> vars = new LinkedHashSet<>();
        final String sanitized = stripQuotedContent(conditionText == null ? "" : conditionText);
        final Matcher matcher = IDENTIFIER_PATTERN.matcher(sanitized);
        while (matcher.find()) {
            final String token = matcher.group();
            if (RESERVED_TOKENS.contains(token.toLowerCase(Locale.ROOT))) {
                continue;
            }
            vars.add(token);
        }
        return vars;
    }

    /**
     * The type to declare a variable with, taken from the plugin that writes it where possible.
     *
     * <p>Guessing from the name alone is only safe for the conventional ones. A readiness variable
     * guessed as String makes a condition such as {@code gui_connected && avatar_ready} nonsense,
     * and the plugin that sets it has already said it is a Bool.
     */
    private String declaredVariableType(final JSONObject snapshot, final String variableName) {
        final JSONObject project = snapshot == null ? null : snapshot.optJSONObject("project");
        final JSONArray plugins = project == null ? null : project.optJSONArray("plugins");
        for (int i = 0; plugins != null && i < plugins.length(); i++) {
            final JSONObject plugin = plugins.optJSONObject(i);
            for (String direction : new String[] {"writesVariables", "readsVariables"}) {
                final JSONArray declared = plugin == null ? null : plugin.optJSONArray(direction);
                for (int j = 0; declared != null && j < declared.length(); j++) {
                    final JSONObject entry = declared.optJSONObject(j);
                    if (entry == null) {
                        continue;
                    }
                    final String bound = entry.optString("boundTo", entry.optString("name", ""));
                    final String type = entry.optString("type", "").trim();
                    if (bound.equals(variableName) && !type.isEmpty()) {
                        return type;
                    }
                }
            }
        }
        return inferVariableType(variableName);
    }

    private String inferVariableType(final String variableName) {
        final String lower = variableName == null ? "" : variableName.toLowerCase(Locale.ROOT);
        if (lower.contains("event")) {
            return "Event";
        }
        return "String";
    }

    private String stripQuotedContent(final String text) {
        final StringBuilder out = new StringBuilder(text.length());
        boolean inSingleQuote = false;
        boolean inDoubleQuote = false;
        boolean escaped = false;
        for (int i = 0; i < text.length(); i++) {
            final char c = text.charAt(i);
            if (escaped) {
                out.append(' ');
                escaped = false;
                continue;
            }
            if (c == '\\') {
                if (inSingleQuote || inDoubleQuote) {
                    out.append(' ');
                    escaped = true;
                } else {
                    out.append(c);
                }
                continue;
            }
            if (c == '\'' && !inDoubleQuote) {
                inSingleQuote = !inSingleQuote;
                out.append(' ');
                continue;
            }
            if (c == '"' && !inSingleQuote) {
                inDoubleQuote = !inDoubleQuote;
                out.append(' ');
                continue;
            }
            out.append((inSingleQuote || inDoubleQuote) ? ' ' : c);
        }
        return out.toString();
    }

    /**
     * Puts a wait for the agents in front of a flow that would otherwise start by using one.
     *
     * <p>An agent that has not connected yet silently swallows whatever it is told to do, so a flow
     * whose first step speaks before anything waits does nothing at all on a slow machine and works
     * on a fast one. That is the hardest kind of fault for a non-technical author to make sense of,
     * which is why the gate is offered rather than left to be discovered.
     *
     * <p>Nothing is added when the project has no plugin that reports readiness, when the flow
     * already waits on one of those variables, or when the generated flow attaches to an existing
     * node rather than starting the project.
     */
    private JSONObject prependReadinessGate(
            final JSONObject candidate, final JSONObject snapshot, final String situation) {
        final JSONObject metadata = candidate.optJSONObject("metadata");
        if (metadata != null && "template-wait-until-ready".equals(metadata.optString("source"))) {
            return candidate;
        }
        final JSONArray operations = candidate.optJSONArray("operations");
        if (operations == null || operations.length() == 0) {
            return candidate;
        }

        final SceneFlowIrTemplateLibrary library = new SceneFlowIrTemplateLibrary();
        final List<SceneFlowIrTemplateLibrary.ReadinessSignal> signals = library.readinessSignals(snapshot);
        if (signals.isEmpty() || flowAlreadyWaitsForReadiness(snapshot, signals)) {
            return candidate;
        }

        JSONObject startOp = null;
        final Set<String> reserved = new LinkedHashSet<>();
        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null) {
                continue;
            }
            final String name = op.optString("op", "");
            if ("create_node".equals(name)) {
                reserved.add(op.optString("nodeId", ""));
            } else if ("create_supernode".equals(name)) {
                reserved.add(op.optString("superNodeId", ""));
            } else {
                continue;
            }
            if (startOp == null && op.optBoolean("isStartNode", false)) {
                startOp = op;
            }
        }
        if (startOp == null) {
            return candidate;
        }

        final String continuationId = "create_supernode".equals(startOp.optString("op", ""))
                ? startOp.optString("superNodeId", "")
                : startOp.optString("nodeId", "");
        final JSONObject gate = library.readinessGateFor(
                situation, resolveRootId(snapshot), snapshot, continuationId, reserved);
        if (gate == null) {
            return candidate;
        }

        // What the flow used to start with now starts once the gate opens.
        startOp.remove("isStartNode");

        // The gate's own nodes go first and its release edge last, because that edge points at a
        // node the generated flow has not created yet at the top of the list.
        final JSONArray gateOps = gate.getJSONArray("operations");
        final JSONArray merged = new JSONArray();
        final JSONArray release = new JSONArray();
        for (int i = 0; i < gateOps.length(); i++) {
            final JSONObject op = gateOps.getJSONObject(i);
            if ("create_edge".equals(op.optString("op")) && "IEDGE".equals(op.optString("edgeType"))) {
                release.put(op);
            } else {
                merged.put(op);
            }
        }
        for (int i = 0; i < operations.length(); i++) {
            merged.put(operations.get(i));
        }
        for (int i = 0; i < release.length(); i++) {
            merged.put(release.get(i));
        }

        final JSONArray assumptions = candidate.optJSONArray("assumptions") == null
                ? new JSONArray()
                : candidate.getJSONArray("assumptions");
        final JSONArray gateAssumptions = gate.optJSONArray("assumptions");
        for (int i = 0; gateAssumptions != null && i < gateAssumptions.length(); i++) {
            assumptions.put(gateAssumptions.get(i));
        }

        if (metadata != null) {
            metadata.put("readinessGate", new JSONObject()
                    .put("added", true)
                    .put("waitsFor", gate.getJSONObject("metadata").optJSONArray("waitsFor"))
                    .put("gateSuperNodeId", gate.getJSONObject("metadata").optString("gateSuperNodeId"))
                    .put("continuationNodeId", continuationId)
                    .put("continuationName", startOp.optString("name", continuationId)));
        }
        return candidate.put("operations", merged).put("assumptions", assumptions);
    }

    /** Whether any condition in the flow already tests one of the readiness variables. */
    private boolean flowAlreadyWaitsForReadiness(
            final JSONObject snapshot, final List<SceneFlowIrTemplateLibrary.ReadinessSignal> signals) {
        final JSONObject flow = snapshot == null ? null : snapshot.optJSONObject("flow");
        final JSONArray edges = flow == null ? null : flow.optJSONArray("edges");
        for (int i = 0; edges != null && i < edges.length(); i++) {
            final JSONObject edge = edges.optJSONObject(i);
            final String condition = edge == null ? "" : edge.optString("conditionText", "");
            if (condition.isBlank()) {
                continue;
            }
            for (SceneFlowIrTemplateLibrary.ReadinessSignal signal : signals) {
                if (condition.contains(signal.variable())) {
                    return true;
                }
            }
        }
        return false;
    }

    private JSONObject assignCreateNodePositions(final JSONObject candidate, final JSONObject snapshot) {
        JSONArray operations = candidate.optJSONArray("operations");
        if (operations == null || operations.length() == 0) {
            return candidate;
        }

        final String rootId = resolveRootId(snapshot);
        final Map<String, Integer> siblingCounts = new HashMap<>();
        final Set<String> knownNodeIds = snapshotNodeIds(snapshot);
        knownNodeIds.addAll(createdNodeIds(operations));

        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null) {
                continue;
            }
            final String kind = op.optString("op", "");
            if ("create_supernode".equals(kind)) {
                if (op.optJSONObject("position") == null) {
                    String parent = op.optString("parentSuperNodeId", rootId);
                    int slot = siblingCounts.merge(parent, 1, Integer::sum) - 1;
                    op.put("position", new JSONObject()
                            .put("x", 120 + slot * 320)
                            .put("y", 120));
                }
                continue;
            }
            if (!"create_node".equals(kind)) {
                continue;
            }
            if (op.optJSONObject("position") != null) {
                continue;
            }
            final String parent = op.optString("parentSuperNodeId", rootId);
            int slot = siblingCounts.merge(parent, 1, Integer::sum) - 1;
            if (rootId.equals(parent)) {
                op.put("position", new JSONObject()
                        .put("x", 120 + slot * 320)
                        .put("y", 340));
            } else {
                op.put("position", new JSONObject()
                        .put("x", 160 + (slot % 3) * 260)
                        .put("y", 180 + (slot / 3) * 200));
            }
        }
        candidate.put("operations", operations);
        return candidate;
    }

    private JSONObject enforceWaitLoopCanonicalShape(
            final JSONObject candidate,
            final JSONObject snapshot,
            final String situation,
            final OutputMode outputMode) {
        if (!looksLikeWaitSituation(situation)) {
            return candidate;
        }
        JSONArray operations = candidate.optJSONArray("operations");
        if (operations == null || operations.length() == 0) {
            return candidate;
        }

        final String rootId = resolveRootId(snapshot);
        final Set<String> snapshotNodeIds = outputMode == OutputMode.STANDALONE
                ? new LinkedHashSet<>()
                : snapshotNodeIds(snapshot);
        final Set<String> candidateNodeIds = createdNodeIds(operations);
        final Set<String> allKnownNodeIds = new LinkedHashSet<>(snapshotNodeIds);
        allKnownNodeIds.addAll(candidateNodeIds);

        final JSONObject superNodeCreateOp = findWaitSuperNodeCreate(operations);
        if (superNodeCreateOp == null) {
            ensureInterruptSourceNodeLiveness(operations, situation);
            return candidate;
        }
        final String superNodeId = superNodeCreateOp.optString("superNodeId", "").trim();
        if (superNodeId.isEmpty()) {
            return candidate;
        }
        superNodeCreateOp.put("isStartNode", true);

        JSONObject waitingNodeOp = findWaitingNodeCreate(operations, superNodeId);
        final String waitLabel = extractLabel(situation, "OkayButtonPressed");
        final String waitSuffix = sanitizeId(waitLabel);
        if (waitingNodeOp == null) {
            final String waitNodeId = nextNumericId(allKnownNodeIds, "N", 1000);
            waitingNodeOp = new JSONObject()
                    .put("op", "create_node")
                    .put("parentSuperNodeId", superNodeId)
                    .put("nodeId", waitNodeId)
                    .put("name", "Waiting")
                    .put("isStartNode", true);
            operations.put(waitingNodeOp);
            allKnownNodeIds.add(waitNodeId);
        } else {
            waitingNodeOp.put("isStartNode", true);
            if (!waitingNodeOp.has("name")) {
                waitingNodeOp.put("name", waitingNodeOp.optString("nodeId", "Waiting"));
            }
        }
        final String waitingNodeId = waitingNodeOp.optString("nodeId", "").trim();
        if (waitingNodeId.isEmpty()) {
            return candidate;
        }

        final Set<String> currentChildNodeIds = childNodeIdsOf(operations, superNodeId);
        final List<JSONObject> interruptEdges = findInterruptEdgesFromSourceOrChildren(
                operations, superNodeId, currentChildNodeIds);
        if (interruptEdges.isEmpty()) {
            operations.put(new JSONObject()
                    .put("op", "create_edge")
                    .put("edgeId", uniqueEdgeId("WaitInterrupt_" + waitSuffix, operations))
                    .put("edgeType", "IEDGE")
                    .put("sourceNodeId", superNodeId)
                    .put("payload", new JSONObject().put("conditionText", "event == \"" + waitLabel + "\"")));
        } else {
            for (JSONObject edgeOp : interruptEdges) {
                edgeOp.put("sourceNodeId", superNodeId);
                if (!edgeOp.has("payload") || edgeOp.optJSONObject("payload") == null) {
                    edgeOp.put("payload", new JSONObject());
                }
                final JSONObject iPayload = edgeOp.getJSONObject("payload");
                if (iPayload.optString("conditionText", "").isBlank()) {
                    iPayload.put("conditionText", "event == \"" + waitLabel + "\"");
                }
            }
        }

        final Set<String> childNodeIds = childNodeIdsOf(operations, superNodeId);
        final boolean reminderRequested = looksLikeReminderSituation(situation);
        final int reminderTimeoutMs = extractReminderTimeoutMs(situation);
        if (reminderRequested) {
            ensureReminderInternalFlow(operations, waitingNodeId, superNodeId, allKnownNodeIds, reminderTimeoutMs);
        }

        final Map<String, String> continuationNodeBySuffix = new HashMap<>();
        final List<JSONObject> normalizedInterruptEdges = findInterruptEdgesFromSource(operations, superNodeId);
        for (JSONObject interruptEdgeOp : normalizedInterruptEdges) {
            final JSONObject payload = interruptEdgeOp.optJSONObject("payload");
            final String conditionText = payload == null ? "" : payload.optString("conditionText", "");
            final String conditionSuffix = conditionText.isBlank()
                    ? waitSuffix
                    : sanitizeId(extractConditionLiteral(conditionText, waitLabel));

            String iTarget = interruptEdgeOp.optString("targetNodeId", "").trim();
            final boolean unknownTarget = !allKnownNodeIds.contains(iTarget);
            final boolean targetNeedsRewrite = iTarget.isEmpty()
                    || childNodeIds.contains(iTarget)
                    || unknownTarget
                    || (outputMode == OutputMode.STANDALONE && unknownTarget);
            if (!targetNeedsRewrite) {
                continue;
            }
            final String suffixKey = conditionSuffix.isBlank() ? waitSuffix : conditionSuffix;
            String afterNodeId = continuationNodeBySuffix.get(suffixKey);
            if (afterNodeId == null || afterNodeId.isBlank()) {
                afterNodeId = findNodeIdByName(operations, rootId, "After_" + suffixKey);
                if (afterNodeId == null || afterNodeId.isBlank()) {
                    afterNodeId = nextNumericId(allKnownNodeIds, "N", 1000);
                    final JSONObject afterNodeOp = new JSONObject()
                            .put("op", "create_node")
                            .put("parentSuperNodeId", rootId)
                            .put("nodeId", afterNodeId)
                            .put("name", "After_" + suffixKey);
                    final int edgeIndex = findOperationIndex(operations, interruptEdgeOp);
                    if (edgeIndex >= 0) {
                        operations = insertOperationAt(operations, edgeIndex, afterNodeOp);
                        candidate.put("operations", operations);
                    } else {
                        operations.put(afterNodeOp);
                    }
                    allKnownNodeIds.add(afterNodeId);
                }
                continuationNodeBySuffix.put(suffixKey, afterNodeId);
            }
            interruptEdgeOp.put("targetNodeId", afterNodeId);
        }

        if (!reminderRequested && !hasSelfTimeoutEdge(operations, waitingNodeId)) {
            JSONObject tEdgeOp = new JSONObject()
                    .put("op", "create_edge")
                    .put("edgeId", uniqueEdgeId("WaitTimeout_" + waitSuffix, operations))
                    .put("edgeType", "TEDGE")
                    .put("sourceNodeId", waitingNodeId)
                    .put("targetNodeId", waitingNodeId)
                    .put("payload", new JSONObject().put("timeoutMs", 1000));
            operations.put(tEdgeOp);
        }
        return candidate;
    }

    private void ensureInterruptSourceNodeLiveness(final JSONArray operations, final String situation) {
        final String sourceNodeId = findPrimaryInterruptSourceNodeId(operations);
        if (sourceNodeId.isBlank()) {
            return;
        }
        if (hasSelfTimeoutEdge(operations, sourceNodeId)) {
            return;
        }
        final String waitSuffix = sanitizeId(extractLabel(situation, "Wait"));
        operations.put(new JSONObject()
                .put("op", "create_edge")
                .put("edgeId", uniqueEdgeId("WaitTimeout_" + waitSuffix, operations))
                .put("edgeType", "TEDGE")
                .put("sourceNodeId", sourceNodeId)
                .put("targetNodeId", sourceNodeId)
                .put("payload", new JSONObject().put("timeoutMs", 1000)));
    }

    private boolean looksLikeReminderSituation(final String situation) {
        if (situation == null) {
            return false;
        }
        final String lower = situation.toLowerCase(Locale.ROOT);
        return lower.contains("remind") || lower.contains("reminder");
    }

    private int extractReminderTimeoutMs(final String situation) {
        if (situation == null || situation.isBlank()) {
            return 5000;
        }
        final String lower = situation.toLowerCase(Locale.ROOT);
        final Matcher sec = Pattern.compile("(\\d+)\\s*(second|seconds|sec|s)\\b").matcher(lower);
        if (sec.find()) {
            return Math.max(1, Integer.parseInt(sec.group(1))) * 1000;
        }
        final Matcher ms = Pattern.compile("(\\d+)\\s*(millisecond|milliseconds|ms)\\b").matcher(lower);
        if (ms.find()) {
            return Math.max(1, Integer.parseInt(ms.group(1)));
        }
        final Matcher min = Pattern.compile("(\\d+)\\s*(minute|minutes|min|m)\\b").matcher(lower);
        if (min.find()) {
            return Math.max(1, Integer.parseInt(min.group(1))) * 60_000;
        }
        return 5000;
    }

    private void ensureReminderInternalFlow(
            final JSONArray operations,
            final String waitingNodeId,
            final String superNodeId,
            final Set<String> knownNodeIds,
            final int reminderTimeoutMs) {
        removeTimedEdgesFromSource(operations, waitingNodeId);

        String reminderNodeId = findNodeByNameOrParentHint(operations, superNodeId, "remind");
        if (reminderNodeId.isBlank()) {
            reminderNodeId = nextNumericId(knownNodeIds, "N", 1000);
            knownNodeIds.add(reminderNodeId);
            operations.put(new JSONObject()
                    .put("op", "create_node")
                    .put("parentSuperNodeId", superNodeId)
                    .put("nodeId", reminderNodeId)
                    .put("name", "Reminder"));
        }
        if (!hasTimedEdge(operations, waitingNodeId, reminderNodeId)) {
            operations.put(new JSONObject()
                    .put("op", "create_edge")
                    .put("edgeId", uniqueEdgeId("WaitToReminder", operations))
                    .put("edgeType", "TEDGE")
                    .put("sourceNodeId", waitingNodeId)
                    .put("targetNodeId", reminderNodeId)
                    .put("payload", new JSONObject().put("timeoutMs", reminderTimeoutMs)));
        }
        if (!hasTimedEdge(operations, reminderNodeId, waitingNodeId)) {
            operations.put(new JSONObject()
                    .put("op", "create_edge")
                    .put("edgeId", uniqueEdgeId("ReminderToWait", operations))
                    .put("edgeType", "TEDGE")
                    .put("sourceNodeId", reminderNodeId)
                    .put("targetNodeId", waitingNodeId)
                    .put("payload", new JSONObject().put("timeoutMs", 1000)));
        }
    }

    private void removeTimedEdgesFromSource(final JSONArray operations, final String sourceNodeId) {
        for (int i = operations.length() - 1; i >= 0; i--) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null || !"create_edge".equals(op.optString("op", ""))) {
                continue;
            }
            if (!"TEDGE".equals(op.optString("edgeType", ""))) {
                continue;
            }
            if (!sourceNodeId.equals(op.optString("sourceNodeId", ""))) {
                continue;
            }
            operations.remove(i);
        }
    }

    private boolean looksLikeWaitSituation(final String situation) {
        if (situation == null) {
            return false;
        }
        final String lower = situation.toLowerCase(Locale.ROOT);
        return (lower.contains("wait") || lower.contains("until")) && (lower.contains("press") || lower.contains("button"));
    }

    private JSONObject findWaitSuperNodeCreate(final JSONArray operations) {
        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null || !"create_supernode".equals(op.optString("op", ""))) {
                continue;
            }
            final String name = op.optString("name", "").toLowerCase(Locale.ROOT);
            final String id = op.optString("superNodeId", "").toLowerCase(Locale.ROOT);
            if (name.contains("wait") || id.contains("wait")) {
                return op;
            }
        }
        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op != null && "create_supernode".equals(op.optString("op", ""))) {
                return op;
            }
        }
        return null;
    }

    private JSONObject findWaitingNodeCreate(final JSONArray operations, final String superNodeId) {
        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null || !"create_node".equals(op.optString("op", ""))) {
                continue;
            }
            if (!superNodeId.equals(op.optString("parentSuperNodeId", ""))) {
                continue;
            }
            final String name = op.optString("name", "").toLowerCase(Locale.ROOT);
            final String id = op.optString("nodeId", "").toLowerCase(Locale.ROOT);
            if (name.contains("wait") || id.contains("wait")) {
                return op;
            }
        }
        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op != null
                    && "create_node".equals(op.optString("op", ""))
                    && superNodeId.equals(op.optString("parentSuperNodeId", ""))) {
                return op;
            }
        }
        return null;
    }

    private List<JSONObject> findInterruptEdgesFromSource(final JSONArray operations, final String sourceNodeId) {
        final List<JSONObject> edges = new ArrayList<>();
        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op != null
                    && "create_edge".equals(op.optString("op", ""))
                    && "IEDGE".equals(op.optString("edgeType", ""))
                    && sourceNodeId.equals(op.optString("sourceNodeId", ""))) {
                edges.add(op);
            }
        }
        return edges;
    }

    private String findPrimaryInterruptSourceNodeId(final JSONArray operations) {
        final Map<String, Integer> counts = new HashMap<>();
        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null
                    || !"create_edge".equals(op.optString("op", ""))
                    || !"IEDGE".equals(op.optString("edgeType", ""))) {
                continue;
            }
            final String source = op.optString("sourceNodeId", "").trim();
            if (source.isEmpty()) {
                continue;
            }
            counts.put(source, counts.getOrDefault(source, 0) + 1);
        }
        String bestId = "";
        int bestCount = 0;
        for (Map.Entry<String, Integer> entry : counts.entrySet()) {
            if (entry.getValue() > bestCount) {
                bestCount = entry.getValue();
                bestId = entry.getKey();
            }
        }
        return bestId;
    }

    private List<JSONObject> findInterruptEdgesFromSourceOrChildren(
            final JSONArray operations,
            final String superNodeId,
            final Set<String> childNodeIds) {
        final List<JSONObject> edges = new ArrayList<>();
        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null
                    || !"create_edge".equals(op.optString("op", ""))
                    || !"IEDGE".equals(op.optString("edgeType", ""))) {
                continue;
            }
            final String source = op.optString("sourceNodeId", "");
            if (superNodeId.equals(source) || childNodeIds.contains(source)) {
                edges.add(op);
            }
        }
        return edges;
    }

    private boolean hasSelfTimeoutEdge(final JSONArray operations, final String nodeId) {
        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null || !"create_edge".equals(op.optString("op", ""))) {
                continue;
            }
            if (!"TEDGE".equals(op.optString("edgeType", ""))) {
                continue;
            }
            if (nodeId.equals(op.optString("sourceNodeId", ""))
                    && nodeId.equals(op.optString("targetNodeId", ""))) {
                return true;
            }
        }
        return false;
    }

    private boolean hasTimedEdge(final JSONArray operations, final String sourceNodeId, final String targetNodeId) {
        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null || !"create_edge".equals(op.optString("op", ""))) {
                continue;
            }
            if (!"TEDGE".equals(op.optString("edgeType", ""))) {
                continue;
            }
            if (sourceNodeId.equals(op.optString("sourceNodeId", ""))
                    && targetNodeId.equals(op.optString("targetNodeId", ""))) {
                return true;
            }
        }
        return false;
    }

    private String findNodeByNameOrParentHint(final JSONArray operations, final String parentSuperNodeId, final String nameHint) {
        final String lowerHint = nameHint == null ? "" : nameHint.toLowerCase(Locale.ROOT);
        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null || !"create_node".equals(op.optString("op", ""))) {
                continue;
            }
            if (!parentSuperNodeId.equals(op.optString("parentSuperNodeId", ""))) {
                continue;
            }
            final String name = op.optString("name", "").toLowerCase(Locale.ROOT);
            final String id = op.optString("nodeId", "");
            if (name.contains(lowerHint)) {
                return id;
            }
        }
        return "";
    }

    private String findNodeIdByName(final JSONArray operations, final String parentSuperNodeId, final String exactName) {
        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null || !"create_node".equals(op.optString("op", ""))) {
                continue;
            }
            if (!parentSuperNodeId.equals(op.optString("parentSuperNodeId", ""))) {
                continue;
            }
            if (exactName.equals(op.optString("name", ""))) {
                return op.optString("nodeId", "");
            }
        }
        return "";
    }

    private int findOperationIndex(final JSONArray operations, final JSONObject target) {
        if (operations == null || target == null) {
            return -1;
        }
        for (int i = 0; i < operations.length(); i++) {
            if (operations.optJSONObject(i) == target) {
                return i;
            }
        }
        return -1;
    }

    private String extractConditionLiteral(final String conditionText, final String fallback) {
        if (conditionText == null || conditionText.isBlank()) {
            return fallback;
        }
        final Matcher matcher = Pattern.compile("\"([^\"]+)\"").matcher(conditionText);
        if (matcher.find()) {
            final String literal = matcher.group(1).trim();
            if (!literal.isEmpty()) {
                return literal;
            }
        }
        return fallback;
    }

    private Set<String> snapshotNodeIds(final JSONObject snapshot) {
        final Set<String> ids = new LinkedHashSet<>();
        final JSONObject flow = snapshot == null ? null : snapshot.optJSONObject("flow");
        if (flow == null) {
            return ids;
        }
        final String rootId = flow.optString("rootId", "").trim();
        if (!rootId.isEmpty()) {
            ids.add(rootId);
        }
        final JSONArray nodes = flow.optJSONArray("nodes");
        if (nodes == null) {
            return ids;
        }
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
        return ids;
    }

    private Set<String> createdNodeIds(final JSONArray operations) {
        final Set<String> ids = new LinkedHashSet<>();
        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null) {
                continue;
            }
            final String kind = op.optString("op", "");
            if ("create_node".equals(kind)) {
                final String id = op.optString("nodeId", "").trim();
                if (!id.isEmpty()) {
                    ids.add(id);
                }
            } else if ("create_supernode".equals(kind)) {
                final String id = op.optString("superNodeId", "").trim();
                if (!id.isEmpty()) {
                    ids.add(id);
                }
            }
        }
        return ids;
    }

    private Set<String> childNodeIdsOf(final JSONArray operations, final String superNodeId) {
        final Set<String> ids = new LinkedHashSet<>();
        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null || !"create_node".equals(op.optString("op", ""))) {
                continue;
            }
            if (!superNodeId.equals(op.optString("parentSuperNodeId", ""))) {
                continue;
            }
            final String id = op.optString("nodeId", "").trim();
            if (!id.isEmpty()) {
                ids.add(id);
            }
        }
        return ids;
    }

    private String uniqueId(final String base, final Set<String> taken) {
        String candidate = base;
        int n = 2;
        while (taken.contains(candidate)) {
            candidate = base + "_" + n++;
        }
        return candidate;
    }

    private String uniqueEdgeId(final String base, final JSONArray operations) {
        final Set<String> taken = new LinkedHashSet<>();
        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null) {
                continue;
            }
            final String edgeId = op.optString("edgeId", "").trim();
            if (!edgeId.isEmpty()) {
                taken.add(edgeId);
            }
        }
        return uniqueId(base, taken);
    }

    private String nextNumericId(final Set<String> taken, final String prefix, final int fallbackStart) {
        int max = fallbackStart - 1;
        for (String id : taken) {
            if (id == null || !id.startsWith(prefix) || id.length() == prefix.length()) {
                continue;
            }
            final String numeric = id.substring(prefix.length());
            try {
                max = Math.max(max, Integer.parseInt(numeric));
            } catch (NumberFormatException ignored) {
                // ignore non-numeric suffixes
            }
        }
        String candidate = prefix + (max + 1);
        while (taken.contains(candidate)) {
            max += 1;
            candidate = prefix + (max + 1);
        }
        return candidate;
    }

    private JSONArray insertOperationAt(final JSONArray operations, final int index, final JSONObject newOp) {
        final JSONArray out = new JSONArray();
        for (int i = 0; i < operations.length(); i++) {
            if (i == index) {
                out.put(newOp);
            }
            out.put(operations.get(i));
        }
        if (index >= operations.length()) {
            out.put(newOp);
        }
        return out;
    }

    private String extractLabel(final String text, final String fallback) {
        if (text == null) {
            return fallback;
        }
        final int quoteStart = text.indexOf('"');
        if (quoteStart >= 0) {
            final int quoteEnd = text.indexOf('"', quoteStart + 1);
            if (quoteEnd > quoteStart + 1) {
                return text.substring(quoteStart + 1, quoteEnd);
            }
        }
        final String lower = text.toLowerCase(Locale.ROOT);
        if ((lower.contains("ok") || lower.contains("okay")) && lower.contains("pressed")) {
            return "OkayButtonPressed";
        }
        if (lower.contains("ok") || lower.contains("okay")) {
            return "OkayButton";
        }
        if (lower.contains("cancel")) {
            return "CancelButton";
        }
        return fallback;
    }

    private String sanitizeId(final String value) {
        final String id = value == null ? "" : value.replaceAll("[^A-Za-z0-9_]", "_");
        return id.isEmpty() ? "Value" : id;
    }

    private Path defaultGeneratedProjectDir(final Path outputPath) {
        final Path parent = outputPath == null ? null : outputPath.getParent();
        if (parent == null) {
            return Path.of("build", "reports", "sceneflow-generated-project");
        }
        return parent.resolve("sceneflow-generated-project");
    }

    private Path createGeneratedProject(
            final JSONObject snapshot,
            final Path generatedSceneFlowPath,
            final Path targetProjectDir) throws SceneFlowIrCompileException {
        try {
            Files.createDirectories(targetProjectDir);
            Files.copy(generatedSceneFlowPath, targetProjectDir.resolve("sceneflow.xml"),
                    StandardCopyOption.REPLACE_EXISTING);
            Files.writeString(targetProjectDir.resolve("project.xml"), buildProjectXml(snapshot) + System.lineSeparator());
            Files.writeString(targetProjectDir.resolve("acticon.xml"),
                    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<Acticon>\n</Acticon>\n");
            Files.writeString(targetProjectDir.resolve("gesticon.xml"),
                    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<Gesticon>\n</Gesticon>\n");
            Files.writeString(targetProjectDir.resolve("visicon.xml"),
                    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<Visicon>\n</Visicon>\n");
            Files.writeString(targetProjectDir.resolve("scenescript.xml"),
                    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<SceneScript lower=\"0\" upper=\"0\">\n</SceneScript>\n");
            Files.writeString(targetProjectDir.resolve("editorconfig.xml"), defaultEditorConfigXml());
            return targetProjectDir;
        } catch (IOException exc) {
            throw new SceneFlowIrCompileException("Generated sceneflow succeeded but project creation failed: " + exc.getMessage(), exc);
        }
    }

    private String buildProjectXml(final JSONObject snapshot) {
        final JSONObject project = snapshot == null ? null : snapshot.optJSONObject("project");
        final String projectName = xmlEscape(project == null ? "GeneratedSceneFlow" : project.optString("name", "GeneratedSceneFlow"));
        final boolean androidProject = project != null && project.optBoolean("androidProject", false);

        final StringBuilder xml = new StringBuilder();
        xml.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        xml.append("<Project name=\"").append(projectName).append("\" androidProject=\"")
                .append(androidProject).append("\">\n");
        xml.append("  <Plugins>\n");
        xml.append("  </Plugins>\n");
        xml.append("  <Agents>\n");
        xml.append("  </Agents>\n");
        xml.append("  <SemanticServices>\n");
        xml.append("  </SemanticServices>\n");
        xml.append("  <Player>\n");
        xml.append("  </Player>\n");
        xml.append("</Project>\n");
        return xml.toString();
    }

    private String defaultEditorConfigXml() {
        return "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                + "<!DOCTYPE properties SYSTEM \"http://java.sun.com/dtd/properties.dtd\">\n"
                + "<properties>\n"
                + "<comment>VSM Editor Config</comment>\n"
                + "<entry key=\"visualization\">true</entry>\n"
                + "<entry key=\"autosave\">false</entry>\n"
                + "<entry key=\"showsceneelements\">false</entry>\n"
                + "<entry key=\"command_log_max\">5000</entry>\n"
                + "<entry key=\"num_magnets\">8</entry>\n"
                + "<entry key=\"grid_x\">1</entry>\n"
                + "<entry key=\"scriptfonttype\">Monospaced</entry>\n"
                + "<entry key=\"node_width\">90</entry>\n"
                + "<entry key=\"shownodeid\">true</entry>\n"
                + "<entry key=\"undo_max_depth\">500</entry>\n"
                + "<entry key=\"node_height\">90</entry>\n"
                + "<entry key=\"showvariables\">true</entry>\n"
                + "<entry key=\"scriptfonsize\">16</entry>\n"
                + "<entry key=\"workspace_fontsize\">14</entry>\n"
                + "<entry key=\"visualizationtrace\">true</entry>\n"
                + "<entry key=\"defaultsupernodename\">default</entry>\n"
                + "<entry key=\"grid\">true</entry>\n"
                + "<entry key=\"autohidebottombar\">true</entry>\n"
                + "<entry key=\"grid_y\">1</entry>\n"
                + "</properties>\n";
    }

    private String xmlEscape(final String value) {
        if (value == null) {
            return "";
        }
        return value
                .replace("&", "&amp;")
                .replace("\"", "&quot;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("'", "&apos;");
    }
}
