package de.dfki.vsm.web;

import de.dfki.vsm.sceneflow.ir.AuthoringResources;
import de.dfki.vsm.sceneflow.ir.ConstraintResolutionMode;
import de.dfki.vsm.sceneflow.ir.SceneFlowIrCompileException;
import de.dfki.vsm.sceneflow.ir.SceneFlowIrLlmCandidateProvider;
import de.dfki.vsm.sceneflow.ir.SceneFlowSituationPipeline;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Turns a situation an author describes into a proposal they can read, and keeps the compiled result
 * until they decide whether to apply it.
 *
 * <p>The generator's intermediate representation never leaves this class. Authors are told what the
 * proposal does in the vocabulary of the editor (steps, scenes, waiting, variables); the operation
 * list that produces it is an implementation detail they have no way to act on. See
 * {@code doc/sceneflow-modelling-support-concept.md} section 4b for why applying a proposal is an
 * ordered plan rather than a single patch: the representation has no operation for creating a scene
 * or a screen, so those resources are reported and left to the author or to a later step.
 */
public final class FlowAssistantService {

    private static final String CATALOG_FILE = "interactive-design-pattern-catalog.json";

    /** How long a proposal stays applicable before it is discarded. */
    private static final long PROPOSAL_TTL_MS = 30L * 60L * 1000L;

    /** Matches the scene a PlayScene command plays, in either quoting style. */
    private static final Pattern PLAY_SCENE = Pattern.compile(
            "PlayScene\\s*\\(\\s*([\"'])(.*?)\\1", Pattern.CASE_INSENSITIVE);

    /** Matches an action asked of a device: {@code PlayAction("[agent name key='value']")}. */
    private static final Pattern PLAY_ACTION = Pattern.compile(
            "PlayAction\\s*\\(\\s*\"\\s*\\[\\s*(\\S+)\\s+(\\S+)\\s*(.*?)\\]",
            Pattern.CASE_INSENSITIVE);

    private final Map<String, Proposal> mProposals = new ConcurrentHashMap<>();

    /**
     * The plugin classes this deployment carries, as {@code className -> display name}.
     *
     * <p>Supplied rather than read, so the service stays usable without a server behind it. Empty by
     * default, which makes every unmet capability read as a dead end, so a caller that has a registry
     * has to say so.
     */
    private java.util.function.Supplier<Map<String, String>> mInstalledPlugins = Map::of;

    /** Tells the service which plugins exist on this deployment but are not in the project. */
    public FlowAssistantService withInstalledPlugins(
            final java.util.function.Supplier<Map<String, String>> installedPlugins) {
        this.mInstalledPlugins = installedPlugins == null ? Map::of : installedPlugins;
        return this;
    }

    /** A generated flow the author has not decided about yet. */
    public static final class Proposal {

        private final String mId;
        private final String mProjectId;
        private final String mStatus;
        private final JSONObject mAuthorView;
        private final String mSceneFlowXml;
        private final FlowAssistantSetup mSetup;
        private final long mCreatedAt;

        private Proposal(final String id, final String projectId, final String status,
                         final JSONObject authorView, final String sceneFlowXml,
                         final FlowAssistantSetup setup) {
            this.mId = id;
            this.mProjectId = projectId;
            this.mStatus = status;
            this.mAuthorView = authorView;
            this.mSceneFlowXml = sceneFlowXml;
            this.mSetup = setup;
            this.mCreatedAt = System.currentTimeMillis();
        }

        /**
         * What has to be added to the project before the flow can work, in order.
         *
         * <p>Carried out by the caller, because none of it is an operation on a flow: a device, an
         * agent and a screen live in the project's configuration and beside it on disk.
         */
        List<FlowAssistantSetup.Step> setupSteps() {
            return mSetup == null ? List.of() : mSetup.steps();
        }

        public String id() {
            return mId;
        }

        public String projectId() {
            return mProjectId;
        }

        public String status() {
            return mStatus;
        }

        /** The only part of a proposal that may be sent to a client. */
        public JSONObject authorView() {
            return mAuthorView;
        }

        /** The merged flow, base plus proposal, or {@code null} when nothing was generated. */
        public String sceneFlowXml() {
            return mSceneFlowXml;
        }

        public boolean isApplicable() {
            return "ready".equals(mStatus) && mSceneFlowXml != null && !mSceneFlowXml.isBlank();
        }
    }

    /**
     * The pattern catalogue, reduced to what an author can act on.
     *
     * <p>Patterns that are not implemented yet are listed too, marked as such, so the panel can show
     * what is coming rather than pretending the catalogue is only what is buildable today.
     */
    public JSONObject catalogue() {
        final JSONObject source = AuthoringResources.read(null, CATALOG_FILE);
        final JSONArray patterns = new JSONArray();
        if (source != null) {
            final JSONArray library = source.optJSONArray("patternLibrary");
            for (int i = 0; library != null && i < library.length(); i++) {
                final JSONObject entry = library.optJSONObject(i);
                if (entry == null || entry.optString("id", "").isBlank()) {
                    continue;
                }
                patterns.put(authorFacingPattern(entry));
            }
        }
        return new JSONObject()
                .put("catalogVersion", source == null ? "" : source.optString("catalogVersion", ""))
                .put("patterns", patterns);
    }

    private JSONObject authorFacingPattern(final JSONObject entry) {
        final JSONObject out = new JSONObject()
                .put("id", entry.optString("id"))
                .put("label", entry.optString("label", entry.optString("id")))
                .put("description", entry.optString("humanDescription", ""))
                .put("available", "implemented".equalsIgnoreCase(entry.optString("status", "")));
        if (entry.has("level")) {
            out.put("level", entry.optInt("level"));
        }
        final JSONArray script = entry.optJSONArray("assistantScript");
        if (script != null) {
            final JSONArray questions = new JSONArray();
            for (int i = 0; i < script.length(); i++) {
                final JSONObject slot = script.optJSONObject(i);
                if (slot == null) {
                    continue;
                }
                questions.put(new JSONObject()
                        .put("slot", slot.optString("slot"))
                        .put("ask", slot.optString("ask"))
                        .put("kind", slot.optString("kind")));
            }
            out.put("questions", questions);
        }
        return out;
    }

    /**
     * Generates a flow for {@code situation} and keeps it until it is applied or expires.
     *
     * @param capabilities     the project's capability snapshot
     * @param baseSceneFlowXml the flow the author is currently looking at
     */
    public Proposal propose(final String projectId,
                            final JSONObject capabilities,
                            final String baseSceneFlowXml,
                            final String situation) throws IOException {
        return propose(projectId, capabilities, baseSceneFlowXml, situation, true);
    }

    public Proposal propose(final String projectId,
                            final JSONObject capabilities,
                            final String baseSceneFlowXml,
                            final String situation,
                            final boolean readinessGate) throws IOException {
        return propose(projectId, capabilities, baseSceneFlowXml, situation, readinessGate, null);
    }

    /**
     * @param readinessGate whether a flow that would start by using an agent gets a wait for that
     *                      agent put in front of it. False is how an author says they have handled
     *                      readiness elsewhere.
     * @param llm           the language service the project selected for the assistant, or null to
     *                      work from the built-in patterns alone. Selected, it is used only for
     *                      situations no pattern recognises: pattern output is validated and says
     *                      the same thing every time, which is not something a model should get the
     *                      chance to replace.
     */
    public Proposal propose(final String projectId,
                            final JSONObject capabilities,
                            final String baseSceneFlowXml,
                            final String situation,
                            final boolean readinessGate,
                            final SceneFlowIrLlmCandidateProvider.Config llm) throws IOException {
        expireStaleProposals();

        final Path work = Files.createTempDirectory("vsm-flow-assistant-");
        try {
            final Path snapshotPath = work.resolve("capabilities.json");
            final Path basePath = work.resolve("sceneflow.xml");
            final Path outputPath = work.resolve("proposed-sceneflow.xml");
            final Path reportPath = work.resolve("report.json");
            // What the project is missing, and the project as it would be once that is added. The
            // flow is generated against the latter, so a proposal is one coherent thing rather than
            // a flow plus a note saying it cannot work yet.
            final FlowAssistantSetup setup =
                    FlowAssistantSetup.plan(capabilities, situation, mInstalledPlugins.get());
            final JSONObject projected = setup.project(capabilities);
            Files.writeString(snapshotPath, projected.toString(2), StandardCharsets.UTF_8);
            Files.writeString(basePath, baseSceneFlowXml == null ? "" : baseSceneFlowXml,
                    StandardCharsets.UTF_8);

            JSONObject report;
            try {
                report = new SceneFlowSituationPipeline().run(
                        snapshotPath, basePath, outputPath, reportPath, situation,
                        new SceneFlowSituationPipeline.Settings(
                                llm == null
                                        ? SceneFlowSituationPipeline.CandidateMode.TEMPLATE
                                        : SceneFlowSituationPipeline.CandidateMode.TEMPLATE_THEN_LLM,
                                SceneFlowSituationPipeline.OutputMode.PATCH,
                                llm,
                                ConstraintResolutionMode.PERMISSIVE,
                                readinessGate),
                        work.resolve("generated-project"));
            } catch (SceneFlowIrCompileException exc) {
                report = new JSONObject()
                        .put("status", "failed")
                        .put("situation", situation == null ? "" : situation)
                        .put("failureReason", exc.getMessage());
            }

            final String status = normalizeStatus(report.optString("status", "failed"));
            final String mergedXml = "ready".equals(status) && Files.isRegularFile(outputPath)
                    ? Files.readString(outputPath, StandardCharsets.UTF_8)
                    : null;

            final JSONObject view = buildAuthorView(status, situation, report, projected);
            if (!setup.isEmpty() && !"failed".equals(status)) {
                view.put("setup", setup.toJson());
            }
            final Proposal proposal = new Proposal(
                    UUID.randomUUID().toString(), projectId, status,
                    view, mergedXml, setup);
            proposal.mAuthorView.put("proposalId", proposal.id());
            mProposals.put(proposal.id(), proposal);
            return proposal;
        } finally {
            deleteRecursively(work);
        }
    }

    /** Returns a proposal that is still applicable, or {@code null}. */
    public Proposal take(final String proposalId, final String projectId) {
        expireStaleProposals();
        final Proposal proposal = proposalId == null ? null : mProposals.get(proposalId);
        if (proposal == null || !proposal.projectId().equals(projectId)) {
            return null;
        }
        return proposal;
    }

    /** Drops a proposal, whether it was applied or discarded. */
    public void discard(final String proposalId) {
        if (proposalId != null) {
            mProposals.remove(proposalId);
        }
    }

    private void expireStaleProposals() {
        final long cutoff = System.currentTimeMillis() - PROPOSAL_TTL_MS;
        mProposals.values().removeIf(proposal -> proposal.mCreatedAt < cutoff);
    }

    private String normalizeStatus(final String pipelineStatus) {
        switch (pipelineStatus) {
            case "success":
                return "ready";
            case "no_pattern_matched":
                return "no_pattern_matched";
            default:
                return "failed";
        }
    }

    // ---------------------------------------------------------------- author-facing view

    private JSONObject buildAuthorView(final String status,
                                       final String situation,
                                       final JSONObject report,
                                       final JSONObject capabilities) {
        final JSONObject view = new JSONObject()
                .put("status", status)
                .put("situation", situation == null ? "" : situation);

        if ("no_pattern_matched".equals(status)) {
            final JSONObject noMatch = report.optJSONObject("noMatch");
            view.put("message", noMatch == null
                    ? "No pattern recognises this situation, so nothing was generated."
                    : noMatch.optString("reason"));
            view.put("recognisedSituations", noMatch == null
                    ? new JSONArray()
                    : noMatch.optJSONArray("recognisedSituations"));
            // An author who selected a language service for exactly this case has to hear that it
            // was tried and could not be reached, rather than reading a plain "not recognised".
            addNotes(view, report);
            return view;
        }

        final JSONObject accepted = acceptedAttempt(report);
        if (accepted == null) {
            view.put("message", "The situation was recognised, but the flow it produced did not hold "
                    + "together, so nothing is being proposed.");
            view.put("problems", rejectionReasons(report));
            return view;
        }

        final JSONObject patternInfo = accepted.optJSONObject("interactiveDesignPattern");
        final JSONObject catalogEntry = patternInfo == null
                ? null
                : patternInfo.optJSONObject("catalogEntry");
        if (catalogEntry != null) {
            view.put("pattern", authorFacingPattern(catalogEntry));
        }

        // Where a proposal came from changes how much an author should trust it without reading it.
        // A pattern was built and tested here; a model's answer was not.
        view.put("generatedBy", "llm".equals(accepted.optString("templateSource", ""))
                ? "language-model"
                : "pattern");

        final JSONObject candidate = accepted.optJSONObject("candidate");
        view.put("changes", describeChanges(candidate));
        describeReadinessGate(candidate).ifPresent(gate -> view.put("readinessGate", gate));
        view.put("resources", checkResources(catalogEntry, candidate, capabilities));
        view.put("assumptions", report.optJSONArray("assumptions") == null
                ? new JSONArray()
                : report.optJSONArray("assumptions"));
        addNotes(view, report);
        return view;
    }

    /** Anything that went wrong on the way but did not stop a proposal from being made. */
    private void addNotes(final JSONObject view, final JSONObject report) {
        final JSONArray warnings = report.optJSONArray("generationWarnings");
        if (warnings != null && warnings.length() > 0) {
            view.put("notes", warnings);
        }
    }

    private JSONObject acceptedAttempt(final JSONObject report) {
        final JSONArray attempts = report.optJSONArray("attempts");
        for (int i = 0; attempts != null && i < attempts.length(); i++) {
            final JSONObject attempt = attempts.optJSONObject(i);
            if (attempt != null && "accepted".equals(attempt.optString("status"))) {
                return attempt;
            }
        }
        return null;
    }

    /**
     * Why every attempt was turned down, in the words the rules already use.
     *
     * <p>Rule messages are written for authors, so they are passed through. Compiler errors are not,
     * so they are reported as one plain sentence instead.
     */
    private JSONArray rejectionReasons(final JSONObject report) {
        final JSONArray problems = new JSONArray();
        final Set<String> seen = new LinkedHashSet<>();
        final JSONArray attempts = report.optJSONArray("attempts");
        for (int i = 0; attempts != null && i < attempts.length(); i++) {
            final JSONObject attempt = attempts.optJSONObject(i);
            if (attempt == null) {
                continue;
            }
            final JSONArray issues = attempt.optJSONArray("semanticIssues");
            for (int j = 0; issues != null && j < issues.length(); j++) {
                final JSONObject issue = issues.optJSONObject(j);
                if (issue == null || !"error".equalsIgnoreCase(issue.optString("severity", ""))) {
                    continue;
                }
                final String message = issue.optString("message", "").trim();
                if (!message.isEmpty() && seen.add(message)) {
                    problems.put(message);
                }
            }
            if (attempt.has("compileError") && seen.add("compile")) {
                problems.put("The generated flow could not be turned into a valid graph.");
            }
        }
        return problems;
    }

    /**
     * The wait for the agents, when one was put in front of the flow, said as one thing.
     *
     * <p>Its four operations describe themselves badly one by one: a supernode, an empty node, a
     * loop onto itself and a condition over variables an author has never heard of. What the author
     * needs to know is that the flow now starts by waiting, for whom, and that they can say no.
     */
    private Optional<JSONObject> describeReadinessGate(final JSONObject candidate) {
        final JSONObject metadata = candidate == null ? null : candidate.optJSONObject("metadata");
        final JSONObject gate = metadata == null ? null : metadata.optJSONObject("readinessGate");
        if (gate == null || !gate.optBoolean("added", false)) {
            return Optional.empty();
        }
        final JSONArray waitsFor = gate.optJSONArray("waitsFor");
        final List<String> agents = new ArrayList<>();
        boolean onlyConnects = false;
        for (int i = 0; waitsFor != null && i < waitsFor.length(); i++) {
            final JSONObject entry = waitsFor.optJSONObject(i);
            if (entry == null) {
                continue;
            }
            agents.add(entry.optString("agent"));
            onlyConnects |= !entry.optBoolean("meansCanAct", true);
        }

        final String continuation = gate.optString("continuationName", "").isBlank()
                ? "the first step"
                : quoted(gate.optString("continuationName"));
        final StringBuilder detail = new StringBuilder();
        detail.append(agents.size() == 1
                ? "The flow now waits for " + joinQuoted(agents) + " before "
                : "The flow now waits for " + joinQuoted(agents) + " before ");
        detail.append(continuation).append(" runs. ");
        detail.append("An agent that has not finished starting up accepts what it is told and does "
                + "nothing with it, which looks like a flow that is broken for no reason.");
        if (onlyConnects) {
            detail.append(agents.size() == 1
                    ? " It reports only that it has connected, which can happen a moment before it "
                            + "is really usable."
                    : " One of them reports only that it has connected, which can happen a moment "
                            + "before it is really usable.");
        }

        return Optional.of(new JSONObject()
                .put("added", true)
                .put("agents", new JSONArray(agents))
                .put("detail", detail.toString())
                .put("canTurnOff", true));
    }

    /** Restates the generated operations as things an author would recognise on the canvas. */
    private JSONArray describeChanges(final JSONObject candidate) {
        final JSONArray changes = new JSONArray();
        final JSONArray operations = candidate == null ? null : candidate.optJSONArray("operations");
        if (operations == null) {
            return changes;
        }
        final JSONObject metadata = candidate.optJSONObject("metadata");
        if (metadata != null && "template-wait-until-ready".equals(metadata.optString("source"))) {
            // Asked for outright rather than added on top, so it is the change, not a footnote.
            return describeGateAsTheWholeProposal(metadata);
        }
        final Map<String, String> nodeNames = new LinkedHashMap<>();
        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null) {
                continue;
            }
            if ("create_node".equals(op.optString("op")) || "create_supernode".equals(op.optString("op"))) {
                nodeNames.put(op.optString("nodeId"),
                        op.optString("name", op.optString("nodeId")));
            }
        }

        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null || isReadinessGateOperation(op)) {
                // The gate is described as one thing by describeReadinessGate, because its parts
                // read as noise: an empty node, a loop onto itself, a condition over variables an
                // author has never seen.
                continue;
            }
            final String sentence = describeOperation(op, nodeNames);
            if (sentence != null) {
                changes.put(sentence);
            }
        }
        return changes;
    }

    private boolean isReadinessGateOperation(final JSONObject op) {
        return op.optString("opId", "").startsWith("gate-");
    }

    /** The gate said as what it does, for the case where waiting is the whole request. */
    private JSONArray describeGateAsTheWholeProposal(final JSONObject metadata) {
        final List<String> agents = new ArrayList<>();
        final List<String> connectOnly = new ArrayList<>();
        final JSONArray waitsFor = metadata.optJSONArray("waitsFor");
        for (int i = 0; waitsFor != null && i < waitsFor.length(); i++) {
            final JSONObject entry = waitsFor.optJSONObject(i);
            if (entry == null) {
                continue;
            }
            agents.add(entry.optString("agent"));
            if (!entry.optBoolean("meansCanAct", true)) {
                connectOnly.add(entry.optString("agent"));
            }
        }

        final JSONArray changes = new JSONArray();
        changes.put(agents.size() == 1
                ? "The flow starts by waiting for " + joinQuoted(agents) + " and carries on as soon "
                        + "as it is ready."
                : "The flow starts by waiting, and carries on only once " + joinQuoted(agents)
                        + " are all ready.");
        changes.put("While it waits it does nothing. This is where a scene goes if the person should "
                + "be told that something is coming.");
        if (!connectOnly.isEmpty()) {
            changes.put(joinQuoted(connectOnly) + " reports only that it has connected, which can "
                    + "happen a moment before it is really usable.");
        }
        return changes;
    }

    private String describeOperation(final JSONObject op, final Map<String, String> nodeNames) {
        final String name = op.optString("op", "");
        switch (name) {
            case "create_node":
            case "create_supernode":
                return "Adds a step called " + quoted(nodeNames.getOrDefault(
                        op.optString("nodeId"), op.optString("nodeId")))
                        + (op.optBoolean("isStartNode", false) ? ", where the flow starts." : ".");
            case "create_edge":
                return describeEdge(op, nodeNames);
            case "add_node_command":
                return describeCommand(op, nodeNames);
            case "add_variable_definition": {
                final String variable = variableName(op);
                if (variable.isEmpty()) {
                    return null;
                }
                final String reason = op.optString("reason", "").trim();
                return "Remembers " + quoted(variable) + "."
                        + (reason.isEmpty() ? " The flow can use it later." : " " + reason);
            }
            default:
                return null;
        }
    }

    private String describeEdge(final JSONObject op, final Map<String, String> nodeNames) {
        final String from = quoted(nodeNames.getOrDefault(
                op.optString("sourceNodeId"), op.optString("sourceNodeId")));
        final String to = quoted(nodeNames.getOrDefault(
                op.optString("targetNodeId"), op.optString("targetNodeId")));
        final JSONObject payload = op.optJSONObject("payload");
        switch (op.optString("edgeType", "")) {
            case "EEDGE":
                return "Continues from " + from + " to " + to + " once " + from + " has finished.";
            case "TEDGE": {
                final int timeout = payload == null ? 0 : payload.optInt("timeoutMs", 0);
                final String delay = timeout > 0 ? readableDuration(timeout) : "a moment";
                return from.equals(to)
                        ? "Keeps " + from + " waiting, checking again every " + delay + "."
                        : "Moves from " + from + " to " + to + " after " + delay + ".";
            }
            case "CEDGE":
            case "IEDGE": {
                final String condition = payload == null ? "" : payload.optString("conditionText", "");
                return "Leaves " + from + " for " + to
                        + (condition.isBlank() ? "." : " as soon as " + readableCondition(condition) + ".");
            }
            case "PEDGE": {
                final int probability = payload == null ? 0 : payload.optInt("probability", 0);
                return "Goes from " + from + " to " + to + " in " + probability + " out of 100 runs.";
            }
            default:
                return "Connects " + from + " to " + to + ".";
        }
    }

    private String describeCommand(final JSONObject op, final Map<String, String> nodeNames) {
        final String node = quoted(nodeNames.getOrDefault(
                op.optString("nodeId"), op.optString("nodeId")));
        final String commandText = op.optString("commandText", "").trim();
        final Matcher scene = PLAY_SCENE.matcher(commandText);
        if (scene.find()) {
            return node + " plays the scene " + quoted(scene.group(2)) + ".";
        }
        // Before the assignment case below, which splits on the first "=" and would tear an action
        // apart at the first of its parameters.
        final Matcher action = PLAY_ACTION.matcher(commandText);
        if (action.find()) {
            return node + " " + describeAction(action.group(1), action.group(2), action.group(3));
        }
        final int assign = commandText.indexOf('=');
        if (assign > 0 && commandText.charAt(assign - 1) != '!'
                && (assign + 1 >= commandText.length() || commandText.charAt(assign + 1) != '=')) {
            final String target = commandText.substring(0, assign).trim();
            final String value = commandText.substring(assign + 1).trim();
            if (value.isEmpty() || "\"\"".equals(value)) {
                return node + " empties " + quoted(target) + ", ready for a new value.";
            }
            return node + " sets " + quoted(target) + " to " + value + ".";
        }
        return node + " runs " + quoted(commandText) + ".";
    }

    /**
     * An action asked of a device, said as what it achieves rather than as what it is called.
     *
     * <p>The ones the generator emits get a sentence of their own. Anything else falls back to naming
     * the action, which is at least the name an author sees in the editor's own command list.
     */
    private String describeAction(final String agent, final String action, final String params) {
        if ("appendMessage".equalsIgnoreCase(action)) {
            return params.contains("role='user'")
                    ? "puts the answer into the conversation, so the person can see what they said."
                    : "puts a line into the conversation.";
        }
        if ("loadScreen".equalsIgnoreCase(action)) {
            return "shows a screen.";
        }
        return "asks " + quoted(agent) + " to " + action + ".";
    }

    private String readableCondition(final String conditionText) {
        final String trimmed = conditionText.trim();
        if (trimmed.contains("!=") && trimmed.contains("\"\"")) {
            return quoted(trimmed.substring(0, trimmed.indexOf("!=")).trim()) + " holds something";
        }
        return trimmed;
    }

    private String readableDuration(final int millis) {
        if (millis % 60000 == 0 && millis >= 60000) {
            final int minutes = millis / 60000;
            return minutes + (minutes == 1 ? " minute" : " minutes");
        }
        if (millis % 1000 == 0 && millis >= 1000) {
            final int seconds = millis / 1000;
            return seconds + (seconds == 1 ? " second" : " seconds");
        }
        return millis + " ms";
    }

    private String quoted(final String value) {
        // Escaped rather than literal: javac has no source encoding configured here, so a literal
        // typographic quote would depend on the building machine's platform charset.
        return "\u201c" + (value == null ? "" : value) + "\u201d";
    }

    // ---------------------------------------------------------------- resource requirements

    /**
     * Reports each requirement of the chosen pattern against what the project actually has.
     *
     * <p>Four outcomes, as in concept section 4a: {@code present} needs nothing, {@code creatable}
     * is filled with a placeholder, {@code author_only} is something only the author can supply such
     * as the wording of a scene, and {@code blocked} means no plugin on this deployment provides the
     * capability at all. A blocked requirement does not stop the flow from being generated. It is
     * recorded so the gap is visible rather than silently absent.
     */
    private JSONArray checkResources(final JSONObject catalogEntry,
                                     final JSONObject candidate,
                                     final JSONObject capabilities) {
        final JSONArray out = new JSONArray();
        final JSONArray requirements = catalogEntry == null
                ? null
                : catalogEntry.optJSONArray("resourceRequirements");
        if (requirements == null) {
            return out;
        }
        for (int i = 0; i < requirements.length(); i++) {
            final JSONObject requirement = requirements.optJSONObject(i);
            if (requirement == null) {
                continue;
            }
            out.put(checkRequirement(requirement, candidate, capabilities));
        }
        return out;
    }

    private JSONObject checkRequirement(final JSONObject requirement,
                                        final JSONObject candidate,
                                        final JSONObject capabilities) {
        final String kind = requirement.optString("kind", "");
        final JSONObject result = new JSONObject()
                .put("role", requirement.optString("role"))
                .put("kind", kind)
                .put("description", requirement.optString("description", ""));

        switch (kind) {
            case "scene":
                return fillSceneRequirement(result, requirement, candidate, capabilities);
            case "agent":
                return fillAgentRequirement(result, requirement, capabilities);
            case "variable":
                return fillVariableRequirement(result, requirement, candidate, capabilities);
            case "input":
                return fillInputRequirement(result, requirement, candidate, capabilities);
            case "capability":
                return fillCapabilityRequirement(result, requirement, candidate, capabilities);
            default:
                return result.put("status", statusForMissing(requirement))
                        .put("detail", "");
        }
    }

    private JSONObject fillSceneRequirement(final JSONObject result,
                                            final JSONObject requirement,
                                            final JSONObject candidate,
                                            final JSONObject capabilities) {
        final Set<String> referenced = scenesReferencedBy(candidate);
        final Set<String> existing = existingSceneNames(capabilities);
        final List<String> missing = new ArrayList<>();
        for (String scene : referenced) {
            if (!existing.contains(scene)) {
                missing.add(scene);
            }
        }
        if (referenced.isEmpty()) {
            return result.put("status", statusForMissing(requirement))
                    .put("detail", "No scene is named yet.");
        }
        if (missing.isEmpty()) {
            return result.put("status", "present")
                    .put("detail", "Uses " + joinQuoted(referenced) + ", which the script already has.");
        }
        return result.put("status", statusForMissing(requirement))
                .put("names", new JSONArray(missing))
                .put("detail", missing.size() == 1
                        ? "The scene " + joinQuoted(missing) + " does not exist yet. Write what the "
                                + "agent should say there."
                        : "The scenes " + joinQuoted(missing) + " do not exist yet. Write what the "
                                + "agent should say in each.");
    }

    /**
     * A requirement stated as something a deployment can do rather than as a named artifact.
     *
     * <p>Only agent readiness is answerable so far, and it is answered from what the flow actually
     * waits on rather than from the catalogue, so an agent whose plugin reports nothing is named as
     * the one that cannot be waited for instead of the requirement failing as a whole.
     */
    private JSONObject fillCapabilityRequirement(final JSONObject result,
                                                 final JSONObject requirement,
                                                 final JSONObject candidate,
                                                 final JSONObject capabilities) {
        if (!declaresCapability(requirement, "agent-readiness")) {
            return result.put("status", statusForMissing(requirement)).put("detail", "");
        }

        final List<String> waited = new ArrayList<>();
        final List<String> connectOnly = new ArrayList<>();
        final JSONObject metadata = candidate == null ? null : candidate.optJSONObject("metadata");
        JSONArray waitsFor = metadata == null ? null : metadata.optJSONArray("waitsFor");
        if (waitsFor == null && metadata != null && metadata.optJSONObject("readinessGate") != null) {
            waitsFor = metadata.getJSONObject("readinessGate").optJSONArray("waitsFor");
        }
        for (int i = 0; waitsFor != null && i < waitsFor.length(); i++) {
            final JSONObject entry = waitsFor.optJSONObject(i);
            if (entry == null) {
                continue;
            }
            waited.add(entry.optString("agent"));
            if (!entry.optBoolean("meansCanAct", true)) {
                connectOnly.add(entry.optString("agent"));
            }
        }

        if (waited.isEmpty()) {
            return result.put("status", "blocked")
                    .put("detail", "No plugin in this project reports when its agent is ready, so "
                            + "there is nothing the flow could wait for.");
        }
        if (!connectOnly.isEmpty()) {
            return result.put("status", "present")
                    .put("names", new JSONArray(waited))
                    .put("detail", "Waits for " + joinQuoted(waited) + ". " + joinQuoted(connectOnly)
                            + " reports only that it has connected, which can happen a moment before "
                            + "it is really usable.");
        }
        return result.put("status", "present")
                .put("names", new JSONArray(waited))
                .put("detail", "Waits for " + joinQuoted(waited) + ", each of which reports when it "
                        + "is able to act.");
    }

    private boolean declaresCapability(final JSONObject requirement, final String capability) {
        final JSONArray providedBy = requirement.optJSONArray("providedBy");
        for (int i = 0; providedBy != null && i < providedBy.length(); i++) {
            final JSONObject provider = providedBy.optJSONObject(i);
            if (provider != null && capability.equals(provider.optString("capability"))) {
                return true;
            }
        }
        return false;
    }

    private JSONObject fillAgentRequirement(final JSONObject result,
                                            final JSONObject requirement,
                                            final JSONObject capabilities) {
        final JSONObject project = capabilities.optJSONObject("project");
        final JSONArray agents = project == null ? null : project.optJSONArray("agents");
        if (agents != null && agents.length() > 0) {
            final List<String> names = new ArrayList<>();
            for (int i = 0; i < agents.length(); i++) {
                final JSONObject agent = agents.optJSONObject(i);
                if (agent != null && !agent.optString("name", "").isBlank()) {
                    names.add(agent.optString("name"));
                }
            }
            return result.put("status", "present")
                    .put("names", new JSONArray(names))
                    .put("detail", names.isEmpty()
                            ? "The project has an agent."
                            : "The project has " + joinQuoted(names) + ".");
        }
        // onMissing says "create", but nothing here creates an agent: it needs a device, which is a
        // project setting rather than anything the flow can express. Saying "I add this" and then
        // not doing it is worse than saying who has to.
        return result.put("status", "author_only")
                .put("detail", "This project has no agent yet. Add a device under the project's "
                        + "settings and give it an agent, then it can be told to do things.");
    }

    /**
     * Reports the variables the patch declares.
     *
     * <p>A pattern with several variable requirements gets the same answer for each of them. Nothing
     * in a generated patch says which declaration serves which role, and guessing by declaration
     * order would be a confident answer with nothing behind it. Naming every variable the flow gains
     * is both true and enough for an author to check.
     */
    private JSONObject fillVariableRequirement(final JSONObject result,
                                               final JSONObject requirement,
                                               final JSONObject candidate,
                                               final JSONObject capabilities) {
        final Set<String> defined = variablesDefinedBy(candidate);
        final Set<String> existing = existingVariableNames(capabilities);
        final List<String> created = new ArrayList<>();
        for (String variable : defined) {
            if (!existing.contains(variable)) {
                created.add(variable);
            }
        }
        if (created.isEmpty()) {
            return result.put("status", "present")
                    .put("detail", "The flow already has what this needs.");
        }
        return result.put("status", "creatable")
                .put("names", new JSONArray(created))
                .put("detail", "Creates " + joinQuoted(created) + " for you.");
    }

    /**
     * The requirement that makes the capability shape worth having: a screen control and a background
     * service both satisfy "something writes the answer", and the pattern does not care which.
     */
    private JSONObject fillInputRequirement(final JSONObject result,
                                            final JSONObject requirement,
                                            final JSONObject candidate,
                                            final JSONObject capabilities) {
        final Set<String> channels = channelsReadBy(candidate);
        final Set<String> writtenByScreens = screenWrittenVariables(capabilities);
        final Set<String> writtenByPlugins = pluginWrittenVariables(capabilities);

        for (String channel : channels) {
            if (writtenByScreens.contains(channel)) {
                return result.put("status", "present")
                        .put("detail", "A screen control already hands " + quoted(channel)
                                + " back to the flow.");
            }
            if (writtenByPlugins.contains(channel)) {
                return result.put("status", "present")
                        .put("detail", "A plugin already writes " + quoted(channel) + ".");
            }
        }

        final String channel = channels.isEmpty() ? "the answer" : channels.iterator().next();
        if (providesUserInput(capabilities)) {
            return result.put("status", "creatable")
                    .put("detail", "Nothing writes " + quoted(channel) + " yet. A screen with a "
                            + "control that sends it can be created from a template.");
        }
        // A capability the project lacks and a capability nobody shipped are different problems.
        // Telling an author with an empty project that nothing can ever answer would be both wrong
        // and the end of the road, when the plugin they need is one dialog away.
        final String installed = installedProviderOfUserInput();
        if (!installed.isEmpty()) {
            return result.put("status", "author_only")
                    .put("detail", "Nothing in this project can hand an answer back yet. Add the "
                            + quoted(installed) + " device under the project's settings, which brings "
                            + "a screen you can put a control on.");
        }
        return result.put("status", "blocked")
                .put("detail", "No plugin in this installation can hand an answer back to the flow, "
                        + "so " + quoted(channel) + " will stay empty and the flow will wait forever. "
                        + "The flow is still created, so the gap is visible rather than missing.");
    }

    private String statusForMissing(final JSONObject requirement) {
        switch (requirement.optString("onMissing", "")) {
            case "create":
                return "creatable";
            case "record":
                return "author_only";
            case "block":
                return "blocked";
            default:
                return "author_only";
        }
    }

    // ---------------------------------------------------------------- snapshot and candidate reads

    private Set<String> scenesReferencedBy(final JSONObject candidate) {
        final Set<String> scenes = new LinkedHashSet<>();
        forEachOperation(candidate, op -> {
            if (!"add_node_command".equals(op.optString("op"))) {
                return;
            }
            final Matcher matcher = PLAY_SCENE.matcher(op.optString("commandText", ""));
            if (matcher.find()) {
                scenes.add(matcher.group(2));
            }
        });
        return scenes;
    }

    private Set<String> variablesDefinedBy(final JSONObject candidate) {
        final Set<String> variables = new LinkedHashSet<>();
        forEachOperation(candidate, op -> {
            if (!"add_variable_definition".equals(op.optString("op"))) {
                return;
            }
            final String name = variableName(op);
            if (!name.isEmpty()) {
                variables.add(name);
            }
        });
        return variables;
    }

    /** The declared name sits in the nested definition, not on the operation itself. */
    private String variableName(final JSONObject op) {
        final JSONObject varDef = op.optJSONObject("varDef");
        return varDef == null ? "" : varDef.optString("name", "").trim();
    }

    /** Variables the generated flow waits on, which is where an answer has to arrive. */
    private Set<String> channelsReadBy(final JSONObject candidate) {
        final Set<String> channels = new LinkedHashSet<>();
        forEachOperation(candidate, op -> {
            final String edgeType = op.optString("edgeType", "");
            if (!"CEDGE".equals(edgeType) && !"IEDGE".equals(edgeType)) {
                return;
            }
            final JSONObject payload = op.optJSONObject("payload");
            final String condition = payload == null ? "" : payload.optString("conditionText", "");
            final Matcher matcher = Pattern.compile("[A-Za-z_][A-Za-z0-9_]*").matcher(condition);
            while (matcher.find()) {
                channels.add(matcher.group());
            }
        });
        return channels;
    }

    private void forEachOperation(final JSONObject candidate,
                                  final java.util.function.Consumer<JSONObject> visitor) {
        final JSONArray operations = candidate == null ? null : candidate.optJSONArray("operations");
        for (int i = 0; operations != null && i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op != null) {
                visitor.accept(op);
            }
        }
    }

    private Set<String> existingSceneNames(final JSONObject capabilities) {
        final Set<String> names = new LinkedHashSet<>();
        final JSONObject script = capabilities.optJSONObject("script");
        final JSONArray scenes = script == null ? null : script.optJSONArray("scenes");
        for (int i = 0; scenes != null && i < scenes.length(); i++) {
            final JSONObject scene = scenes.optJSONObject(i);
            if (scene != null && !scene.optString("name", "").isBlank()) {
                names.add(scene.optString("name"));
            }
        }
        return names;
    }

    private Set<String> existingVariableNames(final JSONObject capabilities) {
        final Set<String> names = new LinkedHashSet<>();
        final JSONObject flow = capabilities.optJSONObject("flow");
        final JSONArray variables = flow == null ? null : flow.optJSONArray("variables");
        for (int i = 0; variables != null && i < variables.length(); i++) {
            final JSONObject variable = variables.optJSONObject(i);
            if (variable != null && !variable.optString("name", "").isBlank()) {
                names.add(variable.optString("name"));
            }
        }
        return names;
    }

    private Set<String> screenWrittenVariables(final JSONObject capabilities) {
        final Set<String> names = new LinkedHashSet<>();
        final JSONObject screens = capabilities.optJSONObject("screens");
        final JSONArray list = screens == null ? null : screens.optJSONArray("screens");
        for (int i = 0; list != null && i < list.length(); i++) {
            final JSONObject screen = list.optJSONObject(i);
            addStrings(names, screen == null ? null : screen.optJSONArray("writesVariables"));
        }
        return names;
    }

    private Set<String> pluginWrittenVariables(final JSONObject capabilities) {
        final Set<String> names = new LinkedHashSet<>();
        final JSONObject project = capabilities.optJSONObject("project");
        final JSONArray plugins = project == null ? null : project.optJSONArray("plugins");
        for (int i = 0; plugins != null && i < plugins.length(); i++) {
            final JSONObject plugin = plugins.optJSONObject(i);
            addStrings(names, plugin == null ? null : plugin.optJSONArray("writesVariables"));
        }
        return names;
    }

    /** Whether any plugin the project uses can put a control in front of the person using the flow. */
    private boolean providesUserInput(final JSONObject capabilities) {
        final JSONObject project = capabilities.optJSONObject("project");
        final JSONArray plugins = project == null ? null : project.optJSONArray("plugins");
        for (int i = 0; plugins != null && i < plugins.length(); i++) {
            final JSONObject plugin = plugins.optJSONObject(i);
            if (plugin != null && offersUserInput(plugin.optString("className", ""))) {
                return true;
            }
        }
        return false;
    }

    /**
     * The same capability among the plugins this deployment carries but the project does not use.
     *
     * @return the display name of one such plugin, or empty when there is none
     */
    private String installedProviderOfUserInput() {
        for (Map.Entry<String, String> installed : mInstalledPlugins.get().entrySet()) {
            if (offersUserInput(installed.getKey())) {
                return installed.getValue();
            }
        }
        return "";
    }

    private boolean offersUserInput(final String className) {
        return className != null && className.toLowerCase(Locale.ROOT).contains("htmlgui");
    }

    private void addStrings(final Set<String> target, final JSONArray source) {
        for (int i = 0; source != null && i < source.length(); i++) {
            final String value = source.optString(i, "").trim();
            if (!value.isEmpty()) {
                target.add(value);
            }
        }
    }

    private String joinQuoted(final Iterable<String> values) {
        final List<String> quoted = new ArrayList<>();
        for (String value : values) {
            quoted.add(quoted(value));
        }
        quoted.sort(Comparator.naturalOrder());
        if (quoted.isEmpty()) {
            return "";
        }
        if (quoted.size() == 1) {
            return quoted.get(0);
        }
        return String.join(", ", quoted.subList(0, quoted.size() - 1))
                + " and " + quoted.get(quoted.size() - 1);
    }

    private void deleteRecursively(final Path root) {
        try (var paths = Files.walk(root)) {
            paths.sorted(Comparator.reverseOrder()).forEach(path -> {
                try {
                    Files.deleteIfExists(path);
                } catch (IOException ignored) {
                    // A leftover temporary file is harmless, and failing the request over one would
                    // throw away a proposal the author is waiting for.
                }
            });
        } catch (IOException ignored) {
            // Same reasoning: cleanup is best effort.
        }
    }
}
