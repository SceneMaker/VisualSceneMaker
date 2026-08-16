package de.dfki.vsm.sceneflow.ir;

import de.dfki.vsm.testsupport.TestRepoPaths;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SceneFlowIrTemplateLibraryTest {

    @Test
    void waitForEventTemplateIsSemanticallyValidAgainstDesignPatternsSnapshot() throws Exception {
        JSONObject snapshot = new JSONObject(Files.readString(TestRepoPaths.doc("capability-snapshot.designpatterns.json")));
        SceneFlowIrTemplateLibrary library = new SceneFlowIrTemplateLibrary();
        List<JSONObject> candidates = library.generateCandidates(
                "Wait until the user pressed the Okay button", snapshot);

        assertFalse(candidates.isEmpty());
        assertTrue("template-constrained-activity".equals(
                candidates.get(0).optJSONObject("metadata").optString("source")));
        assertTrue("constrained_activity_base".equals(
                candidates.get(0)
                        .optJSONObject("metadata")
                        .optJSONObject("interactiveDesignPattern")
                        .optString("selectedPatternId")));
        SemanticValidationResult result = new SceneFlowIrSemanticValidator().validate(candidates.get(0), snapshot);
        assertFalse(result.hasErrors(), "Expected first candidate to be semantically valid");
    }

    @Test
    void constrainedActivityTemplateUsesReminderLoopWhenPromptMentionsReminder() throws Exception {
        JSONObject snapshot = new JSONObject(Files.readString(TestRepoPaths.doc("capability-snapshot.designpatterns.json")));
        List<JSONObject> candidates = new SceneFlowIrTemplateLibrary().generateCandidates(
                "Wait until the user pressed the Okay button and remind every 5 seconds", snapshot);

        JSONObject constrained = candidates.stream()
                .filter(candidate -> "template-constrained-activity".equals(
                        candidate.optJSONObject("metadata").optString("source")))
                .findFirst()
                .orElse(null);
        assertNotNull(constrained);
        assertTrue("periodic_reminder_while_waiting".equals(
                constrained.optJSONObject("metadata")
                        .optJSONObject("interactiveDesignPattern")
                        .optString("selectedPatternId")));
        assertTrue("reminder".equals(
                constrained.optJSONObject("metadata")
                        .optJSONObject("interactiveDesignPattern")
                        .optJSONObject("resolvedMeta")
                        .optJSONObject("constrainedActivity")
                        .optString("kind")));
        String operationsText = constrained.getJSONArray("operations").toString();
        assertTrue(operationsText.contains("\"name\":\"Reminder\""));
        assertTrue(operationsText.contains("\"timeoutMs\":5000"));
    }

    @Test
    void plannedOnlyActivityKindsFallbackToImplementedBasePattern() throws Exception {
        JSONObject snapshot = new JSONObject(Files.readString(TestRepoPaths.doc("capability-snapshot.designpatterns.json")));
        List<JSONObject> candidates = new SceneFlowIrTemplateLibrary().generateCandidates(
                "Wait until the user pressed the Okay button while playing music", snapshot);

        JSONObject constrained = candidates.stream()
                .filter(candidate -> "template-constrained-activity".equals(
                        candidate.optJSONObject("metadata").optString("source")))
                .findFirst()
                .orElse(null);
        assertNotNull(constrained);
        assertTrue("constrained_activity_base".equals(
                constrained.optJSONObject("metadata")
                        .optJSONObject("interactiveDesignPattern")
                        .optString("selectedPatternId")));
        assertTrue("multimodal_activity".equals(
                constrained.optJSONObject("metadata")
                        .optJSONObject("interactiveDesignPattern")
                        .optJSONObject("resolvedMeta")
                        .optJSONObject("constrainedActivity")
                        .optString("kind")));
    }

    @Test
    void timeoutRetryTemplateIsGeneratedWhenPromptMatches() throws Exception {
        JSONObject snapshot = new JSONObject(Files.readString(TestRepoPaths.doc("capability-snapshot.designpatterns.json")));
        List<JSONObject> candidates = new SceneFlowIrTemplateLibrary().generateCandidates(
                "On timeout retry again until success", snapshot);

        boolean hasRetryTemplate = candidates.stream()
                .anyMatch(candidate -> "template-timeout-retry".equals(
                        candidate.optJSONObject("metadata").optString("source")));
        assertTrue(hasRetryTemplate);
    }

    @Test
    void commandOnConditionTemplateIsGeneratedWhenPromptMatches() throws Exception {
        JSONObject snapshot = new JSONObject(Files.readString(TestRepoPaths.doc("capability-snapshot.designpatterns.json")));
        List<JSONObject> candidates = new SceneFlowIrTemplateLibrary().generateCandidates(
                "If an event arrives then increment retry counter", snapshot);

        boolean hasConditionalTemplate = candidates.stream()
                .anyMatch(candidate -> "template-command-on-condition".equals(
                        candidate.optJSONObject("metadata").optString("source")));
        assertTrue(hasConditionalTemplate);
    }

    @Test
    void waitForMultipleButtonsGeneratesOneInterruptEdgePerButton() throws Exception {
        JSONObject snapshot = new JSONObject(Files.readString(TestRepoPaths.doc("capability-snapshot.designpatterns.json")));
        List<JSONObject> candidates = new SceneFlowIrTemplateLibrary().generateCandidates(
                "Wait until the user pressed the Okay button or the Cancel button", snapshot);

        JSONObject constrained = candidates.stream()
                .filter(candidate -> "template-constrained-activity".equals(
                        candidate.optJSONObject("metadata").optString("source")))
                .findFirst()
                .orElse(null);
        assertNotNull(constrained);

        var ops = constrained.getJSONArray("operations");
        List<String> conditions = java.util.stream.IntStream.range(0, ops.length())
                .mapToObj(ops::getJSONObject)
                .filter(op -> "create_edge".equals(op.optString("op", ""))
                        && "IEDGE".equals(op.optString("edgeType", "")))
                .map(op -> op.getJSONObject("payload").optString("conditionText", ""))
                .collect(Collectors.toList());

        assertEquals(2, conditions.size());
        assertTrue(conditions.contains("event == \"OkayButtonPressed\""));
        assertTrue(conditions.contains("event == \"CancelButtonPressed\""));

        SemanticValidationResult result = new SceneFlowIrSemanticValidator().validate(constrained, snapshot);
        assertFalse(result.hasErrors(), "Expected multi-button wait template to be semantically valid");
    }

    @Test
    void unresolvedButtonLabelIsReportedInConstraintResolutionMetadata() throws Exception {
        JSONObject snapshot = new JSONObject(Files.readString(TestRepoPaths.doc("capability-snapshot.designpatterns.json")));
        List<JSONObject> candidates = new SceneFlowIrTemplateLibrary().generateCandidates(
                "Wait until the user pressed the Foobar button",
                snapshot,
                ConstraintResolutionMode.PERMISSIVE);

        JSONObject constrained = candidates.stream()
                .filter(candidate -> "template-constrained-activity".equals(
                        candidate.optJSONObject("metadata").optString("source")))
                .findFirst()
                .orElse(null);
        assertNotNull(constrained);
        JSONObject resolution = constrained.optJSONObject("metadata").optJSONObject("constraintResolution");
        assertNotNull(resolution);
        assertTrue(resolution.optJSONArray("unresolvedLabels").toString().contains("Foobar"));
        assertTrue(resolution.optJSONArray("resolvedLabels").length() >= 1);
    }

    @Test
    void promptResolutionReportsAmbiguityAndConfidenceForMixedSignals() throws Exception {
        JSONObject snapshot = new JSONObject(Files.readString(TestRepoPaths.doc("capability-snapshot.designpatterns.json")));
        List<JSONObject> candidates = new SceneFlowIrTemplateLibrary().generateCandidates(
                "Wait and remind the user while showing pictures",
                snapshot,
                ConstraintResolutionMode.PERMISSIVE);

        JSONObject constrained = candidates.stream()
                .filter(candidate -> "template-constrained-activity".equals(
                        candidate.optJSONObject("metadata").optString("source")))
                .findFirst()
                .orElse(null);
        assertNotNull(constrained);
        JSONObject promptResolution = constrained.optJSONObject("metadata").optJSONObject("promptResolution");
        assertNotNull(promptResolution);
        assertTrue(promptResolution.optDouble("confidence", 1.0) < 0.8);
        assertTrue(promptResolution.optJSONArray("ambiguities").length() >= 1);
        assertEquals("reminder", promptResolution.optString("activityKind"));
    }

    @Test
    void sequenceSituationProducesAChainRatherThanAWaitSupernode() throws Exception {
        JSONObject snapshot = new JSONObject(
                Files.readString(TestRepoPaths.doc("capability-snapshot.designpatterns.json")));
        List<JSONObject> candidates = new SceneFlowIrTemplateLibrary().generateCandidates(
                "first greet the visitor, then explain the study, then ask for consent", snapshot);

        JSONObject sequence = candidateFromSource(candidates, "template-sequence");
        assertNotNull(sequence, "A described sequence must produce a sequence candidate");

        // Before this template existed the same situation matched no predicate and fell into the
        // unconditional fallback, which produced an unrelated constrained-activity wait supernode.
        assertNull(candidateFromSource(candidates, "template-constrained-activity"),
                "A sequence situation must no longer fall back to the wait template");

        assertEquals(3, sequence.getJSONObject("metadata").getInt("stepCount"));
        assertEquals(3, countOps(sequence, "create_node"));
        assertEquals(2, countOps(sequence, "create_edge"));
        assertEquals(3, countOps(sequence, "add_node_command"));
    }

    @Test
    void sequenceStepsAreChainedWithEpsilonEdgesInOrder() throws Exception {
        JSONObject snapshot = new JSONObject(
                Files.readString(TestRepoPaths.doc("capability-snapshot.designpatterns.json")));
        JSONObject sequence = candidateFromSource(new SceneFlowIrTemplateLibrary().generateCandidates(
                "first greet, then explain, then close", snapshot), "template-sequence");

        JSONArray ops = sequence.getJSONArray("operations");
        List<String> nodeIds = new java.util.ArrayList<>();
        List<String> hops = new java.util.ArrayList<>();
        boolean firstIsStartNode = false;
        for (int i = 0; i < ops.length(); i++) {
            JSONObject op = ops.getJSONObject(i);
            if ("create_node".equals(op.optString("op"))) {
                if (nodeIds.isEmpty()) {
                    firstIsStartNode = op.optBoolean("isStartNode", false);
                }
                nodeIds.add(op.getString("nodeId"));
                assertNotNull(op.optJSONObject("position"),
                        "Every step must carry a position: the compiler has no fallback layout");
            } else if ("create_edge".equals(op.optString("op"))) {
                assertEquals("EEDGE", op.getString("edgeType"));
                hops.add(op.getString("sourceNodeId") + ">" + op.getString("targetNodeId"));
            }
        }
        assertTrue(firstIsStartNode, "The sequence has to start somewhere");
        assertEquals(List.of(nodeIds.get(0) + ">" + nodeIds.get(1),
                nodeIds.get(1) + ">" + nodeIds.get(2)), hops);
    }

    @Test
    void sequenceReusesExistingScenesAndReportsTheOnesToAuthor() throws Exception {
        JSONObject snapshot = new JSONObject(
                Files.readString(TestRepoPaths.doc("capability-snapshot.designpatterns.json")));
        // DesignPatterns declares exactly one scene, "Welcome".
        JSONObject sequence = candidateFromSource(new SceneFlowIrTemplateLibrary().generateCandidates(
                "first welcome, then explain the study", snapshot), "template-sequence");

        List<String> commands = commandTexts(sequence);
        assertEquals("PlayScene(\"Welcome\")", commands.get(0),
                "An existing scene must be reused rather than a near-duplicate invented");
        assertTrue(commands.get(1).startsWith("PlayScene(\""));

        List<Object> toAuthor = sequence.getJSONObject("metadata")
                .getJSONArray("scenesToAuthor").toList();
        assertEquals(1, toAuthor.size(), "Only the second scene is missing, was: " + toAuthor);
    }

    @Test
    void sceneNamesAreDoubleQuotedBecauseSingleQuotesSilentlyBecomeVariables() throws Exception {
        JSONObject snapshot = new JSONObject(
                Files.readString(TestRepoPaths.doc("capability-snapshot.designpatterns.json")));
        JSONObject sequence = candidateFromSource(new SceneFlowIrTemplateLibrary().generateCandidates(
                "first greet, then close", snapshot), "template-sequence");

        for (String command : commandTexts(sequence)) {
            assertFalse(command.contains("'"), "Command text must not use single quotes: " + command);
            assertTrue(command.contains("\""), "Scene name must be double quoted: " + command);
        }
    }

    @Test
    void prosePassingNoStepsIsNotTreatedAsASequence() throws Exception {
        JSONObject snapshot = new JSONObject(
                Files.readString(TestRepoPaths.doc("capability-snapshot.designpatterns.json")));
        List<JSONObject> candidates = new SceneFlowIrTemplateLibrary().generateCandidates(
                "the agent should greet the visitor", snapshot);
        assertNull(candidateFromSource(candidates, "template-sequence"),
                "A single instruction is not a sequence");
    }

    @Test
    void generatedStepIdsDoNotCollideWithNodesAlreadyInTheProject() throws Exception {
        JSONObject snapshot = new JSONObject(
                Files.readString(TestRepoPaths.doc("capability-snapshot.designpatterns.json")));
        JSONObject sequence = candidateFromSource(new SceneFlowIrTemplateLibrary().generateCandidates(
                "first greet, then explain, then close", snapshot), "template-sequence");

        java.util.Set<String> existing = new java.util.HashSet<>();
        JSONArray nodes = snapshot.getJSONObject("flow").getJSONArray("nodes");
        for (int i = 0; i < nodes.length(); i++) {
            existing.add(nodes.getJSONObject(i).getString("id"));
        }
        JSONArray ops = sequence.getJSONArray("operations");
        for (int i = 0; i < ops.length(); i++) {
            JSONObject op = ops.getJSONObject(i);
            if ("create_node".equals(op.optString("op"))) {
                assertFalse(existing.contains(op.getString("nodeId")),
                        "Generated id collides with an existing node: " + op.getString("nodeId"));
            }
        }
    }

    private JSONObject candidateFromSource(final List<JSONObject> candidates, final String source) {
        for (JSONObject candidate : candidates) {
            if (source.equals(candidate.optJSONObject("metadata").optString("source"))) {
                return candidate;
            }
        }
        return null;
    }

    private int countOps(final JSONObject candidate, final String op) {
        int count = 0;
        JSONArray ops = candidate.getJSONArray("operations");
        for (int i = 0; i < ops.length(); i++) {
            if (op.equals(ops.getJSONObject(i).optString("op"))) {
                count++;
            }
        }
        return count;
    }

    private List<String> commandTexts(final JSONObject candidate) {
        List<String> out = new java.util.ArrayList<>();
        JSONArray ops = candidate.getJSONArray("operations");
        for (int i = 0; i < ops.length(); i++) {
            JSONObject op = ops.getJSONObject(i);
            if ("add_node_command".equals(op.optString("op"))) {
                out.add(op.getString("commandText"));
            }
        }
        return out;
    }

    /**
     * The library used to answer anything it did not recognise with a constrained-activity wait
     * template, which told the caller a request had been understood when it had not been.
     */
    @Test
    void unrecognisedSituationProducesNoCandidateAtAll() throws Exception {
        JSONObject snapshot = new JSONObject(
                Files.readString(TestRepoPaths.doc("capability-snapshot.designpatterns.json")));
        List<JSONObject> candidates = new SceneFlowIrTemplateLibrary()
                .generateCandidates("make the avatar happy", snapshot);

        assertTrue(candidates.isEmpty(),
                "Expected an honest miss, got: " + candidates.stream()
                        .map(c -> c.getJSONObject("metadata").optString("source"))
                        .collect(Collectors.joining(", ")));
    }

    @Test
    void anEmptySituationIsAMissRatherThanAWaitTemplate() throws Exception {
        JSONObject snapshot = new JSONObject(
                Files.readString(TestRepoPaths.doc("capability-snapshot.designpatterns.json")));
        assertTrue(new SceneFlowIrTemplateLibrary().generateCandidates("", snapshot).isEmpty());
    }

    @Test
    void recognisedSituationHintsCoverEveryPredicate() {
        List<String> hints = SceneFlowIrTemplateLibrary.recognisedSituationHints();
        assertEquals(6, hints.size(),
                "One hint per predicate: readiness, wait, retry, condition, sequence, ask-and-wait");
        String all = String.join(" ", hints).toLowerCase(java.util.Locale.ROOT);
        for (String marker : new String[] {"waiting", "retry", "if", "first", "asking", "ready"}) {
            assertTrue(all.contains(marker), "Hints must mention " + marker + ": " + all);
        }
    }

    // ---- catalogue-driven selection ----

    private Path catalogWith(final String patternLibraryEntries) throws Exception {
        Path catalog = Files.createTempFile("pattern-catalog", ".json");
        Files.writeString(catalog, "{\"catalogVersion\":\"test\",\"patternLibrary\":["
                + patternLibraryEntries + "]}");
        return catalog;
    }

    private static String entry(final String id, final String status, final String supportsMeta,
                                final String fallbackTo) {
        return "{\"id\":\"" + id + "\",\"status\":\"" + status + "\""
                + (supportsMeta == null ? "" : ",\"supportsMeta\":" + supportsMeta)
                + (fallbackTo == null ? "" : ",\"fallbackTo\":\"" + fallbackTo + "\"")
                + "}";
    }

    /** The pattern constraining more of what the template resolved is the better description of it. */
    @Test
    void moreSpecificPatternWinsWhenBothMatch() throws Exception {
        Path catalog = catalogWith(String.join(",",
                entry("broad", "implemented", "{\"a\":[\"1\"]}", null),
                entry("specific", "implemented", "{\"a\":[\"1\"],\"b\":[\"2\"]}", null)));

        var selection = new SceneFlowIrTemplateLibrary(catalog)
                .selectPattern(java.util.Map.of("a", "1", "b", "2"));
        assertEquals("specific", selection.patternId());
    }

    /** A criterion the template did not resolve says nothing, so it must not disqualify a pattern. */
    @Test
    void criterionForAnUnresolvedKeyIsIgnored() throws Exception {
        Path catalog = catalogWith(entry("p", "implemented", "{\"a\":[\"1\"],\"unknown\":[\"x\"]}", null));

        var selection = new SceneFlowIrTemplateLibrary(catalog)
                .selectPattern(java.util.Map.of("a", "1"));
        assertEquals("p", selection.patternId());
    }

    /** A resolved value outside the declared list is a genuine mismatch, unlike an absent key. */
    @Test
    void resolvedValueOutsideTheDeclaredListRejectsThePattern() throws Exception {
        Path catalog = catalogWith(entry("p", "implemented", "{\"a\":[\"1\"]}", null));

        var selection = new SceneFlowIrTemplateLibrary(catalog)
                .selectPattern(java.util.Map.of("a", "999"));
        assertEquals("", selection.patternId(), selection.reason());
    }

    /** Without this, any pattern would match anything by simply constraining an unrelated axis. */
    @Test
    void patternMatchingNoCriterionAtAllIsNotSelected() throws Exception {
        Path catalog = catalogWith(entry("unrelated", "implemented", "{\"other\":[\"x\"]}", null));

        var selection = new SceneFlowIrTemplateLibrary(catalog)
                .selectPattern(java.util.Map.of("a", "1"));
        assertEquals("", selection.patternId(), selection.reason());
    }

    @Test
    void implementedPatternBeatsAMoreSpecificPlannedOne() throws Exception {
        Path catalog = catalogWith(String.join(",",
                entry("built", "implemented", "{\"a\":[\"1\"]}", null),
                entry("notBuiltYet", "planned", "{\"a\":[\"1\"],\"b\":[\"2\"]}", null)));

        var selection = new SceneFlowIrTemplateLibrary(catalog)
                .selectPattern(java.util.Map.of("a", "1", "b", "2"));
        assertEquals("built", selection.patternId());
    }

    /** Which implemented pattern to land on is the catalogue's decision, not the code's. */
    @Test
    void plannedOnlyMatchFollowsTheCatalogueDeclaredFallback() throws Exception {
        Path catalog = catalogWith(String.join(",",
                entry("base", "implemented", "{\"a\":[\"other\"]}", null),
                entry("notBuiltYet", "planned", "{\"a\":[\"1\"]}", "base")));

        var selection = new SceneFlowIrTemplateLibrary(catalog)
                .selectPattern(java.util.Map.of("a", "1"));
        assertEquals("base", selection.patternId());
        assertTrue(selection.reason().contains("planned"), selection.reason());
    }

    @Test
    void plannedMatchWithoutAFallbackIsReportedAsItself() throws Exception {
        Path catalog = catalogWith(entry("notBuiltYet", "planned", "{\"a\":[\"1\"]}", null));

        var selection = new SceneFlowIrTemplateLibrary(catalog)
                .selectPattern(java.util.Map.of("a", "1"));
        assertEquals("notBuiltYet", selection.patternId());
        assertTrue(selection.reason().contains("no implemented fallback"), selection.reason());
    }

    /** A scalar documents where a value comes from; only arrays are things to match against. */
    @Test
    void scalarSupportsMetaValuesAreNotTreatedAsCriteria() throws Exception {
        Path catalog = catalogWith(
                entry("p", "implemented", "{\"a\":[\"1\"],\"note\":\"parsed_from_text_or_default\"}", null));

        var selection = new SceneFlowIrTemplateLibrary(catalog)
                .selectPattern(java.util.Map.of("a", "1", "note", "anything"));
        assertEquals("p", selection.patternId(),
                "A scalar must neither constrain nor reject: " + selection.reason());
    }

    @Test
    void sequenceCandidateIsAttributedToTheSequenceCatalogueEntry() throws Exception {
        JSONObject snapshot = new JSONObject(
                Files.readString(TestRepoPaths.doc("capability-snapshot.designpatterns.json")));
        JSONObject sequence = candidateFromSource(new SceneFlowIrTemplateLibrary().generateCandidates(
                "first greet, then explain, then close", snapshot), "template-sequence");

        assertEquals("sequence", sequence.getJSONObject("metadata")
                .getJSONObject("interactiveDesignPattern").getString("selectedPatternId"),
                "The sequence template must be attributed through the catalogue like every other");
    }

    // ---- ask and wait ----

    private JSONObject askAndWaitFor(final String situation, final String snapshotPath) throws Exception {
        JSONObject snapshot = new JSONObject(Files.readString(Path.of(snapshotPath)));
        return candidateFromSource(
                new SceneFlowIrTemplateLibrary().generateCandidates(situation, snapshot),
                "template-ask-and-wait");
    }

    @Test
    void askingAndWaitingProducesTheAskWaitStoreShape() throws Exception {
        JSONObject candidate = askAndWaitFor("Ask the person for their name and wait for the reply",
                TestRepoPaths.doc("capability-snapshot.designpatterns.json").toString());
        assertNotNull(candidate);

        assertEquals(3, countOps(candidate, "create_node"));
        assertEquals(3, countOps(candidate, "create_edge"));

        // The wait node is the one with no commands, and it must stay that way: a guard is only
        // evaluated once a node's commands have finished.
        JSONArray ops = candidate.getJSONArray("operations");
        String waitNode = null;
        for (int i = 0; i < ops.length(); i++) {
            JSONObject op = ops.getJSONObject(i);
            if ("create_edge".equals(op.optString("op")) && "TEDGE".equals(op.optString("edgeType"))
                    && op.getString("sourceNodeId").equals(op.getString("targetNodeId"))) {
                waitNode = op.getString("sourceNodeId");
            }
        }
        assertNotNull(waitNode, "The wait node polls with a self timeout edge");
        for (int i = 0; i < ops.length(); i++) {
            JSONObject op = ops.getJSONObject(i);
            if ("add_node_command".equals(op.optString("op"))) {
                assertFalse(waitNode.equals(op.getString("nodeId")),
                        "The wait node must carry no commands");
            }
        }
    }

    /** A reset that ran on every poll would discard the answer before it could be seen. */
    @Test
    void theChannelIsClearedWhereTheQuestionIsAskedNotWhereItWaits() throws Exception {
        JSONObject candidate = askAndWaitFor("Ask for the name and wait for an answer",
                TestRepoPaths.doc("capability-snapshot.designpatterns.json").toString());
        String channel = candidate.getJSONObject("metadata").getString("answerChannel");

        JSONArray ops = candidate.getJSONArray("operations");
        String scenePlayingNode = null;
        String clearingNode = null;
        for (int i = 0; i < ops.length(); i++) {
            JSONObject op = ops.getJSONObject(i);
            if (!"add_node_command".equals(op.optString("op"))) {
                continue;
            }
            String text = op.getString("commandText");
            if (text.startsWith("PlayScene(")) {
                scenePlayingNode = op.getString("nodeId");
            } else if (text.equals(channel + " = \"\"")) {
                clearingNode = op.getString("nodeId");
            }
        }
        assertNotNull(clearingNode, "The channel has to be cleared somewhere");
        assertEquals(scenePlayingNode, clearingNode,
                "The reset belongs in the node that asks, not the one that waits");
    }

    /** The channel is shared, so an answer not copied out is lost when the next question clears it. */
    @Test
    void theAnswerIsCopiedIntoAVariableOfItsOwn() throws Exception {
        JSONObject candidate = askAndWaitFor("Ask for the name and wait for an answer",
                TestRepoPaths.doc("capability-snapshot.designpatterns.json").toString());
        JSONObject metadata = candidate.getJSONObject("metadata");
        String channel = metadata.getString("answerChannel");
        String store = metadata.getString("answerStore");

        assertFalse(channel.equals(store), "Copying the channel onto itself would keep nothing");
        assertTrue(commandTexts(candidate).contains(store + " = " + channel));
    }

    /** Reuse what the project already has rather than inventing a near-duplicate beside it. */
    @Test
    void anExistingSceneAndChannelAreReusedRatherThanRecreated() throws Exception {
        JSONObject snapshot = new JSONObject(Files.readString(TestRepoPaths.doc("capability-snapshot.intakeinterview.json")));
        JSONObject candidate = candidateFromSource(new SceneFlowIrTemplateLibrary()
                .generateCandidates("Ask the visitor for their name and wait for the answer", snapshot),
                "template-ask-and-wait");
        assertNotNull(candidate);

        JSONObject metadata = candidate.getJSONObject("metadata");
        assertEquals("ask_name", metadata.getString("questionScene"),
                "IntakeInterview already has an ask_name scene");
        assertEquals(0, metadata.getJSONArray("scenesToAuthor").length(),
                "Nothing to author when the scene exists");
        assertEquals("user_input", metadata.getString("answerChannel"),
                "The project already has a channel a screen fills");

        for (String created : createdVariables(candidate)) {
            assertFalse("user_input".equals(created), "An existing channel must not be redeclared");
        }
    }

    /** With nothing to reuse, everything creatable is created and the rest is recorded. */
    @Test
    void withNothingToReuseTheChannelIsCreatedAndTheSceneIsRecorded() throws Exception {
        JSONObject candidate = askAndWaitFor("Ask the person for their name and wait for the reply",
                TestRepoPaths.doc("capability-snapshot.designpatterns.json").toString());

        assertTrue(createdVariables(candidate).contains("user_input"),
                "The channel is creatable, so it is created");
        assertEquals(1, candidate.getJSONObject("metadata").getJSONArray("scenesToAuthor").length(),
                "The scene is the author's to write, so it is recorded rather than stubbed");
        assertTrue(candidate.getJSONArray("assumptions").toString().contains("sendsVar"),
                "Nothing writes the channel yet, and the author has to be told so");
    }

    /** A bare wait is the constrained-activity pattern; asking makes it a different pattern. */
    @Test
    void anAskingSituationDoesNotAlsoProduceABareWaitTemplate() throws Exception {
        JSONObject snapshot = new JSONObject(
                Files.readString(TestRepoPaths.doc("capability-snapshot.designpatterns.json")));
        List<JSONObject> candidates = new SceneFlowIrTemplateLibrary()
                .generateCandidates("Ask the person for their name and wait for the reply", snapshot);

        assertNotNull(candidateFromSource(candidates, "template-ask-and-wait"));
        assertNull(candidateFromSource(candidates, "template-constrained-activity"),
                "A bare wait supernode would ask nothing and store nothing");
    }

    @Test
    void aPlainWaitStillProducesTheConstrainedActivityTemplate() throws Exception {
        JSONObject snapshot = new JSONObject(
                Files.readString(TestRepoPaths.doc("capability-snapshot.designpatterns.json")));
        List<JSONObject> candidates = new SceneFlowIrTemplateLibrary()
                .generateCandidates("Wait until the user pressed the Okay button", snapshot);

        assertNotNull(candidateFromSource(candidates, "template-constrained-activity"));
        assertNull(candidateFromSource(candidates, "template-ask-and-wait"));
    }

    @Test
    void thePollIntervalCanBeTakenFromTheSituation() throws Exception {
        JSONObject candidate = askAndWaitFor("Ask for the name and wait for an answer, checking every 2 seconds",
                TestRepoPaths.doc("capability-snapshot.designpatterns.json").toString());
        JSONArray ops = candidate.getJSONArray("operations");
        int timeout = -1;
        for (int i = 0; i < ops.length(); i++) {
            JSONObject op = ops.getJSONObject(i);
            if ("create_edge".equals(op.optString("op")) && "TEDGE".equals(op.optString("edgeType"))) {
                timeout = op.getJSONObject("payload").getInt("timeoutMs");
            }
        }
        assertEquals(2000, timeout);
    }

    private java.util.List<String> createdVariables(final JSONObject candidate) {
        java.util.List<String> out = new java.util.ArrayList<>();
        JSONArray ops = candidate.getJSONArray("operations");
        for (int i = 0; i < ops.length(); i++) {
            JSONObject op = ops.getJSONObject(i);
            if ("add_variable_definition".equals(op.optString("op"))) {
                out.add(op.getJSONObject("varDef").getString("name"));
            }
        }
        return out;
    }
}
