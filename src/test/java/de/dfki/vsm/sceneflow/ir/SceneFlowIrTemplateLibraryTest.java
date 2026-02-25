package de.dfki.vsm.sceneflow.ir;

import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SceneFlowIrTemplateLibraryTest {

    @Test
    void waitForEventTemplateIsSemanticallyValidAgainstDesignPatternsSnapshot() throws Exception {
        JSONObject snapshot = new JSONObject(Files.readString(Path.of("doc/capability-snapshot.designpatterns.json")));
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
        JSONObject snapshot = new JSONObject(Files.readString(Path.of("doc/capability-snapshot.designpatterns.json")));
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
        JSONObject snapshot = new JSONObject(Files.readString(Path.of("doc/capability-snapshot.designpatterns.json")));
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
        JSONObject snapshot = new JSONObject(Files.readString(Path.of("doc/capability-snapshot.designpatterns.json")));
        List<JSONObject> candidates = new SceneFlowIrTemplateLibrary().generateCandidates(
                "On timeout retry again until success", snapshot);

        boolean hasRetryTemplate = candidates.stream()
                .anyMatch(candidate -> "template-timeout-retry".equals(
                        candidate.optJSONObject("metadata").optString("source")));
        assertTrue(hasRetryTemplate);
    }

    @Test
    void commandOnConditionTemplateIsGeneratedWhenPromptMatches() throws Exception {
        JSONObject snapshot = new JSONObject(Files.readString(Path.of("doc/capability-snapshot.designpatterns.json")));
        List<JSONObject> candidates = new SceneFlowIrTemplateLibrary().generateCandidates(
                "If an event arrives then increment retry counter", snapshot);

        boolean hasConditionalTemplate = candidates.stream()
                .anyMatch(candidate -> "template-command-on-condition".equals(
                        candidate.optJSONObject("metadata").optString("source")));
        assertTrue(hasConditionalTemplate);
    }

    @Test
    void waitForMultipleButtonsGeneratesOneInterruptEdgePerButton() throws Exception {
        JSONObject snapshot = new JSONObject(Files.readString(Path.of("doc/capability-snapshot.designpatterns.json")));
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
}
