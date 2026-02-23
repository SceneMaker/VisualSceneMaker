package de.dfki.vsm.sceneflow.ir;

import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SceneFlowIrTemplateLibraryTest {

    @Test
    void waitForEventTemplateIsSemanticallyValidAgainstDesignPatternsSnapshot() throws Exception {
        JSONObject snapshot = new JSONObject(Files.readString(Path.of("doc/capability-snapshot.designpatterns.json")));
        SceneFlowIrTemplateLibrary library = new SceneFlowIrTemplateLibrary();
        List<JSONObject> candidates = library.generateCandidates(
                "Wait until the user pressed the Okay button", snapshot);

        assertFalse(candidates.isEmpty());
        SemanticValidationResult result = new SceneFlowIrSemanticValidator().validate(candidates.get(0), snapshot);
        assertFalse(result.hasErrors(), "Expected first candidate to be semantically valid");
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
}

