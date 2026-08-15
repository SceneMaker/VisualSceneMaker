package de.dfki.vsm.web;

import de.dfki.vsm.testsupport.TestRepoPaths;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FlowAssistantServiceTest {

    /** Words from the generator's own vocabulary, which an author has no way to act on. */
    private static final String[] INTERNAL_VOCABULARY = {
            "create_node", "create_edge", "add_node_command", "add_variable_definition",
            "eedge", "tedge", "cedge", "iedge", "supernode", "operations", "nodeid"
    };

    private JSONObject snapshot() throws Exception {
        return new JSONObject(Files.readString(
                TestRepoPaths.doc("capability-snapshot.designpatterns.json"), StandardCharsets.UTF_8));
    }

    private String baseFlow() throws Exception {
        return Files.readString(TestRepoPaths.doc("DesignPatterns/sceneflow.xml"), StandardCharsets.UTF_8);
    }

    @Test
    void catalogueNamesPatternsInTheAuthorsWords() {
        JSONObject catalogue = new FlowAssistantService().catalogue();
        JSONArray patterns = catalogue.getJSONArray("patterns");
        assertTrue(patterns.length() >= 4);

        JSONObject sequence = null;
        for (int i = 0; i < patterns.length(); i++) {
            if ("sequence".equals(patterns.getJSONObject(i).optString("id"))) {
                sequence = patterns.getJSONObject(i);
            }
        }
        assertNotNull(sequence, "The catalogue has to offer the sequence pattern");
        assertEquals("Fixed sequence", sequence.optString("label"));
        assertTrue(sequence.optBoolean("available"));
        assertFalse(sequence.optString("description").isBlank());
        assertTrue(sequence.getJSONArray("questions").length() >= 1,
                "An implemented pattern carries the questions the assistant asks");
    }

    @Test
    void catalogueMarksPlannedPatternsAsNotYetAvailable() {
        JSONArray patterns = new FlowAssistantService().catalogue().getJSONArray("patterns");
        boolean sawPlanned = false;
        for (int i = 0; i < patterns.length(); i++) {
            if (!patterns.getJSONObject(i).optBoolean("available")) {
                sawPlanned = true;
            }
        }
        assertTrue(sawPlanned, "Patterns that are not built yet stay visible, marked as unavailable");
    }

    @Test
    void describedSequenceBecomesAProposalTheAuthorCanRead() throws Exception {
        FlowAssistantService service = new FlowAssistantService();
        FlowAssistantService.Proposal proposal = service.propose(
                "p1", snapshot(), baseFlow(), "first greet, then explain, then close");

        assertEquals("ready", proposal.status());
        assertTrue(proposal.isApplicable());

        JSONObject view = proposal.authorView();
        assertEquals("sequence", view.getJSONObject("pattern").getString("id"));
        assertEquals("Fixed sequence", view.getJSONObject("pattern").getString("label"));

        JSONArray changes = view.getJSONArray("changes");
        assertTrue(changes.length() >= 5, "Three steps and the edges between them");
        assertTrue(changes.toString().contains("Adds a step called"));
        assertTrue(changes.toString().contains("has finished"),
                "A sequence is worth explaining as one step waiting for the previous one");
    }

    @Test
    void aProposalNeverExposesTheGeneratorsVocabulary() throws Exception {
        FlowAssistantService service = new FlowAssistantService();
        String view = service.propose("p1", snapshot(), baseFlow(),
                "first greet, then explain, then close").authorView().toString().toLowerCase(Locale.ROOT);

        for (String word : INTERNAL_VOCABULARY) {
            assertFalse(view.contains(word),
                    "The author-facing proposal leaked the internal term \"" + word + "\"");
        }
    }

    @Test
    void scenesThatDoNotExistYetAreLeftToTheAuthor() throws Exception {
        JSONObject view = new FlowAssistantService()
                .propose("p1", snapshot(), baseFlow(), "first greet, then explain, then close")
                .authorView();

        JSONObject sceneRequirement = requirement(view, "step-content");
        assertNotNull(sceneRequirement, "A sequence needs something to play at each step");
        assertEquals("author_only", sceneRequirement.getString("status"),
                "Only the author can write what an agent says");
        assertTrue(sceneRequirement.getJSONArray("names").length() >= 1);
        assertTrue(sceneRequirement.getString("detail").contains("does not exist")
                || sceneRequirement.getString("detail").contains("do not exist"));
    }

    @Test
    void anAgentTheProjectAlreadyHasIsReportedAsPresent() throws Exception {
        JSONObject view = new FlowAssistantService()
                .propose("p1", snapshot(), baseFlow(), "first greet, then explain, then close")
                .authorView();

        JSONObject speaker = requirement(view, "speaker");
        assertNotNull(speaker);
        assertEquals("present", speaker.getString("status"));
    }

    /**
     * The point of capability-shaped requirements: a deployment with no plugin that can hand an
     * answer back still gets its flow, with the gap named rather than silently missing.
     */
    @Test
    void waitingForAnAnswerNobodyCanGiveIsReportedAsBlockedButStillBuilt() throws Exception {
        FlowAssistantService.Proposal proposal = new FlowAssistantService().propose(
                "p1", snapshot(), baseFlow(), "Ask the person for their name and wait for the reply");

        assertEquals("ready", proposal.status());
        assertTrue(proposal.isApplicable(), "A blocked requirement does not stop the flow");

        JSONObject answerSource = requirement(proposal.authorView(), "answer-source");
        assertNotNull(answerSource);
        assertEquals("blocked", answerSource.getString("status"),
                "The DesignPatterns project has only the timer plugin, which cannot take input");
        assertTrue(answerSource.getString("detail").contains("wait forever"));
    }

    @Test
    void variablesThePatternNeedsAreCreatedRatherThanAskedFor() throws Exception {
        JSONObject view = new FlowAssistantService().propose(
                "p1", snapshot(), baseFlow(),
                "Ask the person for their name and wait for the reply").authorView();

        JSONObject store = requirement(view, "answer-store");
        assertNotNull(store);
        assertEquals("creatable", store.getString("status"));
        assertTrue(store.getString("detail").startsWith("Creates "));
    }

    @Test
    void aSituationNoPatternRecognisesIsSaidSoRatherThanReportedAsAFailure() throws Exception {
        FlowAssistantService.Proposal proposal = new FlowAssistantService()
                .propose("p1", snapshot(), baseFlow(), "make the avatar happy");

        assertEquals("no_pattern_matched", proposal.status());
        assertFalse(proposal.isApplicable());
        assertNull(proposal.sceneFlowXml());
        assertTrue(proposal.authorView().getString("message").contains("No pattern"));
        assertTrue(proposal.authorView().getJSONArray("recognisedSituations").length() >= 1,
                "Telling an author what is recognised is the only actionable part of a non-match");
    }

    @Test
    void theCompiledFlowKeepsWhatWasAlreadyThereAndAddsTheNewSteps() throws Exception {
        FlowAssistantService.Proposal proposal = new FlowAssistantService().propose(
                "p1", snapshot(), baseFlow(), "first greet, then explain, then close");

        String merged = proposal.sceneFlowXml();
        assertNotNull(merged);
        assertTrue(merged.contains("<SceneFlow"));
        assertTrue(merged.toLowerCase(Locale.ROOT).contains("greet"),
                "The described steps have to appear in the merged flow");
    }

    @Test
    void aProposalBelongsToTheProjectItWasGeneratedFor() throws Exception {
        FlowAssistantService service = new FlowAssistantService();
        FlowAssistantService.Proposal proposal = service.propose(
                "p1", snapshot(), baseFlow(), "first greet, then explain, then close");

        assertNotNull(service.take(proposal.id(), "p1"));
        assertNull(service.take(proposal.id(), "p2"),
                "A proposal must not be applicable to a different project");

        service.discard(proposal.id());
        assertNull(service.take(proposal.id(), "p1"));
    }

    private JSONObject requirement(final JSONObject view, final String role) {
        JSONArray resources = view.optJSONArray("resources");
        for (int i = 0; resources != null && i < resources.length(); i++) {
            if (role.equals(resources.getJSONObject(i).optString("role"))) {
                return resources.getJSONObject(i);
            }
        }
        return null;
    }
}
