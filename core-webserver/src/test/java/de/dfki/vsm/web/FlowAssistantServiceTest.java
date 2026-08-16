package de.dfki.vsm.web;

import de.dfki.vsm.sceneflow.ir.SceneFlowIrLlmCandidateProvider;
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

    /**
     * A plugin nobody shipped and a plugin the project has not added are different problems.
     *
     * <p>Telling someone with an empty project that nothing can ever answer is both wrong and the
     * end of the road, when the plugin they need is one dialog away. This is what a newcomer meets
     * first, so it has to point somewhere.
     */
    @Test
    void aCapabilityThatIsInstalledButUnusedSaysWhatToAddRatherThanThatItIsHopeless() throws Exception {
        JSONObject view = new FlowAssistantService()
                .withInstalledPlugins(() -> java.util.Map.of(
                        "de.dfki.vsm.xtension.responsiveweb.HtmlGuiWsExecutor", "HTML GUI"))
                .propose("p1", snapshot(), baseFlow(),
                        "Ask the person for their name and wait for the reply")
                .authorView();

        JSONObject answerSource = requirement(view, "answer-source");
        assertEquals("author_only", answerSource.getString("status"));
        assertTrue(answerSource.getString("detail").contains("HTML GUI"),
                "The author has to be told which device to add: " + answerSource.getString("detail"));
        assertFalse(answerSource.getString("detail").contains("wait forever"));
    }

    /** An agent needs a device, which no flow can express, so the assistant must not claim it will. */
    @Test
    void anEmptyProjectIsToldWhoHasToAddTheAgent() throws Exception {
        JSONObject view = new FlowAssistantService()
                .propose("p1", snapshot(), baseFlow(), "first greet, then explain, then close")
                .authorView();

        JSONObject speaker = requirement(view, "speaker");
        assertEquals("present", speaker.getString("status"),
                "DesignPatterns declares a timer agent, so this one is met");

        JSONObject noAgents = new JSONObject(snapshot().toString());
        noAgents.getJSONObject("project").put("agents", new JSONArray());
        JSONObject withoutAgents = new FlowAssistantService()
                .propose("p1", noAgents, baseFlow(), "first greet, then explain, then close")
                .authorView();

        JSONObject missing = requirement(withoutAgents, "speaker");
        assertEquals("author_only", missing.getString("status"),
                "Nothing here creates an agent, so it must not be reported as something I add");
        assertTrue(missing.getString("detail").contains("Add a device"));
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

    /**
     * The wait for the agents is described as one thing, in the words of what it does.
     *
     * <p>Its parts read as noise separately: a supernode, an empty node, a loop onto itself and a
     * condition over variables an author has never seen. All four are the generator's vocabulary,
     * which is exactly what must not reach a proposal.
     */
    @Test
    void theWaitForTheAgentsIsSaidAsOneThingInPlainWords() throws Exception {
        JSONObject view = new FlowAssistantService()
                .propose("p1", twoAgentSnapshot(), twoAgentFlow(),
                        "first greet, then explain, then close")
                .authorView();

        JSONObject gate = view.getJSONObject("readinessGate");
        assertTrue(gate.getBoolean("added"));
        assertEquals(2, gate.getJSONArray("agents").length());
        assertTrue(gate.getString("detail").contains("waits for"));
        assertTrue(gate.getBoolean("canTurnOff"),
                "An author who has handled readiness elsewhere has to be able to say so");

        String all = view.toString().toLowerCase(Locale.ROOT);
        for (String word : INTERNAL_VOCABULARY) {
            assertFalse(all.contains(word), "The gate leaked the internal term \"" + word + "\"");
        }
    }

    @Test
    void theWaitCanBeDeclined() throws Exception {
        JSONObject view = new FlowAssistantService()
                .propose("p1", twoAgentSnapshot(), twoAgentFlow(),
                        "first greet, then explain, then close", false)
                .authorView();

        assertFalse(view.has("readinessGate"));
    }

    /** A project whose plugins report nothing to wait for gets no wait put in front of it. */
    @Test
    void nothingIsPutInFrontWhenThereIsNothingToWaitFor() throws Exception {
        JSONObject view = new FlowAssistantService()
                .propose("p1", snapshot(), baseFlow(), "first greet, then explain, then close")
                .authorView();

        assertFalse(view.has("readinessGate"),
                "The DesignPatterns project has only a timer, which reports no readiness");
    }

    /**
     * A selected language service must not get the chance to replace a pattern.
     *
     * <p>Pattern output is validated, reproducible and explains itself in the author's words. The
     * service is pointed at a port nothing listens on, so anything reaching it would fail loudly.
     */
    @Test
    void aPatternStillWinsWhenALanguageServiceIsSelected() throws Exception {
        JSONObject view = new FlowAssistantService().propose(
                "p1", snapshot(), baseFlow(), "first greet, then explain, then close", true,
                new SceneFlowIrLlmCandidateProvider.Config(
                        "http://127.0.0.1:9/v1/", null, "nothing", 1, 3)).authorView();

        assertEquals("ready", view.getString("status"));
        assertEquals("pattern", view.getString("generatedBy"));
        assertEquals("sequence", view.getJSONObject("pattern").getString("id"));
    }

    /**
     * A service that cannot be reached leaves the author with the honest non-match, and says that
     * the service was tried. Silence would look like the service had simply not been consulted.
     */
    @Test
    void anUnreachableServiceIsReportedRatherThanSwallowed() throws Exception {
        JSONObject view = new FlowAssistantService().propose(
                "p1", snapshot(), baseFlow(), "make the avatar happy", true,
                new SceneFlowIrLlmCandidateProvider.Config(
                        "http://127.0.0.1:9/v1/", null, "nothing", 1, 3)).authorView();

        assertEquals("no_pattern_matched", view.getString("status"));
        assertTrue(view.getJSONArray("notes").toString().contains("could not be reached"),
                "Expected a note about the service: " + view.optJSONArray("notes"));
    }

    private JSONObject twoAgentSnapshot() {
        return new JSONObject("""
                {
                  "snapshotVersion": "1.3",
                  "project": {
                    "name": "TwoCharacters",
                    "plugins": [
                      {"name": "Px", "className": "x.Executor", "type": "device", "load": true,
                       "commands": [],
                       "writesVariables": [{"name": "characterReady", "type": "Bool",
                                            "boundTo": "avatar_ready",
                                            "description": "Set true when the model is loaded"}],
                       "readsVariables": []},
                      {"name": "Pb", "className": "x.Executor", "type": "device", "load": true,
                       "commands": [],
                       "writesVariables": [{"name": "characterReady", "type": "Bool",
                                            "boundTo": "bob_ready",
                                            "description": "Set true when the model is loaded"}],
                       "readsVariables": []}
                    ],
                    "agents": [
                      {"name": "Xenia", "device": "Px", "features": []},
                      {"name": "Bob", "device": "Pb", "features": []}
                    ]
                  },
                  "script": {"scenes": [], "sections": []},
                  "screens": {"screens": []},
                  "flow": {"rootId": "SceneFlow", "startNodeIds": [], "variables": [],
                           "allowedEdgeTypes": ["EEDGE", "CEDGE", "TEDGE", "IEDGE"],
                           "nodes": [], "edges": []}
                }
                """);
    }

    private String twoAgentFlow() {
        return """
                <?xml version="1.0" encoding="UTF-8"?>
                <SceneFlow id="SceneFlow" name="default" start="" xmlns="xml.sceneflow.dfki.de">
                  <Define></Define>
                  <Declare>
                    <VariableDefinition type="Bool" name ="avatar_ready"><BoolLiteral value="false"/></VariableDefinition>
                    <VariableDefinition type="Bool" name ="bob_ready"><BoolLiteral value="false"/></VariableDefinition>
                  </Declare>
                  <Commands></Commands>
                </SceneFlow>
                """;
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
