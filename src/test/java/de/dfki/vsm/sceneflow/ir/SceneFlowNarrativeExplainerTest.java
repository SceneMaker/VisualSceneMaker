package de.dfki.vsm.sceneflow.ir;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SceneFlowNarrativeExplainerTest {

    @Test
    void conceptsMatchDesignPatternsSnapshot() throws Exception {
        JSONObject report = new SceneFlowNarrativeExplainer()
                .explain(Path.of("doc/DesignPatterns/sceneflow.xml"));
        String actual = joinLines(report.getJSONArray("concepts"));
        String expected = loadSnapshot("designpatterns-concepts.txt");
        assertEquals(expected, actual);
    }

    @Test
    void detectsConstrainedActivityWaitPatternInDesignPatternsSceneFlow() throws Exception {
        JSONObject report = new SceneFlowNarrativeExplainer()
                .explain(Path.of("doc/DesignPatterns/sceneflow.xml"));

        assertEquals("SceneFlow", report.optString("sceneFlowId"));
        assertTrue(report.optJSONArray("patterns").length() >= 1);
        assertTrue(report.optJSONArray("summary").length() >= 1);
        assertTrue(report.optJSONArray("patternInventory").length() >= 1);
        assertTrue(containsPatternType(report.getJSONArray("patternInventory"), "fork_parallel_branches"));
        assertTrue(containsPatternType(report.getJSONArray("patternInventory"), "probabilistic_choice"));
        assertTrue(containsPatternType(report.getJSONArray("patternInventory"), "unconditional_transition"));

        JSONObject firstPattern = report.getJSONArray("patterns").getJSONObject(0);
        assertEquals("constrained_activity_wait_for_interrupt", firstPattern.optString("patternType"));
        assertTrue(firstPattern.getJSONObject("evidence").optJSONObject("waitLoop").optLong("timeoutMs", -1) >= 0);
        assertTrue(firstPattern.getJSONObject("evidence").getJSONArray("interruptExits").length() >= 1);
        assertFalse(firstPattern.optString("description", "").isBlank());

        boolean hasN46Pattern = false;
        boolean hasN7GuardedPattern = false;
        for (int i = 0; i < report.getJSONArray("patterns").length(); i++) {
            JSONObject pattern = report.getJSONArray("patterns").getJSONObject(i);
            if (!"node_interrupt_wait_loop".equals(pattern.optString("patternType"))) {
                if ("node_guarded_wait_loop".equals(pattern.optString("patternType"))) {
                    JSONObject evidence = pattern.optJSONObject("evidence");
                    if (evidence != null && "N7".equals(evidence.optString("nodeId"))) {
                        hasN7GuardedPattern = true;
                    }
                }
                continue;
            }
            JSONObject evidence = pattern.optJSONObject("evidence");
            if (evidence != null && "N46".equals(evidence.optString("nodeId"))) {
                hasN46Pattern = true;
                break;
            }
        }
        assertTrue(hasN46Pattern, "Expected node-level interrupt wait pattern for N46.");
        assertTrue(hasN7GuardedPattern, "Expected guarded wait loop pattern for N7.");
    }

    @Test
    void detectsSupernodeSelfTimeoutWaitPattern() throws Exception {
        Path tempSceneFlow = Files.createTempFile("sceneflow-supernode-self-wait", ".xml");
        Files.writeString(tempSceneFlow, """
                <?xml version="1.0" encoding="UTF-8"?>
                <SceneFlow id="SceneFlow" name="SceneFlow" comment="" hideLocalVar="false" hideGlobalVar="false" modifDate="" start="S1;" context="" package="" xmlns="xml.sceneflow.dfki.de" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="xml.sceneflow.dfki.de res/xsd/sceneflow.xsd">
                  <Define></Define>
                  <Declare>
                    <VariableDefinition type="Event(*, 10)" name ="event"></VariableDefinition>
                  </Declare>
                  <Commands></Commands>
                  <Node id="N1" name="After" history="false">
                    <Define></Define>
                    <Declare></Declare>
                    <Commands></Commands>
                  </Node>
                  <SuperNode id="S1" name="WaitSupernode" comment="" hideLocalVar="false" hideGlobalVar="false" start="">
                    <Define></Define>
                    <Declare></Declare>
                    <Commands></Commands>
                    <TEdge target="S1" start="" timeout="1000"></TEdge>
                    <IEdge target="N1" start="">
                      <Eq>
                        <SimpleVariable name="event"/>
                        <StringLiteral><![CDATA[OkayButtonPressed]]></StringLiteral>
                      </Eq>
                    </IEdge>
                  </SuperNode>
                </SceneFlow>
                """);

        JSONObject report = new SceneFlowNarrativeExplainer().explain(tempSceneFlow);
        assertTrue(report.optJSONArray("patterns").length() >= 1);

        JSONObject pattern = report.getJSONArray("patterns").getJSONObject(0);
        JSONObject waitLoop = pattern.getJSONObject("evidence").getJSONObject("waitLoop");
        assertEquals("supernode_self", waitLoop.optString("scope"));
        assertEquals("S1", waitLoop.optString("nodeId"));
        assertEquals(
                loadSnapshot("supernode-self-timeout-description.txt"),
                pattern.optString("description", ""));
    }

    @Test
    void detectsTimeoutRetryOrEscalationPattern() throws Exception {
        Path tempSceneFlow = Files.createTempFile("sceneflow-timeout-retry", ".xml");
        Files.writeString(tempSceneFlow, """
                <?xml version="1.0" encoding="UTF-8"?>
                <SceneFlow id="SceneFlow" name="SceneFlow" comment="" hideLocalVar="false" hideGlobalVar="false" modifDate="" start="N1;" context="" package="" xmlns="xml.sceneflow.dfki.de" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="xml.sceneflow.dfki.de res/xsd/sceneflow.xsd">
                  <Define></Define>
                  <Declare>
                    <VariableDefinition type="Int" name ="retryCounter"><IntLiteral value="0"/></VariableDefinition>
                  </Declare>
                  <Commands></Commands>
                  <Node id="N1" name="RetryLoop" history="false">
                    <Define></Define>
                    <Declare></Declare>
                    <Commands></Commands>
                    <TEdge target="N1" start="" timeout="1000"></TEdge>
                    <CEdge target="N2" start="">
                      <Ge>
                        <SimpleVariable name="retryCounter"/>
                        <IntLiteral value="3"/>
                      </Ge>
                    </CEdge>
                  </Node>
                  <Node id="N2" name="Escalate" history="false">
                    <Define></Define>
                    <Declare></Declare>
                    <Commands></Commands>
                  </Node>
                </SceneFlow>
                """);

        JSONObject report = new SceneFlowNarrativeExplainer().explain(tempSceneFlow);
        boolean found = false;
        for (int i = 0; i < report.getJSONArray("patterns").length(); i++) {
            JSONObject pattern = report.getJSONArray("patterns").getJSONObject(i);
            if (!"timeout_retry_or_escalation".equals(pattern.optString("patternType"))) {
                continue;
            }
            found = true;
            assertEquals(
                    loadSnapshot("timeout-retry-description.txt"),
                    pattern.optString("description", ""));
            assertEquals("N1", pattern.getJSONObject("evidence").optString("nodeId"));
        }
        assertTrue(found, "Expected timeout retry/escalation pattern.");
    }

    @Test
    void includeIdsStyleAddsIdsInBrackets() throws Exception {
        JSONObject report = new SceneFlowNarrativeExplainer()
                .explain(
                        Path.of("doc/DesignPatterns/sceneflow.xml"),
                        new SceneFlowNarrativeExplainer.NarrativeStyle(true));
        assertTrue(report.getJSONArray("summary").toString().contains("(S5)"));
        assertTrue(report.getJSONArray("summary").toString().contains("(N1)"));
    }

    @Test
    void technicalAudienceIncludesCanonicalEdgeCodes() throws Exception {
        Path tempSceneFlow = Files.createTempFile("sceneflow-tech-audience", ".xml");
        Files.writeString(tempSceneFlow, """
                <?xml version="1.0" encoding="UTF-8"?>
                <SceneFlow id="SceneFlow" name="SceneFlow" comment="" hideLocalVar="false" hideGlobalVar="false" modifDate="" start="S1;" context="" package="" xmlns="xml.sceneflow.dfki.de" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="xml.sceneflow.dfki.de res/xsd/sceneflow.xsd">
                  <Define></Define>
                  <Declare>
                    <VariableDefinition type="Event(*, 10)" name ="event"></VariableDefinition>
                  </Declare>
                  <Commands></Commands>
                  <Node id="N1" name="After" history="false">
                    <Define></Define>
                    <Declare></Declare>
                    <Commands></Commands>
                  </Node>
                  <SuperNode id="S1" name="WaitSupernode" comment="" hideLocalVar="false" hideGlobalVar="false" start="">
                    <Define></Define>
                    <Declare></Declare>
                    <Commands></Commands>
                    <TEdge target="S1" start="" timeout="1000"></TEdge>
                    <IEdge target="N1" start="">
                      <Eq>
                        <SimpleVariable name="event"/>
                        <StringLiteral><![CDATA[OkayButtonPressed]]></StringLiteral>
                      </Eq>
                    </IEdge>
                  </SuperNode>
                </SceneFlow>
                """);

        JSONObject report = new SceneFlowNarrativeExplainer()
                .explain(tempSceneFlow, new SceneFlowNarrativeExplainer.NarrativeStyle(false, "technical"));
        assertEquals("technical", report.optString("audience"));
        String summaryText = report.getJSONArray("summary").toString();
        assertTrue(summaryText.contains("TEDGE"));
        assertTrue(summaryText.contains("IEDGE"));
    }

    private String joinLines(final JSONArray array) {
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < array.length(); i++) {
            if (i > 0) {
                out.append('\n');
            }
            out.append(array.optString(i, ""));
        }
        return out.toString();
    }

    private String loadSnapshot(final String fileName) throws IOException {
        final String resourcePath = "/de/dfki/vsm/sceneflow/ir/snapshots/" + fileName;
        try (InputStream in = SceneFlowNarrativeExplainerTest.class.getResourceAsStream(resourcePath)) {
            if (in == null) {
                throw new IOException("Missing snapshot resource: " + resourcePath);
            }
            return new String(in.readAllBytes(), StandardCharsets.UTF_8).replace("\r\n", "\n").trim();
        }
    }

    private boolean containsPatternType(final JSONArray inventory, final String type) {
        for (int i = 0; i < inventory.length(); i++) {
            if (type.equals(inventory.getJSONObject(i).optString("patternType"))) {
                return true;
            }
        }
        return false;
    }

    private static final String CHAIN_HEADER =
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
            + "<SceneFlow id=\"SceneFlow\" name=\"SceneFlow\" comment=\"\" hideLocalVar=\"false\" "
            + "hideGlobalVar=\"false\" modifDate=\"\" start=\"N1;\" context=\"\" package=\"\" "
            + "xmlns=\"xml.sceneflow.dfki.de\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
            + "xsi:schemaLocation=\"xml.sceneflow.dfki.de res/xsd/sceneflow.xsd\">\n"
            + "<Define></Define><Declare>"
            + "<VariableDefinition type=\"Bool\" name =\"ready\"><BoolLiteral value=\"false\"/></VariableDefinition>"
            + "</Declare><Commands></Commands>\n";

    private static String node(final String id, final String name, final String body) {
        return "<Node id=\"" + id + "\" name=\"" + name + "\" history=\"false\">"
                + "<Define></Define><Declare></Declare><Commands></Commands>" + body + "</Node>\n";
    }

    private JSONObject explainFlow(final String nodes) throws Exception {
        Path file = Files.createTempFile("sceneflow-chain", ".xml");
        Files.writeString(file, CHAIN_HEADER + nodes + "</SceneFlow>\n");
        return new SceneFlowNarrativeExplainer().explain(file);
    }

    private JSONObject firstPatternOfType(final JSONObject report, final String type) {
        JSONArray patterns = report.getJSONArray("patterns");
        for (int i = 0; i < patterns.length(); i++) {
            if (type.equals(patterns.getJSONObject(i).optString("patternType"))) {
                return patterns.getJSONObject(i);
            }
        }
        return null;
    }

    private int countPatternsOfType(final JSONObject report, final String type) {
        int count = 0;
        JSONArray patterns = report.getJSONArray("patterns");
        for (int i = 0; i < patterns.length(); i++) {
            if (type.equals(patterns.getJSONObject(i).optString("patternType"))) {
                count++;
            }
        }
        return count;
    }

    @Test
    void threeStepChainIsOneSequenceAndItsHopsAreNotReportedSeparately() throws Exception {
        JSONObject report = explainFlow(
                node("N1", "Greet", "<EEdge target=\"N2\" start=\"\"></EEdge>")
                + node("N2", "Explain", "<EEdge target=\"N3\" start=\"\"></EEdge>")
                + node("N3", "Close", ""));

        JSONObject sequence = firstPatternOfType(report, "sequence");
        assertNotNull(sequence, "A three-node chain must be recognised as one sequence");
        assertEquals(3, sequence.getJSONObject("evidence").getInt("stepCount"));
        assertEquals(0, countPatternsOfType(report, "unconditional_transition"),
                "The two hops are subsumed by the sequence and must not be reported again");
    }

    /** The description is what an author reads in a preview, so it must not be phrased as mechanics. */
    @Test
    void sequenceIsDescribedInStepsRatherThanEdges() throws Exception {
        JSONObject report = explainFlow(
                node("N1", "Greet", "<EEdge target=\"N2\" start=\"\"></EEdge>")
                + node("N2", "Explain", "<EEdge target=\"N3\" start=\"\"></EEdge>")
                + node("N3", "Close", ""));

        String description = firstPatternOfType(report, "sequence").getString("description");
        assertTrue(description.contains("\"Greet\", then \"Explain\", then \"Close\""), description);
        assertFalse(description.contains("EEDGE"), "Reader-friendly output must not expose edge codes");
    }

    /** One hop is a transition. Three nodes in a row are a sequence. */
    @Test
    void twoNodeChainRemainsAnUnconditionalTransition() throws Exception {
        JSONObject report = explainFlow(
                node("N1", "First", "<EEdge target=\"N2\" start=\"\"></EEdge>")
                + node("N2", "Second", ""));

        assertNull(firstPatternOfType(report, "sequence"));
        assertEquals(1, countPatternsOfType(report, "unconditional_transition"));
    }

    /**
     * A step that can divert the flow is not simply followed by "the next step", so the run ends
     * there. This is what separates a plain sequence from a chain that waits or branches midway.
     */
    @Test
    void chainStopsAtAStepThatCanDivertTheFlow() throws Exception {
        JSONObject report = explainFlow(
                node("N1", "Prepare", "<EEdge target=\"N2\" start=\"\"></EEdge>")
                + node("N2", "Execute",
                        "<CEdge target=\"N3\" start=\"\"><SimpleVariable name=\"ready\"/></CEdge>"
                        + "<TEdge target=\"N2\" start=\"\" timeout=\"1000\"></TEdge>")
                + node("N3", "End", ""));

        assertNull(firstPatternOfType(report, "sequence"),
                "A guarded middle step means this is not a plain sequence");
    }

    /** A node reachable from elsewhere is a meeting point rather than the next step of one run. */
    @Test
    void chainStopsWhereAnotherPathJoins() throws Exception {
        JSONObject report = explainFlow(
                node("N1", "First", "<EEdge target=\"N2\" start=\"\"></EEdge>")
                + node("N2", "Second", "<EEdge target=\"N3\" start=\"\"></EEdge>")
                + node("N3", "Shared", "")
                + node("N4", "Other", "<EEdge target=\"N3\" start=\"\"></EEdge>"));

        assertNull(firstPatternOfType(report, "sequence"),
                "N3 is reachable from N2 and N4, so the run must not absorb it");
    }

    @Test
    void technicalAudienceStillNamesTheEdgeTypeForASequence() throws Exception {
        Path file = Files.createTempFile("sceneflow-chain-tech", ".xml");
        Files.writeString(file, CHAIN_HEADER
                + node("N1", "Greet", "<EEdge target=\"N2\" start=\"\"></EEdge>")
                + node("N2", "Explain", "<EEdge target=\"N3\" start=\"\"></EEdge>")
                + node("N3", "Close", "")
                + "</SceneFlow>\n");

        JSONObject report = new SceneFlowNarrativeExplainer()
                .explain(file, new SceneFlowNarrativeExplainer.NarrativeStyle(false, "technical"));
        assertTrue(firstPatternOfType(report, "sequence").getString("description").contains("EEDGE"));
    }

    // ---- ask and wait ----

    private static String askWaitStore(final String channel, final long pollMs, final boolean withStore) {
        return node("N1", "AskName",
                "<Commands><PlayScene><StringLiteral><![CDATA[ask_name]]></StringLiteral></PlayScene>"
                + "<Assignment><SimpleVariable name=\"" + channel + "\"/><Expression>"
                + "<StringLiteral><![CDATA[]]></StringLiteral></Expression></Assignment></Commands>"
                + "<EEdge target=\"N2\" start=\"\"></EEdge>")
            + "<Node id=\"N2\" name=\"WaitName\" history=\"false\">"
                + "<Define></Define><Declare></Declare><Commands></Commands>"
                + "<CEdge target=\"N3\" start=\"\"><Neq><SimpleVariable name=\"" + channel + "\"/>"
                + "<StringLiteral><![CDATA[]]></StringLiteral></Neq></CEdge>"
                + "<TEdge target=\"N2\" start=\"\" timeout=\"" + pollMs + "\"></TEdge></Node>\n"
            + node("N3", "StoreName", withStore
                ? "<Commands><Assignment><SimpleVariable name=\"kept\"/><Expression>"
                  + "<SimpleVariable name=\"" + channel + "\"/></Expression></Assignment></Commands>"
                : "");
    }

    /** The reset is what identifies this shape, so a node clearing what the next node waits for. */
    @Test
    void aQuestionThatWaitsForItsAnswerIsOneFinding() throws Exception {
        JSONObject report = explainFlow(askWaitStore("user_input", 500, true));

        JSONObject asking = firstPatternOfType(report, "ask_and_wait");
        assertNotNull(asking, "Expected an ask_and_wait finding");
        assertEquals("user_input", asking.getJSONObject("evidence").getString("channel"));
        assertEquals("ask_name", asking.getJSONObject("evidence").getString("questionScene"));
        assertEquals(500, asking.getJSONObject("evidence").getInt("pollIntervalMs"));
    }

    /** The polling loop and the hop into it are the same thing described one edge at a time. */
    @Test
    void theWaitLoopAndTheHopIntoItAreNotReportedSeparately() throws Exception {
        JSONObject report = explainFlow(askWaitStore("user_input", 500, true));

        assertEquals(0, countPatternsOfType(report, "node_guarded_wait_loop"),
                "The waiting half is part of the ask_and_wait finding");
        assertEquals(0, countPatternsOfType(report, "unconditional_transition"),
                "The hop from asking to waiting is part of it too");
    }

    /** A description an author reads must say what happens, not which edges carry it. */
    @Test
    void theFindingIsPhrasedAsAQuestionAndAnAnswer() throws Exception {
        String description = firstPatternOfType(explainFlow(askWaitStore("user_input", 500, true)),
                "ask_and_wait").getString("description");

        assertTrue(description.contains("waits for an answer"), description);
        assertFalse(description.contains("CEDGE") || description.contains("TEDGE"), description);
    }

    /**
     * A wait whose variable nothing clears is a different pattern: it waits for a condition rather
     * than for an answer it just invited. WaitForGui in doc/IntakeInterview is the real example.
     */
    @Test
    void aWaitNobodyInvitedIsStillAPlainGuardedWait() throws Exception {
        JSONObject report = explainFlow(
                node("N1", "Setup", "<Commands><PlayScene><StringLiteral><![CDATA[intro]]>"
                        + "</StringLiteral></PlayScene></Commands><EEdge target=\"N2\" start=\"\"></EEdge>")
                + "<Node id=\"N2\" name=\"WaitForGui\" history=\"false\">"
                + "<Define></Define><Declare></Declare><Commands></Commands>"
                + "<CEdge target=\"N3\" start=\"\"><Neq><SimpleVariable name=\"gui_connected\"/>"
                + "<StringLiteral><![CDATA[]]></StringLiteral></Neq></CEdge>"
                + "<TEdge target=\"N2\" start=\"\" timeout=\"500\"></TEdge></Node>\n"
                + node("N3", "Go", ""));

        assertNull(firstPatternOfType(report, "ask_and_wait"),
                "Nothing cleared gui_connected, so nothing asked for it");
        assertEquals(1, countPatternsOfType(report, "node_guarded_wait_loop"));
    }

    /** Without a store the answer is lost when the next question clears the channel. */
    @Test
    void anAnswerThatIsNotKeptIsCalledOut() throws Exception {
        JSONObject report = explainFlow(askWaitStore("user_input", 500, false));
        JSONObject asking = firstPatternOfType(report, "ask_and_wait");

        assertEquals("dropped", asking.getJSONObject("evidence").getString("answerHandling"));
        assertTrue(asking.getString("description").contains("Nothing keeps the answer"),
                asking.getString("description"));
    }

    /**
     * An answer handed straight to a scene is neither kept nor lost, and calling it lost would be
     * wrong. doc/IntakeInterview does exactly this with its generated summary.
     */
    @Test
    void anAnswerUsedStraightAwayIsNotReportedAsLost() throws Exception {
        String ask = node("N1", "AskSummary",
                "<Commands><PlayScene><StringLiteral><![CDATA[thinking]]></StringLiteral></PlayScene>"
                + "<Assignment><SimpleVariable name=\"llm_summary\"/><Expression>"
                + "<StringLiteral><![CDATA[]]></StringLiteral></Expression></Assignment></Commands>"
                + "<EEdge target=\"N2\" start=\"\"></EEdge>");
        String wait = "<Node id=\"N2\" name=\"WaitSummary\" history=\"false\">"
                + "<Define></Define><Declare></Declare><Commands></Commands>"
                + "<CEdge target=\"N3\" start=\"\"><Neq><SimpleVariable name=\"llm_summary\"/>"
                + "<StringLiteral><![CDATA[]]></StringLiteral></Neq></CEdge>"
                + "<TEdge target=\"N2\" start=\"\" timeout=\"1000\"></TEdge></Node>\n";
        String show = node("N3", "ShowSummary",
                "<Commands><PlayScene><StringLiteral><![CDATA[show_summary]]></StringLiteral>"
                + "<StructExpression><Assignment><SimpleVariable name=\"summary\"/><Expression>"
                + "<SimpleVariable name=\"llm_summary\"/></Expression></Assignment>"
                + "</StructExpression></PlayScene></Commands>");

        JSONObject asking = firstPatternOfType(explainFlow(ask + wait + show), "ask_and_wait");
        assertNotNull(asking);
        assertEquals("used", asking.getJSONObject("evidence").getString("answerHandling"));
        assertTrue(asking.getString("description").contains("used straight away"),
                asking.getString("description"));
    }

    /** The same shape waiting on a service rather than a person needs no special case. */
    @Test
    void waitingForASlowServiceIsTheSameFinding() throws Exception {
        JSONObject report = explainFlow(askWaitStore("llm_summary", 1000, true));

        JSONObject asking = firstPatternOfType(report, "ask_and_wait");
        assertNotNull(asking);
        assertEquals("llm_summary", asking.getJSONObject("evidence").getString("channel"));
        assertEquals(1000, asking.getJSONObject("evidence").getInt("pollIntervalMs"));
    }

    /** A chain must not swallow nodes an ask-and-wait already accounts for. */
    @Test
    void aSequenceDoesNotAbsorbAnAskAndWait() throws Exception {
        JSONObject report = explainFlow(askWaitStore("user_input", 500, true));

        assertNotNull(firstPatternOfType(report, "ask_and_wait"));
        assertNull(firstPatternOfType(report, "sequence"),
                "Ask, wait and store are already narrated together");
    }
}
