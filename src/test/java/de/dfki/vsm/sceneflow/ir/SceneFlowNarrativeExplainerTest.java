package de.dfki.vsm.sceneflow.ir;

import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SceneFlowNarrativeExplainerTest {

    @Test
    void detectsConstrainedActivityWaitPatternInDesignPatternsSceneFlow() throws Exception {
        JSONObject report = new SceneFlowNarrativeExplainer()
                .explain(Path.of("doc/DesignPatterns/sceneflow.xml"));

        assertEquals("SceneFlow", report.optString("sceneFlowId"));
        assertTrue(report.optJSONArray("patterns").length() >= 1);
        assertTrue(report.optJSONArray("summary").length() >= 1);

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
        assertTrue(pattern.optString("description", "").contains("self timeout edge on the supernode"));
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
            assertTrue(pattern.optString("description", "").contains("threshold"));
            assertEquals("N1", pattern.getJSONObject("evidence").optString("nodeId"));
        }
        assertTrue(found, "Expected timeout retry/escalation pattern.");
    }
}
