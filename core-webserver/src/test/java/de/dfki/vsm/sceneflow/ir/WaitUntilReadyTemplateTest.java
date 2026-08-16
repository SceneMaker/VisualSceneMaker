package de.dfki.vsm.sceneflow.ir;

import de.dfki.vsm.testsupport.TestRepoPaths;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Waiting for the agents to be ready before anything starts.
 *
 * <p>The snapshots here are written out rather than read from a fixture, because the case that
 * matters is two characters on one plugin class, and what distinguishes them is only the variable
 * each instance is bound to. plugins/charamel-embed/ExampleProject is the real example, and
 * CapabilitySnapshotCommandInventoryTest pins the snapshot side of it.
 */
class WaitUntilReadyTemplateTest {

    /** Two characters on one plugin class, each with its own connected and ready variables. */
    private JSONObject twoCharacterSnapshot() {
        return new JSONObject("""
                {
                  "snapshotVersion": "1.3",
                  "project": {
                    "name": "TwoCharacters",
                    "plugins": [
                      {
                        "name": "CharamelXenia", "className": "x.CharamelEmbedExecutor",
                        "type": "device", "load": true, "commands": [],
                        "writesVariables": [
                          {"name": "sceneflowVar", "type": "Bool", "boundTo": "avatar_connected",
                           "description": "Set true when the character page connects"},
                          {"name": "characterReady", "type": "Bool", "boundTo": "avatar_ready",
                           "description": "Set true when the character model is fully loaded"}
                        ],
                        "readsVariables": []
                      },
                      {
                        "name": "CharamelBob", "className": "x.CharamelEmbedExecutor",
                        "type": "device", "load": true, "commands": [],
                        "writesVariables": [
                          {"name": "sceneflowVar", "type": "Bool", "boundTo": "bob_connected",
                           "description": "Set true when the character page connects"},
                          {"name": "characterReady", "type": "Bool", "boundTo": "bob_ready",
                           "description": "Set true when the character model is fully loaded"}
                        ],
                        "readsVariables": []
                      }
                    ],
                    "agents": [
                      {"name": "Xenia", "device": "CharamelXenia", "features": []},
                      {"name": "Bob", "device": "CharamelBob", "features": []}
                    ]
                  },
                  "script": {"scenes": [], "sections": []},
                  "screens": {"screens": []},
                  "flow": {
                    "rootId": "SceneFlow", "startNodeIds": [], "variables": [],
                    "allowedEdgeTypes": ["EEDGE", "CEDGE", "PEDGE", "TEDGE", "FEDGE", "IEDGE"],
                    "nodes": [], "edges": []
                  }
                }
                """);
    }

    /** One agent whose plugin reports only that it has connected. */
    private JSONObject connectOnlySnapshot() {
        JSONObject snapshot = twoCharacterSnapshot();
        JSONArray plugins = snapshot.getJSONObject("project").getJSONArray("plugins");
        plugins.remove(1);
        plugins.getJSONObject(0).getJSONArray("writesVariables").remove(1);
        snapshot.getJSONObject("project").getJSONArray("agents").remove(1);
        return snapshot;
    }

    @Test
    void waitingForEveryAgentIsOneConditionRatherThanSeveralWaits() {
        JSONObject candidate = candidate("wait until the agents are ready before anything starts",
                twoCharacterSnapshot());
        assertNotNull(candidate, "A readiness situation has to produce a gate");

        assertEquals(1, countOps(candidate, "create_supernode"));
        JSONObject release = edgeOfType(candidate, "IEDGE");
        assertNotNull(release, "The gate opens on an interrupt edge");
        assertEquals("avatar_ready && bob_ready",
                release.getJSONObject("payload").getString("conditionText"),
                "Both agents belong in one condition, not in two gates");
    }

    /**
     * The interrupt edge has to sit on the supernode. On the waiting node inside it, readiness would
     * only be noticed at the next tick of the self-loop.
     */
    @Test
    void theGateOpensFromTheSupernodeNotFromTheNodeInsideIt() {
        JSONObject candidate = candidate("wait until Xenia and Bob are ready", twoCharacterSnapshot());
        String superNodeId = candidate.getJSONObject("metadata").getString("gateSuperNodeId");

        assertEquals(superNodeId, edgeOfType(candidate, "IEDGE").getString("sourceNodeId"));

        JSONObject tick = edgeOfType(candidate, "TEDGE");
        assertEquals(tick.getString("sourceNodeId"), tick.getString("targetNodeId"),
                "The node inside the gate keeps the flow occupied by looping onto itself");
    }

    /** The waiting node must stay empty, so nothing delays the flow while the gate is closed. */
    @Test
    void nothingIsPutInsideTheGateByDefault() {
        JSONObject candidate = candidate("wait until the agents are ready", twoCharacterSnapshot());

        assertEquals(0, countOps(candidate, "add_node_command"));
        assertTrue(candidate.getJSONArray("assumptions").toString().contains("Put a scene inside"),
                "An author has to be told that the gate is where a waiting message goes");
    }

    @Test
    void namingOneAgentWaitsForThatOneOnly() {
        JSONObject candidate = candidate("wait until Bob is ready", twoCharacterSnapshot());

        assertEquals("bob_ready",
                edgeOfType(candidate, "IEDGE").getJSONObject("payload").getString("conditionText"));
    }

    /**
     * Being connected and being able to act are different moments. A plugin reporting both must be
     * waited on for the later one, since speaking in between fails without any sign of it.
     */
    @Test
    void beingAbleToActIsPreferredOverHavingConnected() {
        JSONObject candidate = candidate("wait until Xenia is ready", twoCharacterSnapshot());

        String condition = edgeOfType(candidate, "IEDGE").getJSONObject("payload").getString("conditionText");
        assertEquals("avatar_ready", condition);
        assertFalse(condition.contains("connected"),
                "Waiting on the connection would let the flow speak before the model is loaded");
    }

    @Test
    void anAgentThatOnlyReportsConnectingIsWaitedForWithThatSaidPlainly() {
        JSONObject candidate = candidate("wait until the agent is ready", connectOnlySnapshot());

        assertEquals("avatar_connected",
                edgeOfType(candidate, "IEDGE").getJSONObject("payload").getString("conditionText"));
        assertTrue(candidate.getJSONArray("assumptions").toString().contains("only that it has connected"),
                "An author has to be warned that the gate opens earlier than it looks");
    }

    /** Without a plugin that reports readiness there is nothing to wait on, so nothing is invented. */
    @Test
    void aProjectWithNothingToWaitForProducesNoGate() throws Exception {
        JSONObject designPatterns = new JSONObject(
                Files.readString(TestRepoPaths.doc("capability-snapshot.designpatterns.json")));

        assertNull(candidate("wait until the agent is ready", designPatterns),
                "The timer plugin reports no readiness, so a gate on it would never open");
    }

    /**
     * The gate is generated as a patch onto a real flow, so its ids must not collide and its
     * condition must survive semantic validation.
     */
    @Test
    void theGateValidatesAgainstARealProject() throws Exception {
        JSONObject snapshot = twoCharacterSnapshot();
        JSONObject candidate = candidate("wait until the agents are ready", snapshot);

        SemanticValidationResult result = new SceneFlowIrSemanticValidator().validate(candidate, snapshot);
        assertFalse(result.hasErrors(), "Unexpected errors: " + result.getIssues());
    }

    /** A readiness situation must not be answered with a wait for a button press. */
    @Test
    void aReadinessSituationDoesNotFallThroughToTheConstrainedActivityWait() {
        List<JSONObject> candidates = new SceneFlowIrTemplateLibrary()
                .generateCandidates("wait until the agents are ready", twoCharacterSnapshot());

        List<String> sources = new ArrayList<>();
        for (JSONObject each : candidates) {
            sources.add(each.getJSONObject("metadata").getString("source"));
        }
        assertTrue(sources.contains("template-wait-until-ready"), "Expected a gate, got " + sources);
        assertFalse(sources.contains("template-constrained-activity"),
                "A readiness gate and a button wait are different things: " + sources);
    }

    /**
     * A flow that starts by speaking gets the gate put in front of it, because an agent that has not
     * connected swallows what it is told without saying so.
     */
    @Test
    void aFlowThatStartsBySpeakingGetsTheGateInFrontOfIt() throws Exception {
        JSONObject snapshot = twoCharacterSnapshot();
        JSONObject candidate = viaPipeline("first greet, then explain, then close", snapshot, true);

        JSONObject gate = candidate.getJSONObject("metadata").getJSONObject("readinessGate");
        assertTrue(gate.getBoolean("added"));
        assertEquals(2, gate.getJSONArray("waitsFor").length(), "Both characters are waited for");

        JSONObject release = edgeOfType(candidate, "IEDGE");
        assertEquals(gate.getString("continuationNodeId"), release.getString("targetNodeId"),
                "The gate opens onto what the flow used to start with");
        assertEquals(gate.getString("gateSuperNodeId"), release.getString("sourceNodeId"));
    }

    /** Exactly one thing may be where the flow starts, and after prepending that is the gate. */
    @Test
    void theGateBecomesTheOnlyStartAndTheOldFirstStepIsNoLongerOne() throws Exception {
        JSONObject candidate = viaPipeline("first greet, then explain, then close",
                twoCharacterSnapshot(), true);

        List<String> starts = new ArrayList<>();
        JSONArray ops = candidate.getJSONArray("operations");
        for (int i = 0; i < ops.length(); i++) {
            JSONObject op = ops.getJSONObject(i);
            if (op.optBoolean("isStartNode", false) && !"SceneFlow".equals(op.optString("parentSuperNodeId"))) {
                continue;
            }
            if (op.optBoolean("isStartNode", false)) {
                starts.add(op.optString("superNodeId", op.optString("nodeId")));
            }
        }
        assertEquals(1, starts.size(), "Exactly one start at the root, got " + starts);
        assertEquals(candidate.getJSONObject("metadata").getJSONObject("readinessGate")
                .getString("gateSuperNodeId"), starts.get(0));
    }

    /** The gate is put in front once. A flow that already waits for the same agents keeps its own. */
    @Test
    void aFlowThatAlreadyWaitsForTheAgentsGetsNoSecondGate() throws Exception {
        JSONObject snapshot = twoCharacterSnapshot();
        snapshot.getJSONObject("flow").getJSONArray("edges").put(new JSONObject()
                .put("type", "IEDGE")
                .put("sourceNodeId", "S1")
                .put("targetNodeId", "N1")
                .put("conditionText", "avatar_ready && bob_ready"));

        JSONObject candidate = viaPipeline("first greet, then explain, then close", snapshot, true);
        assertFalse(candidate.getJSONObject("metadata").has("readinessGate"),
                "The flow already waits, so nothing is added");
    }

    /** Switching the gate off is how an author says they have handled readiness elsewhere. */
    @Test
    void theGateCanBeTurnedOff() throws Exception {
        JSONObject candidate = viaPipeline("first greet, then explain, then close",
                twoCharacterSnapshot(), false);

        assertFalse(candidate.getJSONObject("metadata").has("readinessGate"));
        assertNull(edgeOfType(candidate, "IEDGE"));
    }

    /** Runs the whole pipeline so the merged candidate is the one that actually compiled. */
    private JSONObject viaPipeline(final String situation, final JSONObject snapshot,
                                   final boolean readinessGate) throws Exception {
        java.nio.file.Path work = java.nio.file.Files.createTempDirectory("readiness-gate");
        java.nio.file.Path snapshotPath = work.resolve("capabilities.json");
        java.nio.file.Path basePath = work.resolve("sceneflow.xml");
        java.nio.file.Files.writeString(snapshotPath, snapshot.toString());
        java.nio.file.Files.writeString(basePath, """
                <?xml version="1.0" encoding="UTF-8"?>
                <SceneFlow id="SceneFlow" name="default" start="" xmlns="xml.sceneflow.dfki.de">
                  <Define></Define>
                  <Declare>
                    <VariableDefinition type="Bool" name ="avatar_ready"><BoolLiteral value="false"/></VariableDefinition>
                    <VariableDefinition type="Bool" name ="bob_ready"><BoolLiteral value="false"/></VariableDefinition>
                  </Declare>
                  <Commands></Commands>
                </SceneFlow>
                """);

        JSONObject report = new SceneFlowSituationPipeline().run(
                snapshotPath, basePath, work.resolve("out.xml"), work.resolve("report.json"), situation,
                new SceneFlowSituationPipeline.Settings(
                        SceneFlowSituationPipeline.CandidateMode.TEMPLATE,
                        SceneFlowSituationPipeline.OutputMode.PATCH,
                        null, ConstraintResolutionMode.PERMISSIVE, readinessGate),
                work.resolve("project"));

        assertEquals("success", report.optString("status"),
                "The merged flow has to compile: " + report.optJSONArray("attempts"));
        JSONArray attempts = report.getJSONArray("attempts");
        for (int i = 0; i < attempts.length(); i++) {
            if ("accepted".equals(attempts.getJSONObject(i).optString("status"))) {
                return attempts.getJSONObject(i).getJSONObject("candidate");
            }
        }
        throw new AssertionError("No accepted attempt in " + attempts);
    }

    private JSONObject candidate(final String situation, final JSONObject snapshot) {
        for (JSONObject each : new SceneFlowIrTemplateLibrary().generateCandidates(situation, snapshot)) {
            if ("template-wait-until-ready".equals(each.getJSONObject("metadata").optString("source"))) {
                return each;
            }
        }
        return null;
    }

    private JSONObject edgeOfType(final JSONObject candidate, final String edgeType) {
        JSONArray ops = candidate.getJSONArray("operations");
        for (int i = 0; i < ops.length(); i++) {
            JSONObject op = ops.getJSONObject(i);
            if ("create_edge".equals(op.optString("op")) && edgeType.equals(op.optString("edgeType"))) {
                return op;
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
}
