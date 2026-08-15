package de.dfki.vsm.sceneflow.ir;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Method;

import static org.junit.jupiter.api.Assertions.assertEquals;

class SceneFlowIrLlmCandidateProviderTest {

    @Test
    void normalizesDataWrappedNodeAndEdgeFields() throws Exception {
        SceneFlowIrLlmCandidateProvider provider = new SceneFlowIrLlmCandidateProvider();
        Method normalize = SceneFlowIrLlmCandidateProvider.class
                .getDeclaredMethod("normalizeCandidate", JSONObject.class, int.class);
        normalize.setAccessible(true);

        JSONObject candidate = new JSONObject()
                .put("irVersion", "1.0")
                .put("mode", "patch")
                .put("operations", new JSONArray()
                        .put(new JSONObject()
                                .put("op", "create_node")
                                .put("data", new JSONObject()
                                        .put("id", "S100")
                                        .put("isSuperNode", true)
                                        .put("name", "Wait")
                                        .put("parentSuperNodeId", "SceneFlow")))
                        .put(new JSONObject()
                                .put("op", "create_edge")
                                .put("data", new JSONObject()
                                        .put("id", "E100")
                                        .put("type", "IEDGE")
                                        .put("sourceNodeId", "S100")
                                        .put("targetNodeId", "N1")
                                        .put("payload", new JSONObject()
                                                .put("conditionText", "event == \"OkayButtonPressed\"")))));

        normalize.invoke(provider, candidate, 0);

        JSONArray ops = candidate.getJSONArray("operations");
        JSONObject createSupernode = ops.getJSONObject(0);
        JSONObject createEdge = ops.getJSONObject(1);

        assertEquals("create_supernode", createSupernode.getString("op"));
        assertEquals("S100", createSupernode.getString("superNodeId"));
        assertEquals("SceneFlow", createSupernode.getString("parentSuperNodeId"));

        assertEquals("create_edge", createEdge.getString("op"));
        assertEquals("E100", createEdge.getString("edgeId"));
        assertEquals("IEDGE", createEdge.getString("edgeType"));
        assertEquals("S100", createEdge.getString("sourceNodeId"));
        assertEquals("N1", createEdge.getString("targetNodeId"));
        assertEquals("event == \"OkayButtonPressed\"",
                createEdge.getJSONObject("payload").getString("conditionText"));
    }
}

