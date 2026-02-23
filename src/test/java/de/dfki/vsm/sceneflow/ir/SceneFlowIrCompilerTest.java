package de.dfki.vsm.sceneflow.ir;

import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge;
import de.dfki.vsm.util.xml.XMLUtilities;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SceneFlowIrCompilerTest {

    @Test
    void compilesExampleIrAndRoundTripsDesignPatterns() throws Exception {
        SceneFlow base = loadDesignPatternsSceneFlow();
        JSONObject ir = readJson(Path.of("doc/sceneflow-ir.wait-for-ok-button.example.json"));

        SceneFlow compiled = new SceneFlowIrCompiler().compilePatch(ir, base);

        Path tempDir = Files.createTempDirectory("sceneflow-ir-compiler-test");
        Path out = tempDir.resolve("compiled-sceneflow.xml");
        assertTrue(XMLUtilities.writeToXMLFile(compiled, out.toFile(), "UTF-8"));
        assertTrue(Files.exists(out));

        SceneFlow reloaded = new SceneFlow();
        assertTrue(XMLUtilities.parseFromXMLFile(reloaded, out.toFile()));
        reloaded.establishStartNodes();
        reloaded.establishTargetNodes();
        reloaded.establishAltStartNodes();

        BasicNode waitLoop = findNodeById(reloaded, "WaitLoop");
        assertNotNull(waitLoop);
        assertNotNull(waitLoop.getDedge());
        assertTrue(waitLoop.getDedge() instanceof TimeoutEdge);
        assertEquals(1000L, ((TimeoutEdge) waitLoop.getDedge()).getTimeout());
        assertEquals("WaitLoop", waitLoop.getDedge().getTargetUnid());

        BasicNode waitForOkay = findNodeById(reloaded, "WaitForOkay");
        assertNotNull(waitForOkay);
        assertTrue(waitForOkay instanceof SuperNode);

        boolean hasInterruptToAfterOkay = waitForOkay.getIEdgeList().stream().anyMatch(edge ->
                "AfterOkay".equals(edge.getTargetUnid())
                        && edge instanceof InterruptEdge
                        && ((InterruptEdge) edge).getCondition() != null);
        assertTrue(hasInterruptToAfterOkay);
    }

    @Test
    void failsTransactionallyWhenPatchIsInvalid() throws Exception {
        SceneFlow base = loadDesignPatternsSceneFlow();
        JSONObject invalidIr = new JSONObject()
                .put("irVersion", "1.0")
                .put("mode", "patch")
                .put("operations", new JSONArray()
                        .put(new JSONObject()
                                .put("op", "create_node")
                                .put("parentSuperNodeId", "SceneFlow")
                                .put("nodeId", "TempNode")
                                .put("name", "TempNode"))
                        .put(new JSONObject()
                                .put("op", "create_edge")
                                .put("edgeId", "BrokenEdge")
                                .put("edgeType", "TEDGE")
                                .put("sourceNodeId", "TempNode")
                                .put("targetNodeId", "DoesNotExist")
                                .put("payload", new JSONObject().put("timeoutMs", 1000))));

        assertThrows(SceneFlowIrCompileException.class,
                () -> new SceneFlowIrCompiler().compilePatch(invalidIr, base));

        assertNotNull(findNodeById(base, "N1"));
        assertFalse(nodeExists(base, "TempNode"));
    }

    private SceneFlow loadDesignPatternsSceneFlow() {
        SceneFlow flow = new SceneFlow();
        assertTrue(XMLUtilities.parseFromXMLFile(flow, Path.of("doc/DesignPatterns/sceneflow.xml").toFile()));
        flow.establishStartNodes();
        flow.establishTargetNodes();
        flow.establishAltStartNodes();
        return flow;
    }

    private JSONObject readJson(final Path path) throws IOException {
        return new JSONObject(Files.readString(path));
    }

    private boolean nodeExists(final SuperNode root, final String nodeId) {
        return findNodeById(root, nodeId) != null;
    }

    private BasicNode findNodeById(final SuperNode root, final String nodeId) {
        if (root.getId().equals(nodeId)) {
            return root;
        }
        for (BasicNode node : root.getNodeList()) {
            if (node.getId().equals(nodeId)) {
                return node;
            }
        }
        for (SuperNode node : root.getSuperNodeList()) {
            if (node.getId().equals(nodeId)) {
                return node;
            }
            BasicNode found = findNodeById(node, nodeId);
            if (found != null) {
                return found;
            }
        }
        return null;
    }
}
