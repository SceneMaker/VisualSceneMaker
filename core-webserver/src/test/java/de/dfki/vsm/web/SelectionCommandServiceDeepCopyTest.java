package de.dfki.vsm.web;

import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgeGraphics;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.BoolLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.StringLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.variable.SimpleVariable;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.PlayScenesActivity;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;

class SelectionCommandServiceDeepCopyTest {

    @Test
    void pasteDeepCopiesSupernodeContent() {
        SelectionCommandService service = new SelectionCommandService();
        TestContext context = new TestContext();
        String pid = "p1";

        RunTimeProject project = new RunTimeProject();
        context.projects.put(pid, project);
        SceneFlow root = project.getSceneFlow();

        SuperNode original = new SuperNode();
        original.setId("S1");
        original.setName("S1");
        original.setParentNode(root);
        root.addSuperNode(original);
        root.addStartNode(original);

        BasicNode a = new BasicNode();
        a.setId("A1");
        a.setName("A");
        a.setParentNode(original);
        original.addNode(a);
        original.addStartNode(a);
        original.setHistoryNode(a);

        BasicNode b = new BasicNode();
        b.setId("B1");
        b.setName("B");
        b.setParentNode(original);
        original.addNode(b);

        GuargedEdge ab = new GuargedEdge();
        ab.setSourceNode(a);
        ab.setSourceUnid(a.getId());
        ab.setTargetNode(b);
        ab.setTargetUnid(b.getId());
        ab.setCondition(new BoolLiteral(true));
        a.addCEdge(ab);

        JSONObject copyParams = new JSONObject();
        copyParams.put("projectId", pid);
        copyParams.put("nodeIds", new JSONArray().put("S1"));
        JSONObject copyResult = service.dispatch("SceneFlow.Selection.Copy", copyParams, ignored -> { }, context);
        assertEquals("ok", copyResult.optString("status"));

        JSONObject pasteParams = new JSONObject();
        pasteParams.put("projectId", pid);
        pasteParams.put("superNodeId", "");
        pasteParams.put("dx", 40);
        pasteParams.put("dy", 40);
        JSONObject pasteResult = service.dispatch("SceneFlow.Selection.Paste", pasteParams, ignored -> { }, context);
        assertEquals("ok", pasteResult.optString("status"));

        JSONArray pastedIds = pasteResult.optJSONArray("nodeIds");
        assertNotNull(pastedIds);
        assertEquals(1, pastedIds.length());
        String pastedSuperId = pastedIds.getString(0);
        assertNotEquals("S1", pastedSuperId);

        BasicNode pastedNode = context.resolveNodeById(root, pastedSuperId);
        assertTrue(pastedNode instanceof SuperNode);
        SuperNode pastedSuper = (SuperNode) pastedNode;

        assertEquals(2, pastedSuper.getNodeList().size());
        BasicNode pastedA = pastedSuper.getNodeList().stream().filter(n -> "A".equals(n.getName())).findFirst().orElse(null);
        BasicNode pastedB = pastedSuper.getNodeList().stream().filter(n -> "B".equals(n.getName())).findFirst().orElse(null);
        assertNotNull(pastedA);
        assertNotNull(pastedB);
        assertNotEquals("A1", pastedA.getId());
        assertNotEquals("B1", pastedB.getId());

        assertEquals(1, pastedA.getCEdgeList().size());
        GuargedEdge copiedEdge = pastedA.getCEdgeAt(0);
        assertEquals(pastedB.getId(), copiedEdge.getTargetUnid());
        assertNotNull(copiedEdge.getCondition());
        assertEquals("true", copiedEdge.getCondition().getConcreteSyntax());
        assertEquals(1, pastedSuper.getStartNodeMap().size());
        assertNotNull(pastedSuper.getHistoryNode());
        assertEquals(pastedA.getId(), pastedSuper.getHistoryNode().getId());
    }

    @Test
    void pasteSuperAndNodeDoesNotDuplicateTopLevelSuperEdges() {
        SelectionCommandService service = new SelectionCommandService();
        TestContext context = new TestContext();
        String pid = "p2";

        RunTimeProject project = new RunTimeProject();
        context.projects.put(pid, project);
        SceneFlow root = project.getSceneFlow();

        SuperNode originalSuper = new SuperNode();
        originalSuper.setId("S1");
        originalSuper.setName("S1");
        originalSuper.setParentNode(root);
        root.addSuperNode(originalSuper);
        root.addStartNode(originalSuper);

        BasicNode external = new BasicNode();
        external.setId("N1");
        external.setName("External");
        external.setParentNode(root);
        root.addNode(external);

        GuargedEdge superToExternal = new GuargedEdge();
        superToExternal.setSourceNode(originalSuper);
        superToExternal.setSourceUnid(originalSuper.getId());
        superToExternal.setTargetNode(external);
        superToExternal.setTargetUnid(external.getId());
        superToExternal.setCondition(new BoolLiteral(true));
        superToExternal.setGraphics(new EdgeGraphics());
        originalSuper.addCEdge(superToExternal);

        JSONObject copyParams = new JSONObject();
        copyParams.put("projectId", pid);
        copyParams.put("nodeIds", new JSONArray().put("S1").put("N1"));
        JSONObject copyResult = service.dispatch("SceneFlow.Selection.Copy", copyParams, ignored -> { }, context);
        assertEquals("ok", copyResult.optString("status"));

        JSONObject pasteParams = new JSONObject();
        pasteParams.put("projectId", pid);
        pasteParams.put("superNodeId", "");
        pasteParams.put("dx", 24);
        pasteParams.put("dy", 24);
        JSONObject pasteResult = service.dispatch("SceneFlow.Selection.Paste", pasteParams, ignored -> { }, context);
        assertEquals("ok", pasteResult.optString("status"));

        JSONArray pastedIds = pasteResult.optJSONArray("nodeIds");
        assertNotNull(pastedIds);
        assertEquals(2, pastedIds.length());

        List<BasicNode> topLevel = root.getNodeAndSuperNodeList();
        SuperNode pastedSuper = topLevel.stream()
                .filter(n -> n instanceof SuperNode)
                .map(n -> (SuperNode) n)
                .filter(sn -> !"S1".equals(sn.getId()))
                .findFirst()
                .orElse(null);
        BasicNode pastedExternal = topLevel.stream()
                .filter(n -> !(n instanceof SuperNode))
                .filter(n -> "External".equals(n.getName()) && !"N1".equals(n.getId()))
                .findFirst()
                .orElse(null);

        assertNotNull(pastedSuper);
        assertNotNull(pastedExternal);

        long edgesToPastedExternal = pastedSuper.getCEdgeList().stream()
                .filter(e -> pastedExternal.getId().equals(e.getTargetUnid()))
                .count();
        assertEquals(1L, edgesToPastedExternal, "pasted supernode should have exactly one copied edge to pasted node");

        boolean stillPointsToOriginal = pastedSuper.getCEdgeList().stream()
                .anyMatch(e -> "N1".equals(e.getTargetUnid()));
        assertFalse(stillPointsToOriginal, "pasted supernode must not keep edge to original node");
    }

    @Test
    void pasteKeepsInterruptEdgeFromSupernodeToCopiedTopLevelNode() {
        SelectionCommandService service = new SelectionCommandService();
        TestContext context = new TestContext();
        String pid = "p3";

        RunTimeProject project = new RunTimeProject();
        context.projects.put(pid, project);
        SceneFlow root = project.getSceneFlow();

        SuperNode sourceSuper = new SuperNode();
        sourceSuper.setId("S1");
        sourceSuper.setName("SourceSuper");
        sourceSuper.setParentNode(root);
        root.addSuperNode(sourceSuper);

        BasicNode sourceNode = new BasicNode();
        sourceNode.setId("N1");
        sourceNode.setName("TargetNode");
        sourceNode.setParentNode(root);
        root.addNode(sourceNode);

        InterruptEdge interruptEdge = new InterruptEdge();
        interruptEdge.setSourceNode(sourceSuper);
        interruptEdge.setSourceUnid(sourceSuper.getId());
        interruptEdge.setTargetNode(sourceNode);
        interruptEdge.setTargetUnid(sourceNode.getId());
        interruptEdge.setCondition(new BoolLiteral(true));
        interruptEdge.setGraphics(new EdgeGraphics());
        sourceSuper.addIEdge(interruptEdge);

        JSONObject copyParams = new JSONObject();
        copyParams.put("projectId", pid);
        copyParams.put("nodeIds", new JSONArray().put("S1").put("N1"));
        JSONObject copyResult = service.dispatch("SceneFlow.Selection.Copy", copyParams, ignored -> { }, context);
        assertEquals("ok", copyResult.optString("status"));

        JSONObject pasteParams = new JSONObject();
        pasteParams.put("projectId", pid);
        pasteParams.put("superNodeId", "");
        pasteParams.put("dx", 12);
        pasteParams.put("dy", 12);
        JSONObject pasteResult = service.dispatch("SceneFlow.Selection.Paste", pasteParams, ignored -> { }, context);
        assertEquals("ok", pasteResult.optString("status"));

        JSONArray pastedIds = pasteResult.optJSONArray("nodeIds");
        assertNotNull(pastedIds);
        assertEquals(2, pastedIds.length());

        List<BasicNode> topLevel = root.getNodeAndSuperNodeList();
        SuperNode pastedSuper = topLevel.stream()
                .filter(n -> n instanceof SuperNode)
                .map(n -> (SuperNode) n)
                .filter(sn -> "SourceSuper".equals(sn.getName()) && !"S1".equals(sn.getId()))
                .findFirst()
                .orElse(null);
        BasicNode pastedTarget = topLevel.stream()
                .filter(n -> !(n instanceof SuperNode))
                .filter(n -> "TargetNode".equals(n.getName()) && !"N1".equals(n.getId()))
                .findFirst()
                .orElse(null);

        assertNotNull(pastedSuper);
        assertNotNull(pastedTarget);

        long copiedInterruptEdges = pastedSuper.getIEdgeList().stream()
                .filter(e -> pastedTarget.getId().equals(e.getTargetUnid()))
                .count();
        assertEquals(1L, copiedInterruptEdges, "pasted supernode should keep one interrupt edge to pasted target");

        boolean stillPointsToOriginal = pastedSuper.getIEdgeList().stream()
                .anyMatch(e -> "N1".equals(e.getTargetUnid()));
        assertFalse(stillPointsToOriginal, "pasted supernode must not keep interrupt edge to original node");
    }

    @Test
    void pasteCopiesInterruptEdgeBetweenSelectedBasicNodes() {
        SelectionCommandService service = new SelectionCommandService();
        TestContext context = new TestContext();
        String pid = "p4";

        RunTimeProject project = new RunTimeProject();
        context.projects.put(pid, project);
        SceneFlow root = project.getSceneFlow();

        BasicNode source = new BasicNode();
        source.setId("N1");
        source.setName("Source");
        source.setParentNode(root);
        root.addNode(source);

        BasicNode target = new BasicNode();
        target.setId("N2");
        target.setName("Target");
        target.setParentNode(root);
        root.addNode(target);

        InterruptEdge iedge = new InterruptEdge();
        iedge.setSourceNode(source);
        iedge.setSourceUnid(source.getId());
        iedge.setTargetNode(target);
        iedge.setTargetUnid(target.getId());
        iedge.setCondition(new BoolLiteral(true));
        iedge.setGraphics(new EdgeGraphics());
        source.addIEdge(iedge);

        JSONObject copyParams = new JSONObject();
        copyParams.put("projectId", pid);
        copyParams.put("nodeIds", new JSONArray().put("N1").put("N2"));
        JSONObject copyResult = service.dispatch("SceneFlow.Selection.Copy", copyParams, ignored -> { }, context);
        assertEquals("ok", copyResult.optString("status"));

        JSONObject pasteParams = new JSONObject();
        pasteParams.put("projectId", pid);
        pasteParams.put("superNodeId", "");
        pasteParams.put("dx", 18);
        pasteParams.put("dy", 18);
        JSONObject pasteResult = service.dispatch("SceneFlow.Selection.Paste", pasteParams, ignored -> { }, context);
        assertEquals("ok", pasteResult.optString("status"));

        List<BasicNode> topLevel = root.getNodeAndSuperNodeList();
        BasicNode pastedSource = topLevel.stream()
                .filter(n -> "Source".equals(n.getName()) && !"N1".equals(n.getId()))
                .findFirst()
                .orElse(null);
        BasicNode pastedTarget = topLevel.stream()
                .filter(n -> "Target".equals(n.getName()) && !"N2".equals(n.getId()))
                .findFirst()
                .orElse(null);

        assertNotNull(pastedSource);
        assertNotNull(pastedTarget);
        assertEquals(1, pastedSource.getIEdgeList().size());
        assertEquals(pastedTarget.getId(), pastedSource.getIEdgeAt(0).getTargetUnid());
    }

    @Test
    void pasteCopiesStartSignForSelectedNode() {
        SelectionCommandService service = new SelectionCommandService();
        TestContext context = new TestContext();
        String pid = "p4-start";

        RunTimeProject project = new RunTimeProject();
        context.projects.put(pid, project);
        SceneFlow root = project.getSceneFlow();

        BasicNode source = new BasicNode();
        source.setId("N1");
        source.setName("StartNode");
        source.setParentNode(root);
        root.addNode(source);
        root.addStartNode(source);

        JSONObject copyParams = new JSONObject();
        copyParams.put("projectId", pid);
        copyParams.put("nodeIds", new JSONArray().put("N1"));
        JSONObject copyResult = service.dispatch("SceneFlow.Selection.Copy", copyParams, ignored -> { }, context);
        assertEquals("ok", copyResult.optString("status"));

        JSONObject pasteParams = new JSONObject();
        pasteParams.put("projectId", pid);
        pasteParams.put("superNodeId", "");
        pasteParams.put("dx", 18);
        pasteParams.put("dy", 18);
        JSONObject pasteResult = service.dispatch("SceneFlow.Selection.Paste", pasteParams, ignored -> { }, context);
        assertEquals("ok", pasteResult.optString("status"));

        BasicNode pasted = root.getNodeList().stream()
                .filter(n -> "StartNode".equals(n.getName()) && !"N1".equals(n.getId()))
                .findFirst()
                .orElse(null);
        assertNotNull(pasted);
        assertTrue(root.getStartNodeMap().containsKey(pasted.getId()), "pasted node should keep start sign");
    }

    @Test
    void pasteDeepCopiedSupernodeKeepsInternalInterruptEdges() {
        SelectionCommandService service = new SelectionCommandService();
        TestContext context = new TestContext();
        String pid = "p5";

        RunTimeProject project = new RunTimeProject();
        context.projects.put(pid, project);
        SceneFlow root = project.getSceneFlow();

        SuperNode sourceSuper = new SuperNode();
        sourceSuper.setId("S1");
        sourceSuper.setName("Container");
        sourceSuper.setParentNode(root);
        root.addSuperNode(sourceSuper);

        BasicNode a = new BasicNode();
        a.setId("A1");
        a.setName("A");
        a.setParentNode(sourceSuper);
        sourceSuper.addNode(a);

        BasicNode b = new BasicNode();
        b.setId("B1");
        b.setName("B");
        b.setParentNode(sourceSuper);
        sourceSuper.addNode(b);

        InterruptEdge iedge = new InterruptEdge();
        iedge.setSourceNode(a);
        iedge.setSourceUnid(a.getId());
        iedge.setTargetNode(b);
        iedge.setTargetUnid(b.getId());
        iedge.setCondition(new BoolLiteral(true));
        iedge.setGraphics(new EdgeGraphics());
        a.addIEdge(iedge);

        JSONObject copyParams = new JSONObject();
        copyParams.put("projectId", pid);
        copyParams.put("nodeIds", new JSONArray().put("S1"));
        JSONObject copyResult = service.dispatch("SceneFlow.Selection.Copy", copyParams, ignored -> { }, context);
        assertEquals("ok", copyResult.optString("status"));

        JSONObject pasteParams = new JSONObject();
        pasteParams.put("projectId", pid);
        pasteParams.put("superNodeId", "");
        pasteParams.put("dx", 20);
        pasteParams.put("dy", 20);
        JSONObject pasteResult = service.dispatch("SceneFlow.Selection.Paste", pasteParams, ignored -> { }, context);
        assertEquals("ok", pasteResult.optString("status"));

        String pastedSuperId = pasteResult.getJSONArray("nodeIds").getString(0);
        SuperNode pastedSuper = (SuperNode) context.resolveNodeById(root, pastedSuperId);
        assertNotNull(pastedSuper);

        BasicNode pastedA = pastedSuper.getNodeList().stream().filter(n -> "A".equals(n.getName())).findFirst().orElse(null);
        BasicNode pastedB = pastedSuper.getNodeList().stream().filter(n -> "B".equals(n.getName())).findFirst().orElse(null);
        assertNotNull(pastedA);
        assertNotNull(pastedB);
        assertEquals(1, pastedA.getIEdgeList().size());
        assertEquals(pastedB.getId(), pastedA.getIEdgeAt(0).getTargetUnid());
    }

    @Test
    void pasteFromDifferentSourceProjectCopiesIntoTargetProject() {
        SelectionCommandService service = new SelectionCommandService();
        TestContext context = new TestContext();
        String sourcePid = "source-project";
        String targetPid = "target-project";

        RunTimeProject sourceProject = new RunTimeProject();
        context.projects.put(sourcePid, sourceProject);
        SceneFlow sourceRoot = sourceProject.getSceneFlow();

        RunTimeProject targetProject = new RunTimeProject();
        context.projects.put(targetPid, targetProject);
        SceneFlow targetRoot = targetProject.getSceneFlow();

        BasicNode source = new BasicNode();
        source.setId("N1");
        source.setName("Source");
        source.setParentNode(sourceRoot);
        sourceRoot.addNode(source);

        BasicNode target = new BasicNode();
        target.setId("N2");
        target.setName("Target");
        target.setParentNode(sourceRoot);
        sourceRoot.addNode(target);

        InterruptEdge iedge = new InterruptEdge();
        iedge.setSourceNode(source);
        iedge.setSourceUnid(source.getId());
        iedge.setTargetNode(target);
        iedge.setTargetUnid(target.getId());
        iedge.setCondition(new BoolLiteral(true));
        iedge.setGraphics(new EdgeGraphics());
        source.addIEdge(iedge);

        JSONObject copyParams = new JSONObject();
        copyParams.put("projectId", sourcePid);
        copyParams.put("nodeIds", new JSONArray().put("N1").put("N2"));
        JSONObject copyResult = service.dispatch("SceneFlow.Selection.Copy", copyParams, ignored -> { }, context);
        assertEquals("ok", copyResult.optString("status"));

        // the target project's own clipboard was never populated
        assertTrue(context.clipboardNodes(targetPid).isEmpty());

        JSONObject pasteParams = new JSONObject();
        pasteParams.put("projectId", targetPid);
        pasteParams.put("sourceProjectId", sourcePid);
        pasteParams.put("superNodeId", "");
        pasteParams.put("dx", 10);
        pasteParams.put("dy", 10);
        JSONObject pasteResult = service.dispatch("SceneFlow.Selection.Paste", pasteParams, ignored -> { }, context);
        assertEquals("ok", pasteResult.optString("status"));

        JSONArray pastedIds = pasteResult.optJSONArray("nodeIds");
        assertNotNull(pastedIds);
        assertEquals(2, pastedIds.length());

        // the source project itself is untouched
        assertEquals(2, sourceRoot.getNodeList().size());

        List<BasicNode> targetTopLevel = targetRoot.getNodeAndSuperNodeList();
        BasicNode pastedSource = targetTopLevel.stream()
                .filter(n -> "Source".equals(n.getName()))
                .findFirst()
                .orElse(null);
        BasicNode pastedTarget = targetTopLevel.stream()
                .filter(n -> "Target".equals(n.getName()))
                .findFirst()
                .orElse(null);

        assertNotNull(pastedSource);
        assertNotNull(pastedTarget);
        assertNotEquals("N1", pastedSource.getId());
        assertNotEquals("N2", pastedTarget.getId());
        assertEquals(1, pastedSource.getIEdgeList().size());
        assertEquals(pastedTarget.getId(), pastedSource.getIEdgeAt(0).getTargetUnid());
    }

    @Test
    void pasteFromClosedSourceProjectReturnsError() {
        SelectionCommandService service = new SelectionCommandService();
        TestContext context = new TestContext();
        String targetPid = "target-only";

        RunTimeProject targetProject = new RunTimeProject();
        context.projects.put(targetPid, targetProject);

        JSONObject pasteParams = new JSONObject();
        pasteParams.put("projectId", targetPid);
        pasteParams.put("sourceProjectId", "no-such-project");
        JSONObject pasteResult = service.dispatch("SceneFlow.Selection.Paste", pasteParams, ignored -> { }, context);

        assertEquals("error", pasteResult.optString("status"));
        assertEquals("SOURCE_PROJECT_NOT_FOUND", pasteResult.optString("code"));
    }

    @Test
    void pasteWarnsWhenPlayedSceneIsMissingFromTargetSceneScript() {
        SelectionCommandService service = new SelectionCommandService();
        TestContext context = new TestContext();
        String pid = "scene-warning-missing";

        RunTimeProject project = new RunTimeProject();
        context.projects.put(pid, project);
        SceneFlow root = project.getSceneFlow();

        BasicNode node = new BasicNode();
        node.setId("N1");
        node.setName("Source");
        node.setParentNode(root);
        node.addCmd(new PlayScenesActivity(new StringLiteral("Greeting")));
        root.addNode(node);

        JSONObject copyParams = new JSONObject();
        copyParams.put("projectId", pid);
        copyParams.put("nodeIds", new JSONArray().put("N1"));
        service.dispatch("SceneFlow.Selection.Copy", copyParams, ignored -> { }, context);

        JSONObject pasteParams = new JSONObject();
        pasteParams.put("projectId", pid);
        pasteParams.put("superNodeId", "");
        pasteParams.put("dx", 10);
        pasteParams.put("dy", 10);
        JSONObject pasteResult = service.dispatch("SceneFlow.Selection.Paste", pasteParams, ignored -> { }, context);

        assertEquals("ok", pasteResult.optString("status"));
        JSONArray warnings = pasteResult.optJSONArray("warnings");
        assertNotNull(warnings, "expected a warning about the missing scene");
        boolean mentionsScene = false;
        for (int i = 0; i < warnings.length(); i++) {
            if (warnings.getString(i).contains("Greeting")) {
                mentionsScene = true;
            }
        }
        assertTrue(mentionsScene, "expected a warning mentioning the missing scene 'Greeting': " + warnings);
    }

    @Test
    void pasteDoesNotWarnWhenPlayedSceneExistsInTargetSceneScript() {
        SelectionCommandService service = new SelectionCommandService();
        TestContext context = new TestContext();
        String pid = "scene-warning-present";

        RunTimeProject project = new RunTimeProject();
        context.projects.put(pid, project);
        project.getSceneScript().parseTXT("scene en Greeting\nBob: Hi there.\n");
        SceneFlow root = project.getSceneFlow();

        BasicNode node = new BasicNode();
        node.setId("N1");
        node.setName("Source");
        node.setParentNode(root);
        node.addCmd(new PlayScenesActivity(new StringLiteral("Greeting")));
        root.addNode(node);

        JSONObject copyParams = new JSONObject();
        copyParams.put("projectId", pid);
        copyParams.put("nodeIds", new JSONArray().put("N1"));
        service.dispatch("SceneFlow.Selection.Copy", copyParams, ignored -> { }, context);

        JSONObject pasteParams = new JSONObject();
        pasteParams.put("projectId", pid);
        pasteParams.put("superNodeId", "");
        pasteParams.put("dx", 10);
        pasteParams.put("dy", 10);
        JSONObject pasteResult = service.dispatch("SceneFlow.Selection.Paste", pasteParams, ignored -> { }, context);

        assertEquals("ok", pasteResult.optString("status"));
        JSONArray warnings = pasteResult.optJSONArray("warnings");
        if (warnings != null) {
            for (int i = 0; i < warnings.length(); i++) {
                assertFalse(warnings.getString(i).contains("Greeting"),
                        "did not expect a warning about scene 'Greeting': " + warnings);
            }
        }
    }

    @Test
    void pasteWarnsWhenCommandReferencesUndeclaredGlobalVariable() {
        SelectionCommandService service = new SelectionCommandService();
        TestContext context = new TestContext();
        String pid = "var-warning-missing";

        RunTimeProject project = new RunTimeProject();
        context.projects.put(pid, project);
        SceneFlow root = project.getSceneFlow();

        BasicNode node = new BasicNode();
        node.setId("N1");
        node.setName("Source");
        node.setParentNode(root);
        node.addCmd(new SimpleVariable("undeclaredVar"));
        root.addNode(node);

        JSONObject copyParams = new JSONObject();
        copyParams.put("projectId", pid);
        copyParams.put("nodeIds", new JSONArray().put("N1"));
        service.dispatch("SceneFlow.Selection.Copy", copyParams, ignored -> { }, context);

        JSONObject pasteParams = new JSONObject();
        pasteParams.put("projectId", pid);
        pasteParams.put("superNodeId", "");
        pasteParams.put("dx", 10);
        pasteParams.put("dy", 10);
        JSONObject pasteResult = service.dispatch("SceneFlow.Selection.Paste", pasteParams, ignored -> { }, context);

        assertEquals("ok", pasteResult.optString("status"));
        JSONArray warnings = pasteResult.optJSONArray("warnings");
        assertNotNull(warnings, "expected a warning about the undeclared variable");
        boolean mentionsVar = false;
        for (int i = 0; i < warnings.length(); i++) {
            if (warnings.getString(i).contains("undeclaredVar")) {
                mentionsVar = true;
            }
        }
        assertTrue(mentionsVar, "expected a warning mentioning 'undeclaredVar': " + warnings);
    }

    @Test
    void pasteDoesNotWarnWhenReferencedVariableIsDeclaredGlobally() {
        SelectionCommandService service = new SelectionCommandService();
        TestContext context = new TestContext();
        String pid = "var-warning-present";

        RunTimeProject project = new RunTimeProject();
        context.projects.put(pid, project);
        SceneFlow root = project.getSceneFlow();
        root.addVarDef(new VariableDefinition("declaredVar", "String", null));

        BasicNode node = new BasicNode();
        node.setId("N1");
        node.setName("Source");
        node.setParentNode(root);
        node.addCmd(new SimpleVariable("declaredVar"));
        root.addNode(node);

        JSONObject copyParams = new JSONObject();
        copyParams.put("projectId", pid);
        copyParams.put("nodeIds", new JSONArray().put("N1"));
        service.dispatch("SceneFlow.Selection.Copy", copyParams, ignored -> { }, context);

        JSONObject pasteParams = new JSONObject();
        pasteParams.put("projectId", pid);
        pasteParams.put("superNodeId", "");
        pasteParams.put("dx", 10);
        pasteParams.put("dy", 10);
        JSONObject pasteResult = service.dispatch("SceneFlow.Selection.Paste", pasteParams, ignored -> { }, context);

        assertEquals("ok", pasteResult.optString("status"));
        JSONArray warnings = pasteResult.optJSONArray("warnings");
        if (warnings != null) {
            for (int i = 0; i < warnings.length(); i++) {
                assertFalse(warnings.getString(i).contains("declaredVar"),
                        "did not expect a warning about declared variable 'declaredVar': " + warnings);
            }
        }
    }

    private static final class TestContext implements SelectionCommandService.Context {
        private final Map<String, RunTimeProject> projects = new HashMap<>();
        private final Map<String, List<BasicNode>> nodeClipboard = new HashMap<>();
        private final Map<String, List<SelectionCommandService.ClipboardEdgeData>> edgeClipboard = new HashMap<>();
        private final Map<String, Set<String>> startClipboard = new HashMap<>();
        private final AtomicInteger idCounter = new AtomicInteger(1000);

        @Override
        public RunTimeProject runtimeProject(String projectId) {
            return projects.get(projectId);
        }

        @Override
        public JSONObject errorResponse(String code, String message) {
            return new JSONObject().put("status", "error").put("code", code).put("message", message);
        }

        @Override
        public BasicNode findNodeRecursive(SuperNode root, String nodeId) {
            return resolveNodeById(root, nodeId);
        }

        @Override
        public SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId) {
            if (superNodeId == null || superNodeId.isBlank()) {
                return sceneFlow;
            }
            BasicNode found = resolveNodeById(sceneFlow, superNodeId);
            return found instanceof SuperNode ? (SuperNode) found : null;
        }

        @Override
        public int getEditorConfigInt(String projectId, String key, int fallback) {
            return fallback;
        }

        @Override
        public void collectNodes(SuperNode node, List<BasicNode> out) {
            out.add(node);
            for (BasicNode child : node.getNodeList()) {
                out.add(child);
            }
            for (SuperNode child : node.getSuperNodeList()) {
                collectNodes(child, out);
            }
        }

        @Override
        public String allocateNodeId(String projectId, boolean superNode, Set<String> used) {
            String prefix = superNode ? "S" : "N";
            String id;
            do {
                id = prefix + idCounter.incrementAndGet();
            } while (used.contains(id));
            return id;
        }

        @Override
        public BasicNode resolveNodeById(SuperNode root, String nodeId) {
            if (root == null || nodeId == null) {
                return null;
            }
            if (nodeId.equals(root.getId())) {
                return root;
            }
            for (BasicNode node : root.getNodeList()) {
                if (nodeId.equals(node.getId())) {
                    return node;
                }
            }
            for (SuperNode child : root.getSuperNodeList()) {
                BasicNode found = resolveNodeById(child, nodeId);
                if (found != null) {
                    return found;
                }
            }
            return null;
        }

        @Override
        public Expression parseExpressionOrNull(String text) {
            return null;
        }

        @Override
        public void initializeEdgeDockPoints(AbstractEdge edge, int nodeWidth, int nodeHeight) {
        }

        @Override
        public void normalizeEdge(AbstractEdge edge, int nodeWidth, int nodeHeight) {
        }

        @Override
        public JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SuperNode snapshotTarget, SceneFlow sceneFlow) {
            return new JSONObject().put("status", "ok");
        }

        @Override
        public JSONObject buildSceneFlowResponse(JSONObject snapshot) {
            return new JSONObject(snapshot.toString());
        }

        @Override
        public void broadcastSceneFlowSnapshot(Consumer<String> broadcaster, String projectId, JSONObject snapshot) {
        }

        @Override
        public void recordHistory(String projectId, String action) {
        }

        @Override
        public void markDirty(String projectId) {
        }

        @Override
        public List<BasicNode> clipboardNodes(String projectId) {
            return nodeClipboard.computeIfAbsent(projectId, ignored -> new ArrayList<>());
        }

        @Override
        public List<SelectionCommandService.ClipboardEdgeData> clipboardEdges(String projectId) {
            return edgeClipboard.computeIfAbsent(projectId, ignored -> new ArrayList<>());
        }

        @Override
        public Set<String> clipboardStartNodeIds(String projectId) {
            return startClipboard.computeIfAbsent(projectId, ignored -> new HashSet<>());
        }
    }
}
