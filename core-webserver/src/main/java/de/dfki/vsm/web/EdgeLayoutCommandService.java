package de.dfki.vsm.web;

import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;

/**
 * Handles SceneFlow.Edge.Normalize/Straighten commands.
 */
public final class EdgeLayoutCommandService {

    public interface Context {
        RunTimeProject runtimeProject(String projectId);

        JSONObject errorResponse(String code, String message);

        SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId);

        AbstractEdge resolveEdgeById(SuperNode root, String edgeId);

        int getEditorConfigInt(String projectId, String key, int fallback);

        void relayoutEdgesInOrder(List<AbstractEdge> edges, int nodeWidth, int nodeHeight);

        void normalizeEdge(AbstractEdge edge, int nodeWidth, int nodeHeight);

        void clearDockPointsRecursive(SuperNode root);

        void occupyStartSignDockPointsRecursive(SuperNode root);

        void collectEdgesRecursive(SuperNode root, List<AbstractEdge> edges, Set<AbstractEdge> seen);

        JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SuperNode snapshotTarget, SceneFlow sceneFlow);

        void broadcastSceneFlowSnapshot(Consumer<String> broadcaster, String projectId, JSONObject snapshot);

        void recordHistory(String projectId, String action);

        void recordCommand(String projectId, String action, JSONObject params);
    }

    public JSONObject dispatch(final String method,
                               final JSONObject params,
                               final Consumer<String> broadcaster,
                               final Context context) {
        switch (method) {
            case "SceneFlow.Edge.Normalize":
            case "SceneFlow.Edge.Straighten":
                return handleSingle(method, params, broadcaster, context);
            case "SceneFlow.Edge.NormalizeAll":
            case "SceneFlow.Edge.StraightenAll":
                return handleAll(method, params, broadcaster, context);
            case "SceneFlow.Edge.NormalizeGroup":
            case "SceneFlow.Edge.StraightenGroup":
                return handleGroup(method, params, broadcaster, context);
            default:
                return context.errorResponse("BAD_REQUEST", "Unsupported edge layout command: " + method);
        }
    }

    private JSONObject handleSingle(final String method,
                                    final JSONObject params,
                                    final Consumer<String> broadcaster,
                                    final Context context) {
        final String pid = params.optString("projectId", "");
        final String superNodeId = params.optString("superNodeId", null);
        final String edgeId = params.optString("edgeId", "");

        final RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }
        if (edgeId.isBlank()) {
            return context.errorResponse("BAD_REQUEST", "Missing edgeId");
        }

        final SceneFlow sceneFlow = project.getSceneFlow();
        final SuperNode snapshotTarget = context.resolveSuperNode(sceneFlow, superNodeId);
        final SuperNode targetNode = snapshotTarget != null ? snapshotTarget : sceneFlow;

        final AbstractEdge dataEdge = context.resolveEdgeById(targetNode, edgeId);
        if (dataEdge == null) {
            return context.errorResponse("EDGE_NOT_FOUND", "Edge not found: " + edgeId);
        }

        final boolean isNormalize = "SceneFlow.Edge.Normalize".equals(method);
        final int nodeWidth = context.getEditorConfigInt(pid, "node_width", 90);
        final int nodeHeight = context.getEditorConfigInt(pid, "node_height", nodeWidth);
        if (!isNormalize) {
            List<AbstractEdge> relayout = new ArrayList<>();
            relayout.add(dataEdge);
            context.relayoutEdgesInOrder(relayout, nodeWidth, nodeHeight);
        }
        context.normalizeEdge(dataEdge, nodeWidth, nodeHeight);

        return snapshotAndRecord(context, project, pid, snapshotTarget, sceneFlow, method, params, broadcaster);
    }

    private JSONObject handleAll(final String method,
                                 final JSONObject params,
                                 final Consumer<String> broadcaster,
                                 final Context context) {
        final String pid = params.optString("projectId", "");
        final String superNodeId = params.optString("superNodeId", null);

        final RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }

        final SceneFlow sceneFlow = project.getSceneFlow();
        final SuperNode snapshotTarget = context.resolveSuperNode(sceneFlow, superNodeId);
        final SuperNode targetNode = snapshotTarget != null ? snapshotTarget : sceneFlow;

        final boolean isNormalize = "SceneFlow.Edge.NormalizeAll".equals(method);
        final int nodeWidth = context.getEditorConfigInt(pid, "node_width", 90);
        final int nodeHeight = context.getEditorConfigInt(pid, "node_height", nodeWidth);
        if (!isNormalize) {
            context.clearDockPointsRecursive(targetNode);
            context.occupyStartSignDockPointsRecursive(targetNode);
            List<AbstractEdge> relayout = new ArrayList<>();
            Set<AbstractEdge> seen = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap<>());
            context.collectEdgesRecursive(targetNode, relayout, seen);
            context.relayoutEdgesInOrder(relayout, nodeWidth, nodeHeight);
        }
        for (BasicNode node : targetNode.getNodeAndSuperNodeList()) {
            for (AbstractEdge edge : node.getEdgeList()) {
                context.normalizeEdge(edge, nodeWidth, nodeHeight);
            }
        }

        return snapshotAndRecord(context, project, pid, snapshotTarget, sceneFlow, method, params, broadcaster);
    }

    private JSONObject handleGroup(final String method,
                                   final JSONObject params,
                                   final Consumer<String> broadcaster,
                                   final Context context) {
        final String pid = params.optString("projectId", "");
        final String superNodeId = params.optString("superNodeId", null);
        final JSONArray edgeIds = params.optJSONArray("edgeIds");

        final RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }
        if (edgeIds == null || edgeIds.length() == 0) {
            return context.errorResponse("BAD_REQUEST", "Missing edgeIds");
        }

        final SceneFlow sceneFlow = project.getSceneFlow();
        final SuperNode snapshotTarget = context.resolveSuperNode(sceneFlow, superNodeId);
        final SuperNode targetNode = snapshotTarget != null ? snapshotTarget : sceneFlow;

        final boolean isNormalize = "SceneFlow.Edge.NormalizeGroup".equals(method);
        final int nodeWidth = context.getEditorConfigInt(pid, "node_width", 90);
        final int nodeHeight = context.getEditorConfigInt(pid, "node_height", nodeWidth);

        List<AbstractEdge> groupEdges = new ArrayList<>();
        for (int i = 0; i < edgeIds.length(); i++) {
            String edgeId = edgeIds.optString(i, "").trim();
            if (edgeId.isEmpty()) {
                continue;
            }
            AbstractEdge edge = context.resolveEdgeById(targetNode, edgeId);
            if (edge != null) {
                groupEdges.add(edge);
            }
        }
        if (!isNormalize && !groupEdges.isEmpty()) {
            context.relayoutEdgesInOrder(groupEdges, nodeWidth, nodeHeight);
        }
        for (AbstractEdge edge : groupEdges) {
            context.normalizeEdge(edge, nodeWidth, nodeHeight);
        }

        return snapshotAndRecord(context, project, pid, snapshotTarget, sceneFlow, method, params, broadcaster);
    }

    private JSONObject snapshotAndRecord(final Context context,
                                         final RunTimeProject project,
                                         final String projectId,
                                         final SuperNode snapshotTarget,
                                         final SceneFlow sceneFlow,
                                         final String method,
                                         final JSONObject params,
                                         final Consumer<String> broadcaster) {
        JSONObject response = context.createSceneFlowSnapshot(project, projectId, snapshotTarget, sceneFlow);
        response.put("status", "ok");
        context.broadcastSceneFlowSnapshot(broadcaster, projectId, response);
        context.recordHistory(projectId, method);
        context.recordCommand(projectId, method, params);
        return response;
    }
}
