package de.dfki.vsm.web;

import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.edge.RandomEdge;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

/**
 * Handles SceneFlow.Edge.PEdge.UpdateGroup command.
 */
public final class EdgeProbabilityCommandService {

    public interface Context {
        RunTimeProject runtimeProject(String projectId);

        JSONObject errorResponse(String code, String message);

        SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId);

        BasicNode resolveNodeById(SuperNode root, String nodeId);

        RandomEdge resolvePEdgeForSource(SuperNode root, BasicNode sourceNode, String edgeId, String targetId);

        JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SuperNode snapshotTarget, SceneFlow sceneFlow);

        void broadcastSceneFlowSnapshot(Consumer<String> broadcaster, String projectId, JSONObject snapshot);

        void recordHistory(String projectId, String action);

        void recordCommand(String projectId, String action, JSONObject params);
    }

    public JSONObject dispatch(final JSONObject params,
                               final Consumer<String> broadcaster,
                               final Context context) {
        final String pid = params.optString("projectId", "");
        final String superNodeId = params.optString("superNodeId", null);
        final String sourceId = params.optString("sourceId", "");
        final JSONArray updates = params.optJSONArray("updates");

        final RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }
        if (sourceId.isBlank() || updates == null) {
            return context.errorResponse("BAD_REQUEST", "Missing sourceId or updates");
        }

        final SceneFlow sceneFlow = project.getSceneFlow();
        final SuperNode snapshotTarget = context.resolveSuperNode(sceneFlow, superNodeId);
        final SuperNode targetNode = snapshotTarget != null ? snapshotTarget : sceneFlow;

        final BasicNode sourceNode = context.resolveNodeById(targetNode, sourceId);
        if (sourceNode == null) {
            return context.errorResponse("NODE_NOT_FOUND", "Source node not found: " + sourceId);
        }

        final List<RandomEdge> edges = sourceNode.getPEdgeList();
        if (edges.isEmpty()) {
            return context.errorResponse("EDGE_NOT_FOUND", "No probability edges found");
        }

        final LinkedHashMap<RandomEdge, Integer> updateMap = new LinkedHashMap<>();
        for (int i = 0; i < updates.length(); i++) {
            JSONObject entry = updates.optJSONObject(i);
            if (entry == null) {
                return context.errorResponse("INVALID_PAYLOAD", "Invalid edge update entry");
            }
            String edgeId = entry.optString("edgeId", "");
            String targetId = entry.optString("targetId", "");
            RandomEdge edge = context.resolvePEdgeForSource(targetNode, sourceNode, edgeId, targetId);
            if (edge == null) {
                return context.errorResponse("EDGE_NOT_FOUND", "Edge not found");
            }
            if (updateMap.containsKey(edge)) {
                return context.errorResponse("DUPLICATE_EDGE", "Duplicate edge entry");
            }
            Object raw = entry.opt("probability");
            int probability;
            try {
                probability = Integer.parseInt(String.valueOf(raw));
            } catch (NumberFormatException ex) {
                return context.errorResponse("INVALID_PROBABILITY", "Probability must be a number");
            }
            if (probability < 0 || probability > 100) {
                return context.errorResponse("INVALID_PROBABILITY", "Probability must be between 0 and 100");
            }
            updateMap.put(edge, probability);
        }

        if (updateMap.size() != edges.size()) {
            return context.errorResponse("EDGE_COUNT_MISMATCH", "Provide probabilities for all P-edges");
        }

        int sum = 0;
        for (int probability : updateMap.values()) {
            sum += probability;
        }
        if (sum != 100) {
            return context.errorResponse("PROBABILITY_SUM_INVALID", "Total probability must be 100%");
        }

        for (Map.Entry<RandomEdge, Integer> updateEntry : updateMap.entrySet()) {
            RandomEdge edge = updateEntry.getKey();
            if (edge != null) {
                edge.setProbability(updateEntry.getValue());
            }
        }

        JSONObject response = context.createSceneFlowSnapshot(project, pid, snapshotTarget, sceneFlow);
        response.put("status", "ok");
        context.broadcastSceneFlowSnapshot(broadcaster, pid, response);
        context.recordHistory(pid, "SceneFlow.Edge.PEdge.UpdateGroup");
        context.recordCommand(pid, "SceneFlow.Edge.PEdge.UpdateGroup", params);
        return response;
    }
}
