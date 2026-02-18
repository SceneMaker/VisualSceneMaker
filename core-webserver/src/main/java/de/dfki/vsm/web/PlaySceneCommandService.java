package de.dfki.vsm.web;

import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;

/**
 * Handles SceneFlow.PlayScene.Find/FindMany/Rename commands.
 */
public final class PlaySceneCommandService {

    public interface Context {
        RunTimeProject runtimeProject(String projectId);

        JSONObject errorResponse(String code, String message);

        void collectPlaySceneReferences(SuperNode root, String sceneName, List<JSONObject> matches);

        void collectPlaySceneReferences(SuperNode root, Set<String> sceneNames, List<JSONObject> matches);

        int renamePlaySceneReferences(SuperNode root, String sceneName, String newName);

        void markDirty(String projectId);

        SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId);

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
            case "SceneFlow.PlayScene.Find":
                return findPlaySceneReferences(params, context);
            case "SceneFlow.PlayScene.FindMany":
                return findPlaySceneReferencesMany(params, context);
            case "SceneFlow.PlayScene.Rename":
                return renamePlaySceneReferences(params, broadcaster, context);
            default:
                return context.errorResponse("BAD_REQUEST", "Unsupported playscene command: " + method);
        }
    }

    private JSONObject findPlaySceneReferences(final JSONObject params, final Context context) {
        String pid = params.optString("projectId", "");
        String sceneName = params.optString("sceneName", "").trim();
        RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }
        if (sceneName.isBlank()) {
            return context.errorResponse("BAD_REQUEST", "Missing sceneName");
        }

        SceneFlow sceneFlow = project.getSceneFlow();
        if (sceneFlow == null) {
            return context.errorResponse("SCENEFLOW_NOT_FOUND", "SceneFlow not available");
        }

        List<JSONObject> matches = new ArrayList<>();
        context.collectPlaySceneReferences(sceneFlow, sceneName, matches);
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("matches", new JSONArray(matches));
        response.put("count", matches.size());
        return response;
    }

    private JSONObject findPlaySceneReferencesMany(final JSONObject params, final Context context) {
        String pid = params.optString("projectId", "");
        JSONArray namesJson = params.optJSONArray("sceneNames");
        RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }
        if (namesJson == null || namesJson.isEmpty()) {
            return context.errorResponse("BAD_REQUEST", "Missing sceneNames");
        }

        Set<String> names = new HashSet<>();
        for (int i = 0; i < namesJson.length(); i++) {
            String name = namesJson.optString(i, "").trim();
            if (!name.isEmpty()) {
                names.add(name);
            }
        }
        if (names.isEmpty()) {
            return context.errorResponse("BAD_REQUEST", "Missing sceneNames");
        }

        SceneFlow sceneFlow = project.getSceneFlow();
        if (sceneFlow == null) {
            return context.errorResponse("SCENEFLOW_NOT_FOUND", "SceneFlow not available");
        }

        List<JSONObject> matches = new ArrayList<>();
        context.collectPlaySceneReferences(sceneFlow, names, matches);
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("matches", new JSONArray(matches));
        response.put("count", matches.size());
        return response;
    }

    private JSONObject renamePlaySceneReferences(final JSONObject params,
                                                 final Consumer<String> broadcaster,
                                                 final Context context) {
        String pid = params.optString("projectId", "");
        String sceneName = params.optString("sceneName", "").trim();
        String newName = params.optString("newName", "").trim();
        String superNodeId = params.optString("superNodeId", "").trim();
        RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }
        if (sceneName.isBlank() || newName.isBlank()) {
            return context.errorResponse("BAD_REQUEST", "Missing sceneName or newName");
        }

        SceneFlow sceneFlow = project.getSceneFlow();
        if (sceneFlow == null) {
            return context.errorResponse("SCENEFLOW_NOT_FOUND", "SceneFlow not available");
        }

        int updated = context.renamePlaySceneReferences(sceneFlow, sceneName, newName);
        context.markDirty(pid);

        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("updated", updated);

        SuperNode snapshotTarget = context.resolveSuperNode(sceneFlow, superNodeId);
        if (snapshotTarget == null) {
            snapshotTarget = sceneFlow;
        }
        JSONObject snapshot = context.createSceneFlowSnapshot(project, pid, snapshotTarget, sceneFlow);
        response.put("snapshot", snapshot);
        if (broadcaster != null) {
            context.broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
        }
        context.recordHistory(pid, "SceneFlow.PlayScene.Rename");
        context.recordCommand(pid, "SceneFlow.PlayScene.Rename", params);
        return response;
    }
}
