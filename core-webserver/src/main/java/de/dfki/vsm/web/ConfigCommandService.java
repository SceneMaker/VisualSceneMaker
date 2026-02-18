package de.dfki.vsm.web;

import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.json.JSONObject;

import java.util.Properties;
import java.util.function.Consumer;

/**
 * Handles Config.Update command.
 */
public final class ConfigCommandService {

    public interface Context {
        JSONObject errorResponse(String code, String message);

        boolean projectExists(String projectId);

        Properties loadEditorConfig(String projectId);

        boolean saveEditorConfig(String projectId);

        String projectPath(String projectId);

        void setEditorConfigDirty(String projectId, boolean value);

        void setProjectDirty(String projectId, boolean value);

        JSONObject editorConfigToJson(Properties props);

        RunTimeProject runtimeProject(String projectId);

        SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId);

        JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SuperNode snapshotTarget, SceneFlow sceneFlow);
    }

    public JSONObject dispatch(final JSONObject params,
                               final Consumer<String> broadcaster,
                               final Context context) {
        String pid = params.optString("projectId", "");
        JSONObject values = params.optJSONObject("values");
        if (pid.isBlank()) {
            return context.errorResponse("BAD_REQUEST", "Missing projectId");
        }
        if (values == null) {
            return context.errorResponse("BAD_REQUEST", "Missing values");
        }
        if (!context.projectExists(pid)) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }

        Properties config = context.loadEditorConfig(pid);
        for (String key : values.keySet()) {
            Object raw = values.get(key);
            if (raw == null || raw == JSONObject.NULL) {
                config.remove(key);
            } else {
                config.setProperty(key, String.valueOf(raw));
            }
        }

        boolean saved = false;
        boolean pending = false;
        String path = context.projectPath(pid);
        String normalizedPath = path == null ? "" : path.trim();
        if (!normalizedPath.isBlank()) {
            saved = context.saveEditorConfig(pid);
            if (!saved) {
                return context.errorResponse("CONFIG_SAVE_FAILED", "Failed to save editor config");
            }
        } else {
            pending = true;
            context.setEditorConfigDirty(pid, true);
            context.setProjectDirty(pid, true);
        }

        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("config", context.editorConfigToJson(config));
        response.put("saved", saved);
        response.put("pending", pending);

        RunTimeProject project = context.runtimeProject(pid);
        if (project != null) {
            SceneFlow sceneFlow = project.getSceneFlow();
            String superNodeId = params.optString("superNodeId", "");
            SuperNode snapshotTarget = context.resolveSuperNode(sceneFlow, superNodeId);
            JSONObject snapshot = context.createSceneFlowSnapshot(
                    project,
                    pid,
                    snapshotTarget != null ? snapshotTarget : sceneFlow,
                    sceneFlow
            );
            response.put("snapshot", snapshot);
            if (broadcaster != null) {
                JSONObject evt = new JSONObject();
                evt.put("event", "sceneflow.snapshot");
                evt.put("snapshot", snapshot);
                broadcaster.accept(evt.toString());
            }
        }
        return response;
    }
}
