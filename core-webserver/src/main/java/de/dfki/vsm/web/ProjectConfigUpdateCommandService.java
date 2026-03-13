package de.dfki.vsm.web;

import de.dfki.vsm.model.project.ProjectConfig;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.function.Consumer;

/**
 * Handles ProjectConfig.Update command.
 */
public final class ProjectConfigUpdateCommandService {

    public interface Context {
        JSONObject errorResponse(String code, String message);

        boolean hasRuntimeProject(String projectId);

        ProjectConfig projectConfig(String projectId);

        void applyProjectConfigFromJson(String projectId, ProjectConfig config, JSONObject configJson);

        void markDirty(String projectId);

        JSONObject projectConfigToJson(ProjectConfig config, String projectPath);

        String projectPath(String projectId);
    }

    public JSONObject dispatch(final JSONObject params,
                               final Consumer<String> broadcaster,
                               final Context context) {
        String pid = params.optString("projectId", "");
        JSONObject configJson = params.optJSONObject("config");
        if (pid.isBlank()) {
            return context.errorResponse("BAD_REQUEST", "Missing projectId");
        }
        if (configJson == null) {
            return context.errorResponse("BAD_REQUEST", "Missing config");
        }
        if (!context.hasRuntimeProject(pid)) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }
        ProjectConfig cfg = context.projectConfig(pid);
        if (cfg == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project config not available");
        }

        context.applyProjectConfigFromJson(pid, cfg, configJson);
        context.markDirty(pid);

        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("config", context.projectConfigToJson(cfg, context.projectPath(pid)));
        response.put("saved", false);
        response.put("pending", true);
        if (broadcaster != null) {
            // Broadcast the full updated config so all windows apply it immediately
            // without needing a REST round-trip.
            JSONObject evt = new JSONObject();
            evt.put("type", "event");
            evt.put("event", "project.config");
            evt.put("projectId", pid);
            evt.put("config", context.projectConfigToJson(cfg, context.projectPath(pid)));
            broadcaster.accept(evt.toString());
        }
        return response;
    }
}
