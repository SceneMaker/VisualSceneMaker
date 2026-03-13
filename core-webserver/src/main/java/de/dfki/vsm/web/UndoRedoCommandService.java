package de.dfki.vsm.web;

import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.function.Consumer;

/**
 * Handles SceneFlow.Undo and SceneFlow.Redo commands.
 */
public final class UndoRedoCommandService {

    public interface Context {
        RunTimeProject runtimeProject(String projectId);

        JSONObject errorResponse(String code, String message);

        void ensureHistoryLoaded(String projectId);

        int historyIndex(String projectId);

        int historySize(String projectId);

        void setHistoryIndex(String projectId, int index);

        void setHistorySuspended(String projectId, boolean value);

        void setCommandLogSuspended(String projectId, boolean value);

        boolean applyHistoryEntryAtIndex(String projectId, int index);

        void saveHistoryToDisk(String projectId);

        JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SceneFlow snapshotTarget, SceneFlow sceneFlow);

        JSONObject buildSceneFlowResponse(JSONObject snapshot);

        void broadcastSceneFlowSnapshot(Consumer<String> broadcaster, String projectId, JSONObject snapshot);

        JSONObject buildScriptSnapshot(String projectId);

        void broadcastScriptSnapshot(Consumer<String> broadcaster, String projectId, JSONObject snapshot);
    }

    public JSONObject dispatch(final String method,
                               final JSONObject params,
                               final Consumer<String> broadcaster,
                               final Context context) {
        switch (method) {
            case "SceneFlow.Undo":
                return undoProject(params, broadcaster, context);
            case "SceneFlow.Redo":
                return redoProject(params, broadcaster, context);
            default:
                return context.errorResponse("BAD_REQUEST", "Unsupported undo/redo command: " + method);
        }
    }

    private JSONObject undoProject(final JSONObject params,
                                   final Consumer<String> broadcaster,
                                   final Context context) {
        String pid = params.optString("projectId", "");
        RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }

        context.ensureHistoryLoaded(pid);
        int currentIndex = context.historyIndex(pid);
        if (currentIndex <= 0) {
            JSONObject response = new JSONObject();
            response.put("status", "ok");
            response.put("applied", false);
            return response;
        }

        context.setHistorySuspended(pid, true);
        context.setCommandLogSuspended(pid, true);
        try {
            int nextIndex = Math.max(0, currentIndex - 1);
            context.setHistoryIndex(pid, nextIndex);
            if (!context.applyHistoryEntryAtIndex(pid, nextIndex)) {
                return context.errorResponse("UNDO_FAILED", "Failed to apply undo");
            }
        } finally {
            context.setHistorySuspended(pid, false);
            context.setCommandLogSuspended(pid, false);
        }

        return buildPostApplyResponse(context, project, pid, broadcaster);
    }

    private JSONObject redoProject(final JSONObject params,
                                   final Consumer<String> broadcaster,
                                   final Context context) {
        String pid = params.optString("projectId", "");
        RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }

        context.ensureHistoryLoaded(pid);
        int currentIndex = context.historyIndex(pid);
        int size = context.historySize(pid);
        if (currentIndex >= size - 1) {
            JSONObject response = new JSONObject();
            response.put("status", "ok");
            response.put("applied", false);
            return response;
        }

        context.setHistorySuspended(pid, true);
        context.setCommandLogSuspended(pid, true);
        try {
            int nextIndex = Math.min(size - 1, currentIndex + 1);
            context.setHistoryIndex(pid, nextIndex);
            if (!context.applyHistoryEntryAtIndex(pid, nextIndex)) {
                return context.errorResponse("REDO_FAILED", "Failed to apply redo");
            }
        } finally {
            context.setHistorySuspended(pid, false);
            context.setCommandLogSuspended(pid, false);
        }

        return buildPostApplyResponse(context, project, pid, broadcaster);
    }

    private JSONObject buildPostApplyResponse(final Context context,
                                              final RunTimeProject project,
                                              final String projectId,
                                              final Consumer<String> broadcaster) {
        context.saveHistoryToDisk(projectId);
        SceneFlow sceneFlow = project.getSceneFlow();
        JSONObject snapshot = context.createSceneFlowSnapshot(project, projectId, sceneFlow, sceneFlow);
        context.broadcastSceneFlowSnapshot(broadcaster, projectId, snapshot);
        JSONObject scriptSnapshot = context.buildScriptSnapshot(projectId);
        context.broadcastScriptSnapshot(broadcaster, projectId, scriptSnapshot);

        JSONObject response = context.buildSceneFlowResponse(snapshot);
        response.put("script", scriptSnapshot);
        response.put("applied", true);
        if (broadcaster != null) {
            JSONObject dirtyEvt = new JSONObject();
            dirtyEvt.put("type", "event");
            dirtyEvt.put("event", "project.dirty");
            dirtyEvt.put("projectId", projectId);
            dirtyEvt.put("areas", new JSONArray().put("sceneflow").put("script"));
            broadcaster.accept(dirtyEvt.toString());
        }
        return response;
    }
}
