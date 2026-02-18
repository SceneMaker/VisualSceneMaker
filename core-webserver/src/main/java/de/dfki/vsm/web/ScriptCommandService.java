package de.dfki.vsm.web;

import de.dfki.vsm.model.scenescript.ScriptDiagnostics;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.function.Consumer;

/**
 * Handles Script.Update command.
 */
public final class ScriptCommandService {

    public interface Context {
        RunTimeProject runtimeProject(String projectId);

        JSONObject errorResponse(String code, String message);

        void ensureScriptLoaded(String projectId);

        int scriptVersion(String projectId);

        String scriptText(String projectId);

        boolean scriptParseOk(String projectId);

        JSONArray scriptParseErrors(String projectId);

        String serializeSceneScript(String projectId);

        boolean applyScriptText(String projectId, String text);

        void setScriptText(String projectId, String text);

        void setScriptVersion(String projectId, int version);

        void setScriptParseOk(String projectId, boolean value);

        void clearScriptParseErrors(String projectId);

        void markDirty(String projectId);

        void broadcastScriptSnapshot(Consumer<String> broadcaster, String projectId, JSONObject snapshot);

        void recordHistory(String projectId, String action);

        void recordCommand(String projectId, String action, JSONObject params);

        JSONArray diagnosticsToJson(java.util.List<ScriptDiagnostics.Diagnostic> diagnostics);
    }

    public JSONObject dispatch(final JSONObject params,
                               final Consumer<String> broadcaster,
                               final Context context) {
        String pid = params.optString("projectId", "");
        RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }

        context.ensureScriptLoaded(pid);
        String text = params.optString("text", "");
        if (params.has("version")) {
            int clientVersion = params.optInt("version", context.scriptVersion(pid));
            if (clientVersion != context.scriptVersion(pid)) {
                JSONObject mismatch = new JSONObject();
                mismatch.put("applied", false);
                mismatch.put("reason", "VERSION_MISMATCH");
                mismatch.put("version", context.scriptVersion(pid));
                mismatch.put("text", context.scriptText(pid));
                mismatch.put("parseOk", context.scriptParseOk(pid));
                mismatch.put("parseErrors", context.scriptParseErrors(pid));
                return mismatch;
            }
        }

        String currentText = context.scriptText(pid);
        String previousText = currentText == null ? context.serializeSceneScript(pid) : currentText;
        boolean ok = context.applyScriptText(pid, text);
        if (!ok) {
            context.applyScriptText(pid, previousText);
            ScriptDiagnostics.Result result = ScriptDiagnostics.analyze(text);
            JSONObject failed = new JSONObject();
            failed.put("applied", false);
            failed.put("reason", "PARSE_FAILED");
            failed.put("parseOk", result.isParseOk());
            failed.put("parseErrors", context.diagnosticsToJson(result.getDiagnostics()));
            return failed;
        }

        context.setScriptText(pid, text);
        context.setScriptVersion(pid, Math.max(1, context.scriptVersion(pid) + 1));
        context.setScriptParseOk(pid, true);
        context.clearScriptParseErrors(pid);
        context.markDirty(pid);

        JSONObject response = new JSONObject();
        response.put("applied", true);
        response.put("text", context.scriptText(pid));
        response.put("version", context.scriptVersion(pid));
        response.put("parseOk", context.scriptParseOk(pid));
        response.put("parseErrors", context.scriptParseErrors(pid));
        if (broadcaster != null) {
            context.broadcastScriptSnapshot(broadcaster, pid, response);
            JSONObject dirtyEvt = new JSONObject();
            dirtyEvt.put("event", "project.dirty");
            dirtyEvt.put("projectId", pid);
            dirtyEvt.put("areas", new JSONArray().put("script"));
            broadcaster.accept(dirtyEvt.toString());
        }
        context.recordHistory(pid, "Script.Update");
        context.recordCommand(pid, "Script.Update", params);
        return response;
    }
}
