package de.dfki.vsm.web;

import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.VsmExecutionHistory;
import org.json.JSONObject;

import java.util.function.Consumer;

/**
 * Handles runtime lifecycle commands so transport layers can delegate
 * without duplicating lifecycle logic.
 */
public final class RuntimeCommandService {

    public interface Context {
        boolean loadProject(String path);

        String firstLoadedProjectId();

        RunTimeProject runtimeProject(String projectId);

        String runtimeState(String projectId);

        void setRuntimeState(String projectId, String state);

        String projectPath(String projectId);

        String projectName(String projectId);

        void removeProject(String projectId);

        /** Phase 4 (doc/vsm-workspace-platform-plan.md) — see PortPoolManager's class docs. */
        void ensurePortsAllocated(String projectId);

        JSONObject errorResponse(String code, String message);

        void addRuntimeCapabilities(JSONObject target);

        void log(String message);

        JSONObject runtimeVariableSet(String projectId, String name, String valueExpr);

        JSONObject runtimeQuery(String projectId, String query);
    }

    public JSONObject dispatchRuntimeCommand(final String method,
                                             final JSONObject params,
                                             final Consumer<String> broadcaster,
                                             final Context context) {
        if ("Runtime.Variable.Set".equals(method)) {
            return handleVariableSet(params, context);
        }
        if ("Runtime.Query".equals(method)) {
            return handleQuery(params, context);
        }
        return dispatchLifecycle(method, params, broadcaster, context);
    }

    public JSONObject dispatchLifecycle(final String method,
                                        final JSONObject params,
                                        final Consumer<String> broadcaster,
                                        final Context context) {
        if ("Runtime.Load".equals(method)) {
            return handleLoad(params, context);
        }
        if ("Runtime.Unload".equals(method)) {
            return handleUnload(params, broadcaster, context);
        }
        if ("Runtime.Play".equals(method)
                || "Runtime.Start".equals(method)
                || "Runtime.Resume".equals(method)
                || "Runtime.Pause".equals(method)
                || "Runtime.Stop".equals(method)) {
            return handleRuntimeControl(method, params, broadcaster, context);
        }
        return context.errorResponse("BAD_REQUEST", "Unsupported runtime command: " + method);
    }

    private JSONObject handleVariableSet(final JSONObject params, final Context context) {
        final String pid = params.optString("projectId", "");
        final String name = params.optString("name", "");
        String valueExpr = params.optString("value", "");
        if (valueExpr.isBlank()) {
            valueExpr = params.optString("valueExpr", "");
        }
        if (name.isBlank() || valueExpr.isBlank()) {
            return context.errorResponse("BAD_REQUEST", "Missing name or value");
        }
        return context.runtimeVariableSet(pid, name, valueExpr);
    }

    private JSONObject handleQuery(final JSONObject params, final Context context) {
        final String pid = params.optString("projectId", "");
        final String query = params.optString("query", "");
        if (query.isBlank()) {
            return context.errorResponse("BAD_REQUEST", "Missing query");
        }
        return context.runtimeQuery(pid, query);
    }

    private JSONObject handleLoad(final JSONObject params, final Context context) {
        final String path = params.optString("projectPath", "");
        if (path.isBlank()) {
            return context.errorResponse("BAD_REQUEST", "Missing projectPath");
        }
        if (!context.loadProject(path)) {
            return context.errorResponse("LOAD_FAILED", "Failed to load project");
        }
        final String pid = context.firstLoadedProjectId();
        final RunTimeProject project = pid != null ? context.runtimeProject(pid) : null;
        if (pid == null || project == null) {
            return context.errorResponse("LOAD_FAILED", "Project was loaded but runtime metadata is unavailable");
        }

        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("projectId", pid);
        response.put("state", safeState(context.runtimeState(pid)));
        response.put("projectPath", context.projectPath(pid));
        response.put("projectName", context.projectName(pid));
        context.addRuntimeCapabilities(response);
        return response;
    }

    private JSONObject handleUnload(final JSONObject params,
                                    final Consumer<String> broadcaster,
                                    final Context context) {
        final String pid = params.optString("projectId", "");
        final RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found: " + pid);
        }
        if (project.wasExecuted()) {
            project.unload();
        }
        context.removeProject(pid);
        broadcastState(broadcaster, pid, "stopped");

        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("state", "stopped");
        response.put("projectId", pid);
        context.addRuntimeCapabilities(response);
        return response;
    }

    private JSONObject handleRuntimeControl(final String method,
                                            final JSONObject params,
                                            final Consumer<String> broadcaster,
                                            final Context context) {
        final String pid = params.optString("projectId", "");
        final RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found: " + pid);
        }

        boolean success = false;
        String newState = safeState(context.runtimeState(pid));

        if ("Runtime.Play".equals(method) || "Runtime.Start".equals(method)) {
            if (project.isRunning()) {
                if (project.isPaused()) {
                    success = project.proceed();
                    newState = success ? "running" : "paused";
                } else {
                    success = true;
                    newState = "running";
                }
            } else {
                // Phase 4: must run before launch() — see PortPoolManager's class docs on why
                // this is a no-op on any call after the project's very first launch.
                context.ensurePortsAllocated(pid);
                boolean launched = project.launch();
                if (launched) {
                    success = project.start();
                    newState = success ? "running" : "stopped";
                    if (success) {
                        VsmExecutionHistory.recordExecution(
                                project.getProjectConfig().getProjectUUID());
                    }
                }
            }
        } else if ("Runtime.Resume".equals(method)) {
            if (project.isRunning() && project.isPaused()) {
                success = project.proceed();
                newState = success ? "running" : "paused";
            } else {
                success = false;
                newState = project.isPaused() ? "paused" : (project.isRunning() ? "running" : "stopped");
            }
        } else if ("Runtime.Pause".equals(method)) {
            if (project.isRunning() && !project.isPaused()) {
                success = project.pause();
                newState = success ? "paused" : "running";
            } else {
                success = true;
                newState = project.isPaused() ? "paused" : (project.isRunning() ? "running" : "stopped");
            }
        } else if ("Runtime.Stop".equals(method)) {
            if (project.isRunning()) {
                success = project.abort();
                if (success) {
                    project.unload();
                }
                newState = "stopped";
            } else {
                success = true;
                newState = "stopped";
            }
        }

        context.setRuntimeState(pid, newState);
        context.log("[RUNTIME] Final state: " + newState + ", success=" + success);
        broadcastState(broadcaster, pid, newState);

        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("state", newState);
        response.put("projectId", pid);
        context.addRuntimeCapabilities(response);
        return response;
    }

    private void broadcastState(final Consumer<String> broadcaster, final String projectId, final String state) {
        if (broadcaster == null) {
            return;
        }
        JSONObject payload = new JSONObject();
        payload.put("state", state);
        payload.put("status", state);
        if (projectId != null) {
            payload.put("projectId", projectId);
        }
        JSONObject evt = new JSONObject();
        evt.put("type", "event");
        evt.put("event", "runtime.state");
        evt.put("payload", payload);
        broadcaster.accept(evt.toString());
    }

    private String safeState(final String state) {
        return state == null || state.isBlank() ? "stopped" : state;
    }
}
