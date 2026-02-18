package de.dfki.vsm.runtime.api.android;

import de.dfki.vsm.model.project.ProjectConfig;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.BoolLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.FloatLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.IntLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.StringLiteral;
import de.dfki.vsm.runtime.CoreRuntime;
import de.dfki.vsm.runtime.api.RuntimeWsProtocol;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.function.Consumer;

/**
 * Runtime command endpoint for a single hosted runtime instance.
 */
public final class AndroidRuntimeEndpoint implements AndroidRuntimeApi {

    private final Object lock = new Object();
    private final CoreRuntime runtime;
    private final String projectId;
    private final String projectPath;
    private final String projectName;
    private volatile String runtimeState = "stopped";

    public AndroidRuntimeEndpoint(final CoreRuntime runtime, final File projectDir) {
        this(runtime, projectDir == null ? "" : projectDir.getAbsolutePath());
    }

    public AndroidRuntimeEndpoint(final CoreRuntime runtime, final String projectPath) {
        this.runtime = Objects.requireNonNull(runtime, "runtime");
        this.projectPath = projectPath == null ? "" : projectPath;
        this.projectId = UUID.randomUUID().toString();

        String name = "project";
        try {
            RunTimeProject rtp = runtime.getRunTimeProject();
            ProjectConfig cfg = rtp == null ? null : rtp.getProjectConfig();
            if (cfg != null && cfg.getProjectName() != null && !cfg.getProjectName().isBlank()) {
                name = cfg.getProjectName();
            }
        } catch (Exception ignored) {
            // Keep default project name.
        }
        this.projectName = name;
    }

    @Override
    public String projectId() {
        return projectId;
    }

    @Override
    public String projectName() {
        return projectName;
    }

    @Override
    public String projectPath() {
        return projectPath;
    }

    @Override
    public String runtimeState() {
        return runtimeState;
    }

    @Override
    public JSONObject runtimeSnapshot() {
        synchronized (lock) {
            JSONObject payload = new JSONObject();
            payload.put("state", runtimeState);
            payload.put("projectId", projectId);
            payload.put("project", projectPath);
            payload.put("globalVariables", collectVariables());
            payload.put("localVariables", new JSONArray());
            return payload;
        }
    }

    @Override
    public RunTimeProject runtimeProject() {
        return runtime.getRunTimeProject();
    }

    @Override
    public JSONObject snapshot(final String requestProjectId) {
        if (!matchesProject(requestProjectId)) {
            return error("PROJECT_NOT_FOUND", "Project not found: " + requestProjectId);
        }
        return runtimeSnapshot();
    }

    @Override
    public JSONObject dispatchCommand(final String method,
                                      final JSONObject params,
                                      final Consumer<String> broadcaster) {
        final String cmd = method == null ? "" : method.trim();
        final JSONObject safeParams = params == null ? new JSONObject() : params;

        if (!isRuntimeMethod(cmd)) {
            return error("BAD_REQUEST", "Unsupported runtime command: " + cmd);
        }
        if (!matchesProject(safeParams.optString("projectId", projectId))) {
            return error("PROJECT_NOT_FOUND", "Project not found: " + safeParams.optString("projectId", ""));
        }

        synchronized (lock) {
            switch (cmd) {
                case "Runtime.Load":
                    return loadResponse();
                case "Runtime.Play":
                case "Runtime.Start":
                    return startRuntime(broadcaster);
                case "Runtime.Resume":
                    return resumeRuntime(broadcaster);
                case "Runtime.Pause":
                    return pauseRuntime(broadcaster);
                case "Runtime.Stop":
                case "Runtime.Unload":
                    return stopRuntime(broadcaster);
                case "Runtime.Variable.Set":
                    return setVariable(safeParams);
                case "Runtime.Query":
                    return error("UNSUPPORTED", "Runtime.Query is not available in Android runtime mode.");
                default:
                    return error("BAD_REQUEST", "Unsupported runtime command: " + cmd);
            }
        }
    }

    private JSONObject loadResponse() {
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("projectId", projectId);
        response.put("state", runtimeState);
        response.put("projectPath", projectPath);
        response.put("projectName", projectName);
        return response;
    }

    private JSONObject startRuntime(final Consumer<String> broadcaster) {
        if (runtime.isRunning()) {
            if ("paused".equals(runtimeState)) {
                if (!runtime.getRunTimeProject().proceed()) {
                    return error("RUNTIME_RESUME_FAILED", "Failed to resume runtime");
                }
            }
            runtimeState = "running";
            broadcastState(broadcaster, runtimeState);
            return okState();
        }

        boolean launched = runtime.launch();
        if (!launched) {
            return error("RUNTIME_LAUNCH_FAILED", "Failed to launch runtime");
        }
        boolean started = runtime.start();
        if (!started) {
            runtimeState = "stopped";
            return error("RUNTIME_START_FAILED", "Failed to start runtime");
        }

        runtimeState = "running";
        broadcastState(broadcaster, runtimeState);
        return okState();
    }

    private JSONObject pauseRuntime(final Consumer<String> broadcaster) {
        if (!runtime.isRunning()) {
            return okState();
        }
        if (!runtime.getRunTimeProject().pause()) {
            return error("RUNTIME_PAUSE_FAILED", "Failed to pause runtime");
        }
        runtimeState = "paused";
        broadcastState(broadcaster, runtimeState);
        return okState();
    }

    private JSONObject resumeRuntime(final Consumer<String> broadcaster) {
        if (!runtime.isRunning()) {
            return error("RUNTIME_NOT_RUNNING", "Runtime is not running");
        }
        if (!runtime.getRunTimeProject().proceed()) {
            return error("RUNTIME_RESUME_FAILED", "Failed to resume runtime");
        }
        runtimeState = "running";
        broadcastState(broadcaster, runtimeState);
        return okState();
    }

    private JSONObject stopRuntime(final Consumer<String> broadcaster) {
        if (runtime.isRunning()) {
            runtime.abort();
        }
        runtime.unload();
        runtimeState = "stopped";
        broadcastState(broadcaster, runtimeState);
        return okState();
    }

    private JSONObject setVariable(final JSONObject params) {
        String name = params.optString("name", "").trim();
        String valueExpr = params.optString("value", "").trim();
        if (valueExpr.isEmpty()) {
            valueExpr = params.optString("valueExpr", "").trim();
        }
        if (name.isEmpty() || valueExpr.isEmpty()) {
            return error("BAD_REQUEST", "Missing name or value");
        }

        if (!runtime.getRunTimeProject().hasVariable(name)) {
            return error("VARIABLE_NOT_FOUND", "Unknown variable: " + name);
        }

        boolean applied = applyVariable(name, valueExpr);
        if (!applied) {
            return error("VARIABLE_SET_FAILED", "Failed to set variable: " + name);
        }

        AbstractValue value = runtime.getRunTimeProject().getValueOf(name);
        JSONObject payload = new JSONObject();
        payload.put("status", "ok");
        payload.put("projectId", projectId);
        payload.put("name", name);
        payload.put("value", value == null ? JSONObject.NULL : value.getValue());
        return payload;
    }

    private boolean applyVariable(final String name, final String rawValue) {
        if ("true".equalsIgnoreCase(rawValue) || "false".equalsIgnoreCase(rawValue)) {
            return runtime.getRunTimeProject().setVariable(name, Boolean.parseBoolean(rawValue));
        }
        try {
            int intValue = Integer.parseInt(rawValue);
            return runtime.getRunTimeProject().setVariable(name, intValue);
        } catch (NumberFormatException ignored) {
            // Try float next.
        }
        try {
            float floatValue = Float.parseFloat(rawValue);
            return runtime.getRunTimeProject().setVariable(name, floatValue);
        } catch (NumberFormatException ignored) {
            // Fallback to string.
        }
        return runtime.getRunTimeProject().setVariable(name, rawValue);
    }

    private JSONArray collectVariables() {
        JSONArray vars = new JSONArray();
        List<VariableDefinition> defs = new ArrayList<>();
        try {
            defs = runtime.getRunTimeProject().getVarDefInSceneFlow();
        } catch (Exception ignored) {
            // Return empty on parse/runtime mismatch.
        }

        for (VariableDefinition def : defs) {
            if (def == null) {
                continue;
            }
            String name = def.getName();
            if (name == null || name.isBlank()) {
                continue;
            }
            AbstractValue value = runtime.getRunTimeProject().getValueOf(name);
            JSONObject row = new JSONObject();
            row.put("name", name);
            row.put("type", def.getType());
            String serialized = serializeVariableValue(value, def);
            row.put("value", serialized == null ? JSONObject.NULL : serialized);
            vars.put(row);
        }
        return vars;
    }

    private String serializeVariableValue(final AbstractValue value, final VariableDefinition def) {
        if (value != null) {
            if (value.getType() == AbstractValue.Type.EVENT) {
                return value.getFormattedSyntax();
            }
            return sanitizeVariableValue(value.getConcreteSyntax());
        }
        return valueFromDefinition(def);
    }

    private String sanitizeVariableValue(final String value) {
        if (value == null) {
            return null;
        }
        return value.replaceAll("#[a-zA-Z]#", "");
    }

    private String valueFromDefinition(final VariableDefinition def) {
        if (def == null) {
            return null;
        }
        Expression exp = def.getExp();
        if (exp instanceof StringLiteral) {
            String value = ((StringLiteral) exp).getValue();
            return "\"" + (value == null ? "" : value) + "\"";
        }
        if (exp instanceof IntLiteral) {
            return Integer.toString(((IntLiteral) exp).getValue());
        }
        if (exp instanceof FloatLiteral) {
            return Float.toString(((FloatLiteral) exp).getValue());
        }
        if (exp instanceof BoolLiteral) {
            return Boolean.toString(((BoolLiteral) exp).getValue());
        }

        String type = def.getType() == null ? "" : def.getType().trim().toLowerCase();
        if ("string".equals(type)) {
            return "\"\"";
        }
        if ("int".equals(type)) {
            return "0";
        }
        if ("float".equals(type)) {
            return "0.0";
        }
        if ("bool".equals(type) || "boolean".equals(type)) {
            return "false";
        }
        return null;
    }

    private JSONObject okState() {
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("state", runtimeState);
        response.put("projectId", projectId);
        return response;
    }

    private void broadcastState(final Consumer<String> broadcaster, final String state) {
        if (broadcaster == null) {
            return;
        }
        JSONObject payload = new JSONObject();
        payload.put("projectId", projectId);
        payload.put("state", state);
        payload.put("status", state);
        emitEvent(broadcaster, "runtime", "runtime.state", payload);
    }

    private void emitEvent(final Consumer<String> broadcaster,
                           final String channel,
                           final String event,
                           final JSONObject payload) {
        JSONObject message = new JSONObject();
        message.put("type", "event");
        message.put("ts", System.currentTimeMillis());
        message.put("channel", channel);
        message.put("event", event);
        message.put("payload", payload == null ? new JSONObject() : payload);
        broadcaster.accept(message.toString());
    }

    private boolean matchesProject(final String candidate) {
        // Android runtime host is single-project; accept any incoming projectId to avoid stale-id no-op commands.
        return true;
    }

    private boolean isRuntimeMethod(final String cmd) {
        return "Runtime.Load".equals(cmd)
                || "Runtime.Play".equals(cmd)
                || "Runtime.Start".equals(cmd)
                || "Runtime.Resume".equals(cmd)
                || "Runtime.Pause".equals(cmd)
                || "Runtime.Stop".equals(cmd)
                || "Runtime.Unload".equals(cmd)
                || "Runtime.Variable.Set".equals(cmd)
                || "Runtime.Query".equals(cmd);
    }

    private JSONObject error(final String code, final String message) {
        JSONObject payload = new JSONObject();
        payload.put("status", "error");
        payload.put("code", code);
        payload.put("message", message);
        return payload;
    }
}
