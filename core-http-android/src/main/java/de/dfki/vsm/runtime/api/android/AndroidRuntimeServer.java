package de.dfki.vsm.runtime.api.android;

import fi.iki.elonen.NanoHTTPD;
import fi.iki.elonen.NanoWSD;
import de.dfki.vsm.Preferences;
import de.dfki.vsm.model.config.ConfigElement;
import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.model.project.LLMConfig;
import de.dfki.vsm.model.project.PlayerConfig;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.project.ProjectConfig;
import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.scenescript.SceneObject;
import de.dfki.vsm.model.scenescript.SceneScript;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.Consumer;

/**
 * Reusable Android-friendly HTTP/WebSocket runtime server.
 */
public final class AndroidRuntimeServer extends NanoWSD {

    private final AndroidRuntimeApi endpoint;
    private final AndroidRuntimeWsAdapter wsAdapter;
    private final String authToken;
    private final AndroidRuntimeEventBridge eventBridge;

    public AndroidRuntimeServer(final int port,
                                final AndroidRuntimeApi endpoint,
                                final String authToken) {
        super(port);
        this.endpoint = Objects.requireNonNull(endpoint, "endpoint");
        this.wsAdapter = new AndroidRuntimeWsAdapter(endpoint);
        this.authToken = authToken == null ? "" : authToken.trim();
        this.eventBridge = new AndroidRuntimeEventBridge(
                endpoint::projectId,
                () -> wsAdapter.sessions().broadcaster()
        );
    }

    public void startServer() throws IOException {
        // Use infinite read timeout so WebSocket connections are not dropped after the default 5s idle timeout.
        start(0, false);
        eventBridge.start();
    }

    public void stopServer() {
        eventBridge.stop();
        wsAdapter.sessions().shutdown();
        stop();
    }

    public JSONObject dispatchRuntimeCommand(final String method) {
        JSONObject params = new JSONObject();
        params.put("projectId", endpoint.projectId());
        return endpoint.dispatchCommand(method, params, wsAdapter.sessions().broadcaster());
    }

    public Consumer<String> broadcaster() {
        return wsAdapter.sessions().broadcaster();
    }

    @Override
    public Response serve(final IHTTPSession session) {
        if (Method.OPTIONS.equals(session.getMethod())) {
            Response preflight = NanoHTTPD.newFixedLengthResponse(Response.Status.OK, "application/json", "{\"status\":\"ok\"}");
            addCorsHeaders(preflight, session);
            return preflight;
        }
        Response response = super.serve(session);
        addCorsHeaders(response, session);
        return response;
    }

    @Override
    protected WebSocket openWebSocket(final IHTTPSession handshake) {
        return new WsSession(handshake);
    }

    @Override
    protected Response serveHttp(final IHTTPSession session) {
        if (!isAuthorized(session)) {
            return jsonResponse(Response.Status.UNAUTHORIZED, error("UNAUTHORIZED", "Missing or invalid token"));
        }

        final String uri = normalizeUri(session.getUri());
        final Method method = session.getMethod();

        if (Method.GET.equals(method) && "/api/v1/info".equals(uri)) {
            JSONObject info = new JSONObject();
            info.put("name", "VSM Android Runtime");
            info.put("version", "android");
            info.put("revision", "android");
            info.put("mode", "runtime_only");
            info.put("tokenRequired", !authToken.isEmpty());
            info.put("projectId", endpoint.projectId());
            info.put("projectName", endpoint.projectName());
            return jsonResponse(Response.Status.OK, info);
        }

        if (Method.GET.equals(method) && "/api/v1/token".equals(uri)) {
            JSONObject token = new JSONObject();
            token.put("token", authToken);
            token.put("tokenRequired", !authToken.isEmpty());
            return jsonResponse(Response.Status.OK, token);
        }

        if (Method.GET.equals(method) && "/api/v1/preferences".equals(uri)) {
            JSONObject payload = new JSONObject();
            JSONObject prefs = new JSONObject();
            try {
                for (Object keyObj : Preferences.getKeySet()) {
                    if (keyObj == null) {
                        continue;
                    }
                    String key = String.valueOf(keyObj);
                    prefs.put(key, Preferences.getProperty(key));
                }
            } catch (Exception ignored) {
                // Keep empty preferences object when unavailable.
            }
            payload.put("preferences", prefs);
            return jsonResponse(Response.Status.OK, payload);
        }

        if (Method.GET.equals(method) && "/api/v1/projects/recent".equals(uri)) {
            JSONObject payload = new JSONObject();
            payload.put("recent", new JSONArray());
            return jsonResponse(Response.Status.OK, payload);
        }

        if (Method.GET.equals(method) && "/api/v1/projects/tutorials".equals(uri)) {
            JSONObject payload = new JSONObject();
            payload.put("tutorials", new JSONArray());
            return jsonResponse(Response.Status.OK, payload);
        }

        if (Method.GET.equals(method) && "/api/v1/projects/samples".equals(uri)) {
            JSONObject payload = new JSONObject();
            payload.put("samples", new JSONArray());
            return jsonResponse(Response.Status.OK, payload);
        }

        if (Method.GET.equals(method) && "/api/v1/projects".equals(uri)) {
            JSONObject payload = new JSONObject();
            JSONArray projects = new JSONArray();
            JSONObject project = new JSONObject();
            project.put("projectId", endpoint.projectId());
            project.put("name", endpoint.projectName());
            project.put("path", endpoint.projectPath());
            project.put("dirty", false);
            project.put("pending", false);
            projects.put(project);
            payload.put("projects", projects);
            return jsonResponse(Response.Status.OK, payload);
        }

        if (Method.GET.equals(method) && "/api/v1/devices".equals(uri)) {
            JSONObject payload = new JSONObject();
            JSONArray devices = new JSONArray();
            ProjectConfig cfg = endpoint.runtimeProject().getProjectConfig();
            if (cfg != null) {
                for (PluginConfig plugin : cfg.getPluginConfigList()) {
                    if (plugin == null) {
                        continue;
                    }
                    JSONObject dev = new JSONObject();
                    dev.put("name", plugin.getPluginName() == null ? "" : plugin.getPluginName());
                    dev.put("className", plugin.getClassName() == null ? "" : plugin.getClassName());
                    devices.put(dev);
                }
            }
            payload.put("devices", devices);
            return jsonResponse(Response.Status.OK, payload);
        }

        if (Method.GET.equals(method)
                && uri.startsWith("/api/v1/projects/")
                && uri.endsWith("/runtime")) {
            String pid = extractProjectId(uri, "/runtime");
            return jsonResponse(Response.Status.OK, endpoint.snapshot(pid));
        }

        if (Method.GET.equals(method)
                && uri.startsWith("/api/v1/projects/")
                && uri.endsWith("/sceneflow")) {
            String pid = extractProjectId(uri, "/sceneflow");
            if (!pid.isEmpty() && !endpoint.projectId().equals(pid)) {
                return jsonResponse(Response.Status.NOT_FOUND, error("PROJECT_NOT_FOUND", "Project not found: " + pid));
            }
            String requestedSuperNodeId = session.getParms().getOrDefault("superNodeId", "");
            return jsonResponse(Response.Status.OK, sceneFlowSnapshot(requestedSuperNodeId));
        }

        if (Method.GET.equals(method)
                && uri.startsWith("/api/v1/projects/")
                && uri.endsWith("/script")) {
            String pid = extractProjectId(uri, "/script");
            if (!pid.isEmpty() && !endpoint.projectId().equals(pid)) {
                return jsonResponse(Response.Status.NOT_FOUND, error("PROJECT_NOT_FOUND", "Project not found: " + pid));
            }
            SceneScript script = endpoint.runtimeProject().getSceneScript();
            JSONObject payload = new JSONObject();
            payload.put("text", script == null ? "" : script.getText());
            payload.put("version", script == null ? 1 : script.getHashCode());
            payload.put("parseOk", true);
            payload.put("parseErrors", new JSONArray());
            return jsonResponse(Response.Status.OK, payload);
        }

        if (Method.GET.equals(method)
                && uri.startsWith("/api/v1/projects/")
                && uri.endsWith("/script/scenes")) {
            String pid = extractProjectId(uri, "/script/scenes");
            if (!pid.isEmpty() && !endpoint.projectId().equals(pid)) {
                return jsonResponse(Response.Status.NOT_FOUND, error("PROJECT_NOT_FOUND", "Project not found: " + pid));
            }
            JSONObject response = new JSONObject();
            JSONArray languages = new JSONArray();
            SceneScript script = endpoint.runtimeProject().getSceneScript();
            if (script != null) {
                Map<String, Map<String, Integer>> grouped = new TreeMap<>();
                for (SceneObject scene : script.getSceneList()) {
                    if (scene == null) continue;
                    String language = scene.getLanguage();
                    String name = scene.getName();
                    String langKey = language == null ? "" : language.trim();
                    String nameKey = name == null ? "" : name.trim();
                    if (nameKey.isEmpty()) {
                        continue;
                    }
                    grouped.computeIfAbsent(langKey, key -> new TreeMap<>())
                            .merge(nameKey, 1, Integer::sum);
                }
                for (Map.Entry<String, Map<String, Integer>> langEntry : grouped.entrySet()) {
                    JSONObject langJson = new JSONObject();
                    langJson.put("language", langEntry.getKey());
                    JSONArray groups = new JSONArray();
                    for (Map.Entry<String, Integer> groupEntry : langEntry.getValue().entrySet()) {
                        JSONObject groupJson = new JSONObject();
                        groupJson.put("name", groupEntry.getKey());
                        groupJson.put("count", groupEntry.getValue());
                        groups.put(groupJson);
                    }
                    langJson.put("groups", groups);
                    languages.put(langJson);
                }
            }
            response.put("languages", languages);
            return jsonResponse(Response.Status.OK, response);
        }

        if (Method.GET.equals(method)
                && uri.startsWith("/api/v1/projects/")
                && uri.endsWith("/script/elements")) {
            String pid = extractProjectId(uri, "/script/elements");
            if (!pid.isEmpty() && !endpoint.projectId().equals(pid)) {
                return jsonResponse(Response.Status.NOT_FOUND, error("PROJECT_NOT_FOUND", "Project not found: " + pid));
            }
            JSONObject response = new JSONObject();
            response.put("acticon", new JSONArray());
            response.put("gesticon", new JSONArray());
            response.put("visicon", new JSONArray());
            return jsonResponse(Response.Status.OK, response);
        }

        if (Method.GET.equals(method)
                && uri.startsWith("/api/v1/projects/")
                && uri.endsWith("/semantic")) {
            String pid = extractProjectId(uri, "/semantic");
            if (!pid.isEmpty() && !endpoint.projectId().equals(pid)) {
                return jsonResponse(Response.Status.NOT_FOUND, error("PROJECT_NOT_FOUND", "Project not found: " + pid));
            }
            JSONObject response = new JSONObject();
            response.put("version", 1);
            response.put("annotations", new JSONArray());
            return jsonResponse(Response.Status.OK, response);
        }

        if (Method.GET.equals(method)
                && uri.startsWith("/api/v1/projects/")
                && uri.endsWith("/validate/vars")) {
            String pid = extractProjectId(uri, "/validate/vars");
            if (!pid.isEmpty() && !endpoint.projectId().equals(pid)) {
                return jsonResponse(Response.Status.NOT_FOUND, error("PROJECT_NOT_FOUND", "Project not found: " + pid));
            }
            JSONObject response = new JSONObject();
            response.put("missing", new JSONArray());
            return jsonResponse(Response.Status.OK, response);
        }

        if (Method.GET.equals(method)
                && uri.startsWith("/api/v1/projects/")
                && uri.endsWith("/config")) {
            String pid = extractProjectId(uri, "/config");
            if (!pid.isEmpty() && !endpoint.projectId().equals(pid)) {
                return jsonResponse(Response.Status.NOT_FOUND, error("PROJECT_NOT_FOUND", "Project not found: " + pid));
            }
            JSONObject response = new JSONObject();
            response.put("config", loadEditorConfig(endpoint.projectPath()));
            return jsonResponse(Response.Status.OK, response);
        }

        if (Method.GET.equals(method)
                && uri.startsWith("/api/v1/projects/")
                && uri.endsWith("/project-config")) {
            String pid = extractProjectId(uri, "/project-config");
            if (!pid.isEmpty() && !endpoint.projectId().equals(pid)) {
                return jsonResponse(Response.Status.NOT_FOUND, error("PROJECT_NOT_FOUND", "Project not found: " + pid));
            }
            JSONObject response = new JSONObject();
            response.put("config", projectConfigToJson(endpoint.runtimeProject().getProjectConfig(), endpoint.projectPath()));
            return jsonResponse(Response.Status.OK, response);
        }

        if (Method.GET.equals(method)
                && uri.startsWith("/api/v1/projects/")
                && uri.endsWith("/plugin-interfaces")) {
            String pid = extractProjectId(uri, "/plugin-interfaces");
            if (!pid.isEmpty() && !endpoint.projectId().equals(pid)) {
                return jsonResponse(Response.Status.NOT_FOUND, error("PROJECT_NOT_FOUND", "Project not found: " + pid));
            }
            JSONObject response = new JSONObject();
            response.put("interfaces", new JSONArray());
            response.put("errors", new JSONArray());
            response.put("source", "android-runtime");
            return jsonResponse(Response.Status.OK, response);
        }

        if (Method.GET.equals(method)
                && uri.startsWith("/api/v1/projects/")
                && uri.endsWith("/project-config/keys")) {
            String pid = extractProjectId(uri, "/project-config/keys");
            if (!pid.isEmpty() && !endpoint.projectId().equals(pid)) {
                return jsonResponse(Response.Status.NOT_FOUND, error("PROJECT_NOT_FOUND", "Project not found: " + pid));
            }
            JSONObject response = new JSONObject();
            response.put("device", session.getParms().getOrDefault("device", ""));
            response.put("className", session.getParms().getOrDefault("className", ""));
            response.put("scope", session.getParms().getOrDefault("scope", "plugin"));
            response.put("supported", false);
            response.put("required", new JSONArray());
            response.put("optional", new JSONArray());
            response.put("pluginSpecific", new JSONArray());
            return jsonResponse(Response.Status.OK, response);
        }

        if (Method.POST.equals(method)
                && uri.startsWith("/api/v1/projects/")
                && uri.endsWith("/sceneflow/navigate")) {
            String pid = extractProjectId(uri, "/sceneflow/navigate");
            if (!pid.isEmpty() && !endpoint.projectId().equals(pid)) {
                return jsonResponse(Response.Status.NOT_FOUND, error("PROJECT_NOT_FOUND", "Project not found: " + pid));
            }
            JSONObject body = parseJsonBody(session);
            String requestedSuperNodeId = body.optString("superNodeId", "");
            return jsonResponse(Response.Status.OK, sceneFlowSnapshot(requestedSuperNodeId));
        }

        if (Method.GET.equals(method) && "/api/v1/runtime/status".equals(uri)) {
            JSONObject payload = new JSONObject();
            payload.put("status", "ok");
            payload.put("state", endpoint.runtimeState());
            payload.put("projectId", endpoint.projectId());
            return jsonResponse(Response.Status.OK, payload);
        }

        if (Method.GET.equals(method) && "/api/v1/runtime/variables".equals(uri)) {
            return jsonResponse(Response.Status.OK, endpoint.runtimeSnapshot());
        }

        if (Method.POST.equals(method) && "/api/v1/runtime/load".equals(uri)) {
            JSONObject body = parseJsonBody(session);
            JSONObject params = new JSONObject();
            params.put("projectId", endpoint.projectId());
            params.put("projectPath", body.optString("projectPath", endpoint.projectPath()));
            return jsonResponse(Response.Status.OK, endpoint.dispatchCommand("Runtime.Load", params, wsAdapter.sessions().broadcaster()));
        }

        if (Method.POST.equals(method) && "/api/v1/runtime/start".equals(uri)) {
            return jsonResponse(Response.Status.OK, command("Runtime.Start"));
        }
        if (Method.POST.equals(method) && "/api/v1/runtime/pause".equals(uri)) {
            return jsonResponse(Response.Status.OK, command("Runtime.Pause"));
        }
        if (Method.POST.equals(method) && "/api/v1/runtime/resume".equals(uri)) {
            return jsonResponse(Response.Status.OK, command("Runtime.Resume"));
        }
        if (Method.POST.equals(method) && "/api/v1/runtime/stop".equals(uri)) {
            return jsonResponse(Response.Status.OK, command("Runtime.Stop"));
        }
        if (Method.POST.equals(method) && "/api/v1/runtime/unload".equals(uri)) {
            return jsonResponse(Response.Status.OK, command("Runtime.Unload"));
        }

        if (Method.POST.equals(method) && "/api/v1/command".equals(uri)) {
            JSONObject body = parseJsonBody(session);
            String cmd = body.optString("method", body.optString("name", ""));
            JSONObject params = body.optJSONObject("params");
            if (params == null) {
                params = body.optJSONObject("payload");
            }
            if (params == null) {
                params = new JSONObject();
            }
            if (!params.has("projectId")) {
                params.put("projectId", endpoint.projectId());
            }
            return jsonResponse(Response.Status.OK, endpoint.dispatchCommand(cmd, params, wsAdapter.sessions().broadcaster()));
        }

        return jsonResponse(Response.Status.NOT_FOUND, error("NOT_FOUND", "No route for " + method + " " + uri));
    }

    private JSONObject loadEditorConfig(final String projectPath) {
        Properties props = new Properties();
        File file = new File(projectPath, "editorconfig.xml");
        if (file.exists()) {
            try (FileInputStream in = new FileInputStream(file)) {
                props.loadFromXML(in);
            } catch (Exception ignored) {
                // Fallback to defaults below.
            }
        }

        JSONObject out = new JSONObject();
        if (props.isEmpty()) {
            out.put("autosave", "true");
            out.put("workspace_fontsize", Preferences.getProperty("workspace_fontsize"));
            out.put("node_width", Preferences.getProperty("node_width"));
            out.put("node_height", Preferences.getProperty("node_height"));
            return out;
        }
        Set<String> names = props.stringPropertyNames();
        for (String key : names) {
            out.put(key, props.getProperty(key));
        }
        return out;
    }

    private JSONObject projectConfigToJson(final ProjectConfig cfg, final String path) {
        JSONObject cfgJson = new JSONObject();
        if (cfg == null) {
            cfgJson.put("name", "");
            cfgJson.put("path", path == null ? "" : path);
            cfgJson.put("plugins", new JSONArray());
            cfgJson.put("agents", new JSONArray());
            cfgJson.put("llms", new JSONArray());
            JSONObject player = new JSONObject();
            player.put("features", new JSONArray());
            cfgJson.put("player", player);
            cfgJson.put("llmPrompts", new JSONObject());
            cfgJson.put("llmSelections", new JSONObject());
            cfgJson.put("semanticServices", new JSONObject());
            cfgJson.put("sceneTitleConcepts", new JSONArray());
            return cfgJson;
        }

        cfgJson.put("name", cfg.getProjectName() == null ? "" : cfg.getProjectName());
        cfgJson.put("path", path == null ? "" : path);

        JSONArray pluginsJson = new JSONArray();
        for (PluginConfig plugin : cfg.getPluginConfigList()) {
            if (plugin == null) continue;
            JSONObject entry = new JSONObject();
            entry.put("type", plugin.getPluginType() == null ? "" : plugin.getPluginType());
            entry.put("name", plugin.getPluginName() == null ? "" : plugin.getPluginName());
            entry.put("className", plugin.getClassName() == null ? "" : plugin.getClassName());
            entry.put("load", plugin.isMarkedtoLoad());
            entry.put("features", configFeaturesToJson(plugin.getEntryList()));
            pluginsJson.put(entry);
        }
        cfgJson.put("plugins", pluginsJson);

        JSONArray agentsJson = new JSONArray();
        for (AgentConfig agent : cfg.getAgentConfigList()) {
            if (agent == null) continue;
            JSONObject entry = new JSONObject();
            entry.put("name", agent.getAgentName() == null ? "" : agent.getAgentName());
            entry.put("device", agent.getDeviceName() == null ? "" : agent.getDeviceName());
            entry.put("features", configFeaturesToJson(agent.getEntryList()));
            agentsJson.put(entry);
        }
        cfgJson.put("agents", agentsJson);

        JSONArray llmsJson = new JSONArray();
        for (LLMConfig llm : cfg.getLLMConfigList()) {
            if (llm == null) continue;
            JSONObject entry = new JSONObject();
            entry.put("name", llm.getLLMName() == null ? "" : llm.getLLMName());
            entry.put("features", configFeaturesToJson(llm.getEntryList()));
            llmsJson.put(entry);
        }
        cfgJson.put("llms", llmsJson);

        JSONObject playerJson = new JSONObject();
        PlayerConfig player = cfg.getPlayerConfig();
        playerJson.put("features", configFeaturesToJson(player != null ? player.getEntryList() : null));
        cfgJson.put("player", playerJson);

        cfgJson.put("llmPrompts", llmPromptsToJson(cfg.getLLMPrompts()));
        cfgJson.put("llmSelections", configElementToKeyValueObject(cfg.getLLMSelections()));
        cfgJson.put("semanticServices", configElementToKeyValueObject(cfg.getSemanticServices()));
        cfgJson.put("sceneTitleConcepts", sceneTitleConceptsToJson(cfg.getSceneTitleConcepts()));
        return cfgJson;
    }

    private JSONArray configFeaturesToJson(final java.util.List<ConfigFeature> features) {
        JSONArray arr = new JSONArray();
        if (features == null) {
            return arr;
        }
        for (ConfigFeature feature : features) {
            if (feature == null) continue;
            JSONObject entry = new JSONObject();
            entry.put("key", feature.getKey() == null ? "" : feature.getKey());
            entry.put("value", feature.getValue() == null ? "" : feature.getValue());
            arr.put(entry);
        }
        return arr;
    }

    private JSONObject configElementToKeyValueObject(final ConfigElement element) {
        JSONObject obj = new JSONObject();
        if (element == null || element.getEntryList() == null) {
            return obj;
        }
        for (ConfigFeature feature : element.getEntryList()) {
            if (feature == null) {
                continue;
            }
            String key = feature.getKey();
            if (key == null || key.isBlank()) {
                continue;
            }
            obj.put(key, feature.getValue() == null ? "" : feature.getValue());
        }
        return obj;
    }

    private JSONObject llmPromptsToJson(final ConfigElement prompts) {
        JSONObject out = new JSONObject();
        out.put("formatPrompt", "");
        out.put("actionPrompts", new JSONArray());
        if (prompts == null || prompts.getEntryList() == null) {
            return out;
        }

        String formatPrompt = "";
        TreeMap<Integer, String> actionPromptByIndex = new TreeMap<>();
        for (ConfigFeature feature : prompts.getEntryList()) {
            if (feature == null) continue;
            String key = feature.getKey() == null ? "" : feature.getKey().trim();
            String value = feature.getValue() == null ? "" : feature.getValue();
            if ("formatPrompt".equals(key)) {
                formatPrompt = value;
                continue;
            }
            if (key.startsWith("actionPrompt.")) {
                try {
                    int idx = Integer.parseInt(key.substring("actionPrompt.".length()));
                    actionPromptByIndex.put(idx, value);
                } catch (NumberFormatException ignored) {
                    // Ignore malformed keys.
                }
            }
        }
        JSONArray actionPrompts = new JSONArray();
        for (String val : actionPromptByIndex.values()) {
            actionPrompts.put(val);
        }
        out.put("formatPrompt", formatPrompt);
        out.put("actionPrompts", actionPrompts);
        return out;
    }

    private JSONArray sceneTitleConceptsToJson(final ConfigElement concepts) {
        JSONArray out = new JSONArray();
        if (concepts == null || concepts.getEntryList() == null) {
            return out;
        }
        for (ConfigFeature feature : concepts.getEntryList()) {
            if (feature == null) {
                continue;
            }
            String value = feature.getValue();
            String key = feature.getKey();
            if (value != null && !value.isBlank()) {
                out.put(value);
            } else if (key != null && !key.isBlank()) {
                out.put(key);
            }
        }
        return out;
    }

    private JSONObject command(final String method) {
        JSONObject params = new JSONObject();
        params.put("projectId", endpoint.projectId());
        return endpoint.dispatchCommand(method, params, wsAdapter.sessions().broadcaster());
    }

    private JSONObject parseJsonBody(final IHTTPSession session) {
        try {
            Map<String, String> files = new HashMap<>();
            session.parseBody(files);
            String body = files.getOrDefault("postData", "").trim();
            if (body.isEmpty()) {
                return new JSONObject();
            }
            return new JSONObject(body);
        } catch (Exception ignored) {
            return new JSONObject();
        }
    }

    private JSONObject sceneFlowSnapshot(final String requestedSuperNodeId) {
        SceneFlow sceneFlow = endpoint.runtimeProject().getSceneFlow();
        if (sceneFlow == null) {
            JSONObject empty = new JSONObject();
            empty.put("projectId", endpoint.projectId());
            empty.put("superNodeId", "");
            empty.put("revision", 0);
            empty.put("nodes", new JSONArray());
            empty.put("edges", new JSONArray());
            empty.put("comments", new JSONArray());
            empty.put("path", new JSONArray());
            empty.put("pathNodes", new JSONArray());
            return empty;
        }

        SuperNode superNode = AndroidSceneFlowSnapshotBuilder.resolveSuperNode(sceneFlow, requestedSuperNodeId);
        if (superNode == null) {
            superNode = sceneFlow;
        }

        JSONObject editorConfig = loadEditorConfig(endpoint.projectPath());
        int nodeWidth = parseInt(editorConfig.optString("node_width", "90"), 90);
        int nodeHeight = parseInt(editorConfig.optString("node_height", "90"), 90);
        return AndroidSceneFlowSnapshotBuilder.createSnapshot(
                endpoint.projectId(),
                superNode,
                sceneFlow,
                nodeWidth,
                nodeHeight,
                null
        );
    }

    private int parseInt(final String text, final int fallback) {
        if (text == null || text.isBlank()) {
            return fallback;
        }
        try {
            return Integer.parseInt(text.trim());
        } catch (NumberFormatException ignored) {
            return fallback;
        }
    }

    private String extractProjectId(final String uri, final String tail) {
        String prefix = "/api/v1/projects/";
        if (!uri.startsWith(prefix) || !uri.endsWith(tail)) {
            return "";
        }
        String middle = uri.substring(prefix.length(), uri.length() - tail.length());
        if (middle.endsWith("/")) {
            middle = middle.substring(0, middle.length() - 1);
        }
        return middle;
    }

    private boolean isAuthorized(final IHTTPSession session) {
        if (authToken.isEmpty()) {
            return true;
        }

        String authHeader = session.getHeaders().getOrDefault("authorization", "");
        if (authHeader.startsWith("Bearer ")) {
            String provided = authHeader.substring("Bearer ".length()).trim();
            if (authToken.equals(provided)) {
                return true;
            }
        }

        String queryToken = session.getParms().getOrDefault("token", "");
        return authToken.equals(queryToken);
    }

    private Response jsonResponse(final Response.Status status, final JSONObject payload) {
        return NanoHTTPD.newFixedLengthResponse(status, "application/json", payload.toString());
    }

    private void addCorsHeaders(final Response response, final IHTTPSession session) {
        if (response == null) {
            return;
        }
        String origin = session == null ? null : session.getHeaders().get("origin");

        if (origin != null && !origin.isBlank()) {
            response.addHeader("Access-Control-Allow-Origin", origin);
            response.addHeader("Vary", "Origin");
        } else {
            response.addHeader("Access-Control-Allow-Origin", "*");
        }
        response.addHeader("Access-Control-Allow-Headers", "Authorization,Content-Type,Accept,Origin,User-Agent,DNT,Cache-Control,X-Requested-With");
        response.addHeader("Access-Control-Allow-Methods", "GET,POST,PUT,DELETE,PATCH,OPTIONS");
        response.addHeader("Access-Control-Max-Age", "86400");
    }

    private String normalizeUri(final String uri) {
        if (uri == null || uri.isBlank()) {
            return "/";
        }
        if (uri.length() > 1 && uri.endsWith("/")) {
            return uri.substring(0, uri.length() - 1);
        }
        return uri;
    }

    private JSONObject error(final String code, final String message) {
        JSONObject payload = new JSONObject();
        payload.put("status", "error");
        payload.put("code", code);
        payload.put("message", message);
        return payload;
    }

    private final class WsSession extends WebSocket implements AndroidRuntimeWsSession {

        private WsSession(final IHTTPSession handshakeRequest) {
            super(handshakeRequest);
        }

        @Override
        protected void onOpen() {
            wsAdapter.onOpen(this);
            try {
                JSONObject hello = new JSONObject();
                hello.put("type", "event");
                hello.put("event", "system.hello");
                JSONObject payload = new JSONObject();
                payload.put("serverVersion", "android");
                payload.put("tokenRequired", !authToken.isEmpty());
                payload.put("wsProtocol", 1);
                hello.put("payload", payload);
                send(hello.toString());

                JSONObject runtimeState = new JSONObject();
                runtimeState.put("type", "event");
                runtimeState.put("event", "runtime.state");
                JSONObject runtimePayload = new JSONObject();
                runtimePayload.put("projectId", endpoint.projectId());
                runtimePayload.put("state", endpoint.runtimeState());
                runtimeState.put("payload", runtimePayload);
                send(runtimeState.toString());
            } catch (Exception ignored) {
                // Keep socket open even if initial event dispatch fails.
            }
        }

        @Override
        protected void onClose(final WebSocketFrame.CloseCode code,
                               final String reason,
                               final boolean initiatedByRemote) {
            wsAdapter.onClose(this);
        }

        @Override
        protected void onMessage(final WebSocketFrame message) {
            if (message != null) {
                wsAdapter.onMessage(this, message.getTextPayload());
            }
        }

        @Override
        protected void onPong(final WebSocketFrame pong) {
            // no-op
        }

        @Override
        protected void onException(final IOException exception) {
            wsAdapter.onClose(this);
        }

        @Override
        public void sendText(final String message) {
            try {
                send(message);
            } catch (Exception ignored) {
                wsAdapter.onClose(this);
            }
        }
    }
}
