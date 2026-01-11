package de.dfki.vsm.web;

import de.dfki.vsm.Preferences;
import de.dfki.vsm.model.project.ProjectConfig;
import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgeGraphics;
import de.dfki.vsm.model.sceneflow.chart.graphics.node.NodeGraphics;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.badge.CommentBadge;
import de.dfki.vsm.model.sceneflow.chart.graphics.comment.CommentGraphics;
import de.dfki.vsm.runtime.project.RunTimeProject;
import java.util.List;
import java.util.ArrayList;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import io.javalin.Javalin;
import io.javalin.core.util.Header;
import io.javalin.http.staticfiles.Location;
import io.javalin.http.Context;
import io.javalin.websocket.WsContext;
import org.json.JSONArray;
import org.json.JSONObject;

import java.net.URI;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.io.InputStream;

public final class WebUiServer {

    private static final LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();
    private static final String API_PREFIX = "/api/v1";
    private static final int RECENT_MAX = 8;
    private static WebUiServer sInstance;
    private static final String DEMO_PROJECT_ID = "demo-project";

    private Javalin mApp;
    private boolean mAllowExternal = false;
    private final Map<String, ProjectRef> projectStore = new HashMap<>();
    private final java.util.Set<WsContext> wsSessions = ConcurrentHashMap.newKeySet();

    private WebUiServer() {
    }

    public static synchronized WebUiServer getInstance() {
        if (sInstance == null) {
            sInstance = new WebUiServer();
        }
        return sInstance;
    }

    public void setAllowExternal(boolean allow) {
        mAllowExternal = allow;
    }

    public void start() {
        if (mApp != null) {
            return;
        }
        Preferences.load();
        mApp = Javalin.create(config -> {
            config.addStaticFiles("/web-ui", Location.CLASSPATH);
            // Serve packaged images (e.g., vsm_logo.svg)
            config.addStaticFiles("images", Location.CLASSPATH);
            // Configure SPA mode: serve index.html for all routes that don't match API endpoints or static files
            config.addSinglePageRoot("/", "/web-ui/index.html", Location.CLASSPATH);
        }).start(mAllowExternal ? "0.0.0.0" : "127.0.0.1", 8090);
        registerRoutes();
        sLogger.message("Web UI server started on " + getLocalUrl());
    }

    public void stop() {
        if (mApp != null) {
            mApp.stop();
            mApp = null;
        }
    }

    public String getLocalUrl() {
        return "http://127.0.0.1:8090";
    }

    private void registerRoutes() {
        mApp.get(API_PREFIX + "/info", this::handleInfo);
        mApp.get(API_PREFIX + "/token", this::handleToken);
        mApp.get(API_PREFIX + "/projects/recent", this::handleRecentProjects);
        mApp.get(API_PREFIX + "/projects/samples", ctx -> handleStaticProjectList(ctx, "res/prj"));
        mApp.get(API_PREFIX + "/projects/tutorials", ctx -> handleStaticProjectList(ctx, "res/tutorials"));
        mApp.get(API_PREFIX + "/projects", this::handleProjects);
        mApp.get(API_PREFIX + "/preferences", this::handlePreferences);
        mApp.get(API_PREFIX + "/devices", this::handleDevices);
        mApp.post(API_PREFIX + "/projects/recent/remove", this::handleRecentRemove);
        mApp.post(API_PREFIX + "/projects/recent/add", this::handleRecentAdd);
        // Basic hooks for opens/saves from other clients
        mApp.post(API_PREFIX + "/projects/opened", this::handleProjectOpened);
        mApp.post(API_PREFIX + "/projects/saved", this::handleProjectSaved);

        // Project lifecycle and data endpoints (minimal placeholders).
        mApp.post(API_PREFIX + "/projects/open", this::handleProjectOpen);
        mApp.post(API_PREFIX + "/projects", this::handleProjectCreate);
        mApp.post(API_PREFIX + "/projects/{pid}/save", this::handleProjectSave);
        mApp.post(API_PREFIX + "/projects/{pid}/save-as", this::handleProjectSaveAs);
        mApp.post(API_PREFIX + "/projects/{pid}/close", this::handleProjectClose);
        mApp.get(API_PREFIX + "/projects/{pid}/config", this::handleProjectConfig);
        mApp.get(API_PREFIX + "/projects/{pid}/project-config", this::handleProjectConfig);
        mApp.get(API_PREFIX + "/projects/{pid}/project-config/keys", this::handleProjectConfigKeys);
        mApp.get(API_PREFIX + "/projects/{pid}/script", this::handleScript);
        mApp.get(API_PREFIX + "/projects/{pid}/script/scenes", this::handleScriptScenes);
        mApp.get(API_PREFIX + "/projects/{pid}/script/elements", this::handleScriptElements);
        mApp.get(API_PREFIX + "/projects/{pid}/sceneflow", this::handleSceneflow);
        mApp.get(API_PREFIX + "/projects/{pid}/runtime", this::handleRuntime);
        mApp.post(API_PREFIX + "/projects/{pid}/sceneflow/navigate", this::handleSceneflowNavigate);
        mApp.post(API_PREFIX + "/projects/{pid}/script/diagnostics", this::handleScriptDiagnostics);

        // WebSocket endpoint: accepts requests and replies with JSON. Broadcasts snapshots/runtime state after mutations.
        mApp.ws("/ws", ws -> {
            ws.onConnect(ctx -> wsSessions.add(ctx));
            ws.onClose(ctx -> wsSessions.remove(ctx));
            ws.onError(ctx -> wsSessions.remove(ctx));
            ws.onMessage(ctx -> handleWsMessage(ctx.message(), ctx::send, msg -> broadcast(ctx, msg)));
        });

        // Serve packaged images (e.g., vsm_logo.svg) explicitly.
        mApp.get("/images/{file}", this::handleImage);
    }

    private void handleInfo(Context ctx) {
        JSONObject info = new JSONObject();
        info.put("name", "SceneMaker Web");
        info.put("port", 8090);
        info.put("tokenRequired", true);
        writeJson(ctx, info);
    }

    private void handleToken(Context ctx) {
        JSONObject token = new JSONObject();
        token.put("token", "dev-token");
        writeJson(ctx, token);
    }

    private void handleRecentProjects(Context ctx) {
        JSONArray recent = new JSONArray();
        for (int i = 0; i <= RECENT_MAX; i++) {
            String path = Preferences.getProperty("recentproject." + i + ".path");
            String name = Preferences.getProperty("recentproject." + i + ".name");
            String date = Preferences.getProperty("recentproject." + i + ".date");
            if (path == null || path.isBlank() || name == null || name.isBlank()) {
                continue;
            }
            JSONObject entry = new JSONObject();
            entry.put("path", path);
            entry.put("name", name);
            if (date != null && !date.isBlank()) {
                entry.put("date", date);
            }
            recent.put(entry);
        }
        JSONObject response = new JSONObject();
        response.put("projects", recent);
        writeJson(ctx, response);
    }

    private void handleRecentRemove(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String path = body.optString("path", "").trim();
        if (!path.isEmpty()) {
            removeRecent(path);
        }
        handleRecentProjects(ctx);
    }

    private void handleRecentAdd(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String path = body.optString("path", "").trim();
        String name = body.optString("name", "").trim();
        if (!path.isEmpty()) {
            addRecent(path, name);
        }
        handleRecentProjects(ctx);
    }

    private void handleProjectOpened(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String path = body.optString("path", "").trim();
        String name = body.optString("name", "").trim();
        if (!path.isEmpty()) {
            addRecent(path, name);
        }
        handleRecentProjects(ctx);
    }

    private void handleProjectSaved(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String path = body.optString("path", "").trim();
        String name = body.optString("name", "").trim();
        if (!path.isEmpty()) {
            addRecent(path, name);
        }
        handleRecentProjects(ctx);
    }

    public static void addRecent(String path, String name) {
        if (path == null || name == null || path.isBlank() || name.isBlank()) {
            if (path == null || path.isBlank()) {
                return;
            }
            name = fileName(path);
        }
        // Shift existing entries down and put new at position 0
        int max = RECENT_MAX;
        // If already present, remove it first
        for (int i = 0; i <= max; i++) {
            String existing = Preferences.getProperty("recentproject." + i + ".path");
            if (path.equals(existing)) {
                for (int j = i; j < max; j++) {
                    String nextPath = Preferences.getProperty("recentproject." + (j + 1) + ".path");
                    String nextName = Preferences.getProperty("recentproject." + (j + 1) + ".name");
                    String nextDate = Preferences.getProperty("recentproject." + (j + 1) + ".date");
                    if (nextPath == null) {
                        Preferences.removeProperty("recentproject." + j + ".path");
                        Preferences.removeProperty("recentproject." + j + ".name");
                        Preferences.removeProperty("recentproject." + j + ".date");
                    } else {
                        Preferences.setProperty("recentproject." + j + ".path", nextPath);
                        Preferences.setProperty("recentproject." + j + ".name", nextName);
                        if (nextDate != null) {
                            Preferences.setProperty("recentproject." + j + ".date", nextDate);
                        } else {
                            Preferences.removeProperty("recentproject." + j + ".date");
                        }
                    }
                }
                break;
            }
        }
        // Shift down
        for (int i = max; i > 0; i--) {
            String prevPath = Preferences.getProperty("recentproject." + (i - 1) + ".path");
            String prevName = Preferences.getProperty("recentproject." + (i - 1) + ".name");
            String prevDate = Preferences.getProperty("recentproject." + (i - 1) + ".date");
            if (prevPath != null) {
                Preferences.setProperty("recentproject." + i + ".path", prevPath);
                Preferences.setProperty("recentproject." + i + ".name", prevName);
                if (prevDate != null) {
                    Preferences.setProperty("recentproject." + i + ".date", prevDate);
                }
            }
        }
        Preferences.setProperty("recentproject.0.path", path);
        Preferences.setProperty("recentproject.0.name", name);
        Preferences.setProperty("recentproject.0.date", new java.text.SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss").format(new java.util.Date()));
        Preferences.save();
    }

    public static void removeRecent(String path) {
        if (path == null) return;
        int max = RECENT_MAX;
        for (int i = 0; i <= max; i++) {
            String existing = Preferences.getProperty("recentproject." + i + ".path");
            if (path.equals(existing)) {
                for (int j = i; j < max; j++) {
                    String nextPath = Preferences.getProperty("recentproject." + (j + 1) + ".path");
                    String nextName = Preferences.getProperty("recentproject." + (j + 1) + ".name");
                    String nextDate = Preferences.getProperty("recentproject." + (j + 1) + ".date");
                    if (nextPath == null) {
                        Preferences.removeProperty("recentproject." + j + ".path");
                        Preferences.removeProperty("recentproject." + j + ".name");
                        Preferences.removeProperty("recentproject." + j + ".date");
                    } else {
                        Preferences.setProperty("recentproject." + j + ".path", nextPath);
                        Preferences.setProperty("recentproject." + j + ".name", nextName);
                        if (nextDate != null) {
                            Preferences.setProperty("recentproject." + j + ".date", nextDate);
                        } else {
                            Preferences.removeProperty("recentproject." + j + ".date");
                        }
                    }
                }
                Preferences.save();
                break;
            }
        }
    }

    private static String fileName(String path) {
        int idx = Math.max(path.lastIndexOf('/'), path.lastIndexOf('\\'));
        if (idx >= 0 && idx < path.length() - 1) {
            return path.substring(idx + 1);
        }
        return path;
    }

    private void handleStaticProjectList(Context ctx, String directory) {
        JSONArray list = new JSONArray();
        Path base = resolveResourcePath(directory);
        if (base != null && Files.exists(base) && Files.isDirectory(base)) {
            try (Stream<Path> children = Files.list(base)) {
                children
                        .filter(Files::isDirectory)
                        .sorted(Comparator.comparing(path -> path.getFileName().toString().toLowerCase()))
                        .forEach(path -> {
                            JSONObject entry = new JSONObject();
                            entry.put("name", path.getFileName().toString());
                            entry.put("path", path.toAbsolutePath().toString());
                            list.put(entry);
                        });
            } catch (Exception exc) {
                sLogger.warning("Warning: Cannot list static projects in '" + directory + "': " + exc.getMessage());
            }
        }
        JSONObject response = new JSONObject();
        response.put("projects", list);
        writeJson(ctx, response);
    }

    private void handleProjects(Context ctx) {
        JSONObject response = new JSONObject();
        JSONArray list = new JSONArray();
        for (ProjectRef ref : projectStore.values()) {
            JSONObject entry = new JSONObject();
            entry.put("projectId", ref.id);
            entry.put("name", ref.name);
            entry.put("path", ref.path == null ? "" : ref.path);
            entry.put("dirty", ref.dirty);
            entry.put("pending", false);
            list.put(entry);
        }
        response.put("projects", list);
        writeJson(ctx, response);
    }

    private void handlePreferences(Context ctx) {
        // Minimal placeholder; real preference sync is handled client-side for now.
        JSONObject response = new JSONObject();
        response.put("preferences", new JSONObject());
        writeJson(ctx, response);
    }

    private void handleDevices(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("devices", new JSONArray());
        writeJson(ctx, response);
    }

    private void handleProjectOpen(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String path = body.optString("path", "");
        JSONObject response = new JSONObject();
        if (path.isBlank()) {
            ctx.status(400).result("Missing path");
            return;
        }
        String projectId = ensureProject(path, fileName(path));
        response.put("projectId", projectId);
        response.put("path", path);
        response.put("name", fileName(path));
        writeJson(ctx, response);
    }

    private void handleProjectCreate(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String name = body.optString("name", "Untitled");
        String baseDir = body.optString("baseDir", "");
        String projectId = ensureProject(baseDir, name);
        JSONObject response = new JSONObject();
        response.put("projectId", projectId);
        response.put("name", name);
        response.put("path", baseDir);
        writeJson(ctx, response);
    }

    private void handleProjectSave(Context ctx) {
        String pid = ctx.pathParam("pid");
        markClean(pid);
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        writeJson(ctx, response);
    }

    private void handleProjectSaveAs(Context ctx) {
        JSONObject body = new JSONObject(ctx.body());
        String path = body.optString("path", "");
        String pid = ctx.pathParam("pid");
        if (!pid.isEmpty()) {
            ProjectRef ref = projectStore.get(pid);
            if (ref != null) {
                ref.path = path;
                ref.name = fileName(path);
                ref.dirty = false;
            }
        }
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("path", path);
        writeJson(ctx, response);
    }

    private void handleProjectClose(Context ctx) {
        String pid = ctx.pathParam("pid");
        if (!pid.isEmpty()) {
            projectStore.remove(pid);
        }
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        writeJson(ctx, response);
    }

    private void handleProjectConfig(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        JSONObject response = new JSONObject();
        if (ref != null && ref.runtimeProject != null && ref.runtimeProject.getProjectConfig() != null) {
            ProjectConfig cfg = ref.runtimeProject.getProjectConfig();
            JSONObject cfgJson = new JSONObject();
            cfgJson.put("name", cfg.getProjectName());
            cfgJson.put("path", ref.path == null ? "" : ref.path);
            response.put("config", cfgJson);
        } else {
            response.put("config", new JSONObject());
        }
        writeJson(ctx, response);
    }

    private void handleProjectConfigKeys(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("keys", new JSONArray());
        writeJson(ctx, response);
    }

    private void handleScript(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        JSONObject response = new JSONObject();
        if (ref != null && ref.runtimeProject != null) {
            try {
                String script = loadFile(ref.runtimeProject.getProjectPath(), "scenescript.xml");
                response.put("script", script == null ? "" : script);
            } catch (Exception exc) {
                sLogger.warning("Warning: cannot load script for pid=" + pid + ": " + exc.getMessage());
                response.put("script", "");
            }
        } else {
            response.put("script", "");
        }
        writeJson(ctx, response);
    }

    private void handleScriptScenes(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("scenes", new JSONArray());
        writeJson(ctx, response);
    }

    private void handleScriptElements(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("elements", new JSONArray());
        writeJson(ctx, response);
    }

    private void handleSceneflow(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        JSONObject response = new JSONObject();
        if (ref != null && ref.runtimeProject != null) {
            try {
                String sceneflowXml = loadFile(ref.runtimeProject.getProjectPath(), "sceneflow.xml");
                ref.nodes = serializeNodes(ref.runtimeProject);
                ref.edges = serializeEdges(ref.runtimeProject);
                ref.comments = serializeComments(ref.runtimeProject);
                JSONObject meta = new JSONObject();
                meta.put("path", new JSONArray());
                meta.put("superNodeId", "");
                response.put("nodes", new JSONArray(ref.nodes));
                response.put("edges", new JSONArray(ref.edges));
                response.put("comments", new JSONArray(ref.comments));
                response.put("path", meta.optJSONArray("path"));
                response.put("superNodeId", meta.optString("superNodeId", ""));
                response.put("raw", sceneflowXml == null ? "" : sceneflowXml);
            } catch (Exception exc) {
                sLogger.warning("Warning: cannot load sceneflow for pid=" + pid + ": " + exc.getMessage());
                response.put("nodes", new JSONArray());
                response.put("edges", new JSONArray());
                response.put("comments", new JSONArray());
                response.put("raw", "");
            }
        } else {
            response.put("nodes", new JSONArray());
            response.put("edges", new JSONArray());
            response.put("comments", new JSONArray());
            response.put("raw", "");
        }
        writeJson(ctx, response);
    }

    private void handleRuntime(Context ctx) {
        String pid = ctx.pathParam("pid");
        ProjectRef ref = projectStore.get(pid);
        JSONObject response = new JSONObject();
        response.put("state", ref != null ? ref.runtimeState : "stopped");
        response.put("vars", serializeVars(ref));
        if (ref != null && ref.runtimeProject != null) {
            response.put("project", ref.runtimeProject.getProjectPath());
        }
        writeJson(ctx, response);
    }

    private void handleSceneflowNavigate(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        writeJson(ctx, response);
    }

    private void handleScriptDiagnostics(Context ctx) {
        JSONObject response = new JSONObject();
        response.put("issues", new JSONArray());
        writeJson(ctx, response);
    }

    private List<JSONObject> serializeNodes(RunTimeProject rtp) {
        if (rtp == null || rtp.getSceneFlow() == null) {
            return new ArrayList<>();
        }
        List<JSONObject> nodes = new ArrayList<>();
        collectNodes(rtp, rtp.getSceneFlow(), nodes, true, null);
        nodes.sort(Comparator.comparing(o -> o.optString("id", "")));
        return nodes;
    }

    private void collectNodes(RunTimeProject rtp, SuperNode superNode, List<JSONObject> out, boolean isRoot, String parentId) {
        if (superNode == null) {
            return;
        }
        // Add the supernode itself (including root sceneflow so UI can show it)
        JSONObject obj = new JSONObject();
        obj.put("id", superNode.getId());
        obj.put("name", superNode.getName());
        obj.put("type", "Super");
        obj.put("isSuper", true);
        obj.put("isRoot", isRoot);
        if (parentId != null && !parentId.isBlank()) {
            obj.put("parentId", parentId);
        }

        JSONObject pos = new JSONObject();
        pos.put("x", superNode.getGraphics() != null && superNode.getGraphics().getPosition() != null ? superNode.getGraphics().getPosition().getXPos() : 0);
        pos.put("y", superNode.getGraphics() != null && superNode.getGraphics().getPosition() != null ? superNode.getGraphics().getPosition().getYPos() : 0);
        obj.put("position", pos);

        JSONObject size = new JSONObject();
        size.put("w", 140);
        size.put("h", 140);
        int childCount = (superNode.getNodeList() != null ? superNode.getNodeList().size() : 0)
                + (superNode.getSuperNodeList() != null ? superNode.getSuperNodeList().size() : 0);
        obj.put("childCount", childCount);
        obj.put("size", size);

        obj.put("isStart", isStartNode(rtp, superNode));
        obj.put("isAltStart", isAltStartNode(rtp, superNode));
        obj.put("isHistory", superNode.isHistoryNode());
        obj.put("comment", superNode.getComment() == null ? "" : superNode.getComment());
        obj.put("commands", serializeCommands(superNode.getCmdList()));
        out.add(obj);

        // Basic children
        if (superNode.getNodeList() != null) {
            superNode.getNodeList().stream()
                    .sorted(Comparator.comparing(BasicNode::getId))
                    .forEach(n -> {
                        JSONObject child = new JSONObject();
                        child.put("id", n.getId());
                        child.put("name", n.getName());
                        child.put("type", "Basic");
                        child.put("isSuper", false);
                        child.put("parentId", superNode.getId());

                        JSONObject cpos = new JSONObject();
                        cpos.put("x", n.getGraphics() != null && n.getGraphics().getPosition() != null ? n.getGraphics().getPosition().getXPos() : 0);
                        cpos.put("y", n.getGraphics() != null && n.getGraphics().getPosition() != null ? n.getGraphics().getPosition().getYPos() : 0);
                        child.put("position", cpos);

                        JSONObject csize = new JSONObject();
                        csize.put("w", 120);
                        csize.put("h", 120);
                        child.put("size", csize);

                        child.put("isStart", isStartNode(rtp, n));
                        child.put("isAltStart", isAltStartNode(rtp, n));
                        child.put("isHistory", n.isHistoryNode());
                        child.put("comment", n.getComment() == null ? "" : n.getComment());
                        child.put("commands", serializeCommands(n.getCmdList()));
                        out.add(child);
                    });
        }
        // Super children
        if (superNode.getSuperNodeList() != null) {
            superNode.getSuperNodeList().stream()
                    .sorted(Comparator.comparing(BasicNode::getId))
                    .forEach(sn -> collectNodes(rtp, sn, out, false, superNode.getId()));
        }
    }

    private List<JSONObject> serializeEdges(RunTimeProject rtp) {
        if (rtp == null || rtp.getSceneFlow() == null) {
            return new ArrayList<>();
        }
        List<AbstractEdge> edges = new ArrayList<>();
        collectEdges(rtp.getSceneFlow(), edges);
        return edges.stream()
                .sorted(Comparator
                        .comparing(AbstractEdge::getSourceUnid)
                        .thenComparing(AbstractEdge::getTargetUnid)
                        .thenComparing(e -> e.getClass().getSimpleName()))
                .map(e -> {
                    JSONObject obj = new JSONObject();
                    String edgeId = e.getSourceUnid() + "_" + e.getTargetUnid() + "_" + e.getClass().getSimpleName();
                    obj.put("id", edgeId);
                    obj.put("sourceId", e.getSourceUnid());
                    obj.put("targetId", e.getTargetUnid());
                    obj.put("type", mapEdgeType(e));
                    obj.put("label", edgeLabel(e));
                    EdgeGraphics g = e.getGraphics();
                    if (g != null && g.getConnection() != null) {
                        JSONArray points = new JSONArray();
                        g.getConnection().getPointList().forEach(p -> {
                            JSONObject pt = new JSONObject();
                            pt.put("x", p.getCtrlXPos());
                            pt.put("y", p.getCtrlYPos());
                            points.put(pt);
                        });
                        obj.put("points", points);
                    } else {
                        obj.put("points", new JSONArray());
                    }
                    return obj;
                }).collect(Collectors.toList());
    }

    private void collectEdges(SuperNode superNode, List<AbstractEdge> out) {
        if (superNode == null) return;
        if (superNode.getNodeList() != null) {
            for (BasicNode n : superNode.getNodeList()) {
                if (n.getEdgeList() != null) {
                    out.addAll(n.getEdgeList());
                }
            }
        }
        if (superNode.getSuperNodeList() != null) {
            for (SuperNode sn : superNode.getSuperNodeList()) {
                collectEdges(sn, out);
            }
        }
    }

    private boolean isStartNode(RunTimeProject rtp, BasicNode node) {
        if (rtp == null || node == null || rtp.getSceneFlow() == null) return false;
        if (rtp.getSceneFlow().getStartNodeMap() != null) {
            return rtp.getSceneFlow().getStartNodeMap().containsKey(node.getId());
        }
        return false;
    }

    private boolean isAltStartNode(RunTimeProject rtp, BasicNode node) {
        if (rtp == null || node == null || rtp.getSceneFlow() == null) return false;
        // Alt-start nodes are stored on edges; approximate by checking alt-start maps on incoming edges
        return rtp.getSceneFlow().getEdgeList().stream().anyMatch(e -> {
            Map<?, ?> altMap = e.getCopyOfAltStartNodeMap();
            if (altMap == null) return false;
            return altMap.values().stream().anyMatch(val -> {
                if (val instanceof de.dfki.vsm.util.tpl.Tuple) {
                    Object second = ((de.dfki.vsm.util.tpl.Tuple<?, ?>) val).getSecond();
                    return second instanceof BasicNode && ((BasicNode) second).getId().equals(node.getId());
                }
                return false;
            });
        });
    }

    private List<JSONObject> serializeComments(RunTimeProject rtp) {
        if (rtp == null || rtp.getSceneFlow() == null) {
            return new ArrayList<>();
        }
        ArrayList<CommentBadge> comments = rtp.getSceneFlow().getCommentList();
        if (comments == null) {
            return new ArrayList<>();
        }
        return comments.stream().map(c -> {
            JSONObject obj = new JSONObject();
            CommentGraphics cgLocal = c.getGraphics();
            String stableId = "comment";
            if (cgLocal != null && cgLocal.getRectangle() != null) {
                stableId = stableId + "_" + cgLocal.getRectangle().getXPos() + "_" + cgLocal.getRectangle().getYPos() + "_" + cgLocal.getRectangle().getWidth() + "_" + cgLocal.getRectangle().getHeight();
            } else if (c.getHTMLText() != null) {
                stableId = stableId + "_" + c.getHTMLText().hashCode();
            } else {
                stableId = stableId + "_" + UUID.randomUUID();
            }
            obj.put("id", stableId);
            obj.put("text", c.getHTMLText() == null ? "" : c.getHTMLText());
            CommentGraphics cg = c.getGraphics();
            if (cg != null && cg.getRectangle() != null) {
                JSONObject rect = new JSONObject();
                rect.put("x", cg.getRectangle().getXPos());
                rect.put("y", cg.getRectangle().getYPos());
                rect.put("w", cg.getRectangle().getWidth());
                rect.put("h", cg.getRectangle().getHeight());
                obj.put("rect", rect);
            }
            return obj;
        }).collect(Collectors.toList());
    }

    // --- Mutations (minimal, in-memory only) -------------------------------
    private void addNode(JSONObject params) {
        String pid = params.optString("projectId", "");
        String id = params.optString("id", UUID.randomUUID().toString());
        String name = params.optString("name", id);
        JSONObject pos = params.optJSONObject("position");
        JSONObject size = params.optJSONObject("size");
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) return;
        JSONObject node = new JSONObject();
        node.put("id", id);
        node.put("name", name);
        node.put("type", params.optString("type", "BasicNode"));
        node.put("position", pos == null ? new JSONObject().put("x", 0).put("y", 0) : pos);
        node.put("size", size == null ? new JSONObject().put("w", 120).put("h", 60) : size);
        node.put("isStart", params.optBoolean("isStart", false));
        node.put("isAltStart", params.optBoolean("isAltStart", false));
        node.put("isHistory", params.optBoolean("isHistory", false));
        node.put("comment", params.optString("comment", ""));
        node.put("commands", params.optJSONArray("commands") == null ? new JSONArray() : params.optJSONArray("commands"));
        ref.nodes.add(node);
    }

    private void updateNode(JSONObject params) {
        String pid = params.optString("projectId", "");
        String id = params.optString("id", "");
        if (id.isEmpty()) return;
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) return;
        for (int i = 0; i < ref.nodes.size(); i++) {
            JSONObject n = ref.nodes.get(i);
            if (id.equals(n.optString("id"))) {
                mergeJson(n, params);
                break;
            }
        }
    }

    private void deleteNode(JSONObject params) {
        String pid = params.optString("projectId", "");
        String id = params.optString("id", "");
        if (id.isEmpty()) return;
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) return;
        ref.nodes.removeIf(n -> id.equals(n.optString("id")));
        // Also remove edges connected to this node
        ref.edges.removeIf(e -> id.equals(e.optString("source")) || id.equals(e.optString("target")));
    }

    private void addEdge(JSONObject params) {
        String pid = params.optString("projectId", "");
        String id = params.optString("id", UUID.randomUUID().toString());
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) return;
        JSONObject edge = new JSONObject();
        edge.put("id", id);
        edge.put("source", params.optString("source", ""));
        edge.put("target", params.optString("target", ""));
        edge.put("type", params.optString("type", "Edge"));
        edge.put("label", params.optString("label", ""));
        edge.put("points", params.optJSONArray("points") == null ? new JSONArray() : params.optJSONArray("points"));
        ref.edges.add(edge);
    }

    private void updateEdge(JSONObject params) {
        String pid = params.optString("projectId", "");
        String id = params.optString("id", "");
        if (id.isEmpty()) return;
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) return;
        for (int i = 0; i < ref.edges.size(); i++) {
            JSONObject e = ref.edges.get(i);
            if (id.equals(e.optString("id"))) {
                mergeJson(e, params);
                break;
            }
        }
    }

    private void deleteEdge(JSONObject params) {
        String pid = params.optString("projectId", "");
        String id = params.optString("id", "");
        if (id.isEmpty()) return;
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) return;
        ref.edges.removeIf(e -> id.equals(e.optString("id")));
    }

    private void addComment(JSONObject params) {
        String pid = params.optString("projectId", "");
        String id = params.optString("id", UUID.randomUUID().toString());
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) return;
        JSONObject c = new JSONObject();
        c.put("id", id);
        c.put("text", params.optString("text", ""));
        JSONObject rect = params.optJSONObject("rect");
        if (rect == null) {
            rect = new JSONObject().put("x", 0).put("y", 0).put("w", 200).put("h", 100);
        }
        c.put("rect", rect);
        ref.comments.add(c);
    }

    private void updateComment(JSONObject params) {
        String pid = params.optString("projectId", "");
        String id = params.optString("id", "");
        if (id.isEmpty()) return;
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) return;
        for (int i = 0; i < ref.comments.size(); i++) {
            JSONObject c = ref.comments.get(i);
            if (id.equals(c.optString("id"))) {
                mergeJson(c, params);
                break;
            }
        }
    }

    private void deleteComment(JSONObject params) {
        String pid = params.optString("projectId", "");
        String id = params.optString("id", "");
        if (id.isEmpty()) return;
        ProjectRef ref = projectStore.get(pid);
        if (ref == null) return;
        ref.comments.removeIf(c -> id.equals(c.optString("id")));
    }

    private void setRuntimeState(String pid, String state) {
        ProjectRef ref = projectStore.get(pid);
        if (ref != null) {
            ref.runtimeState = state;
        }
    }

    // Merge params into target JSONObject (shallow), overriding only provided keys.
    private void mergeJson(JSONObject target, JSONObject updates) {
        if (target == null || updates == null) return;
        for (String key : updates.keySet()) {
            if ("projectId".equals(key)) {
                continue;
            }
            Object val = updates.get(key);
            target.put(key, val);
        }
    }

    // --- WebSocket handling -------------------------------------------------
    private void handleWsMessage(String raw, java.util.function.Consumer<String> sender, java.util.function.Consumer<String> broadcaster) {
        try {
            JSONObject msg = new JSONObject(raw);
            String id = msg.optString("id", "");
            String method = msg.optString("method", "");
            JSONObject params = msg.optJSONObject("params");
            JSONObject result = dispatchWs(method, params == null ? new JSONObject() : params, broadcaster);
            JSONObject resp = new JSONObject();
            if (!id.isEmpty()) {
                resp.put("id", id);
            }
            resp.put("status", "ok");
            resp.put("result", result);
            sender.accept(resp.toString());
        } catch (Exception exc) {
            JSONObject resp = new JSONObject();
            resp.put("status", "error");
            resp.put("message", exc.getMessage());
            sender.accept(resp.toString());
        }
    }

    private JSONObject dispatchWs(String method, JSONObject params, java.util.function.Consumer<String> broadcaster) {
        switch (method) {
            case "SceneFlow.Get":
            case "SceneFlow.Snapshot":
                return snapshotPayload(params.optString("projectId", ""));
            case "SceneFlow.Node.Add":
                return mutateAndSnapshot(params.optString("projectId", ""), () -> addNode(params), broadcaster);
            case "SceneFlow.Node.Update":
                return mutateAndSnapshot(params.optString("projectId", ""), () -> updateNode(params), broadcaster);
            case "SceneFlow.Node.Delete":
                return mutateAndSnapshot(params.optString("projectId", ""), () -> deleteNode(params), broadcaster);
            case "SceneFlow.Edge.Add":
                return mutateAndSnapshot(params.optString("projectId", ""), () -> addEdge(params), broadcaster);
            case "SceneFlow.Edge.Update":
                return mutateAndSnapshot(params.optString("projectId", ""), () -> updateEdge(params), broadcaster);
            case "SceneFlow.Edge.Delete":
                return mutateAndSnapshot(params.optString("projectId", ""), () -> deleteEdge(params), broadcaster);
            case "SceneFlow.Comment.Add":
                return mutateAndSnapshot(params.optString("projectId", ""), () -> addComment(params), broadcaster);
            case "SceneFlow.Comment.Update":
                return mutateAndSnapshot(params.optString("projectId", ""), () -> updateComment(params), broadcaster);
            case "SceneFlow.Comment.Delete":
                return mutateAndSnapshot(params.optString("projectId", ""), () -> deleteComment(params), broadcaster);
            case "Project.Save":
            case "Project.SaveAs":
            case "Project.Close":
                JSONObject ok = new JSONObject();
                ok.put("status", "ok");
                return ok;
            case "Runtime.Start":
            case "Runtime.Pause":
            case "Runtime.Stop":
                String pid = params.optString("projectId", "");
                String state = method.endsWith("Start") ? "running" : method.endsWith("Pause") ? "paused" : "stopped";
                setRuntimeState(pid, state);
                JSONObject rt = new JSONObject();
                rt.put("state", state);
                if (broadcaster != null) {
                    JSONObject evt = new JSONObject();
                    evt.put("event", "runtime.state");
                    evt.put("state", state);
                    evt.put("projectId", pid);
                    broadcaster.accept(evt.toString());
                }
                return rt;
            default:
                JSONObject unknown = new JSONObject();
                unknown.put("message", "Unhandled method: " + method);
                return unknown;
        }
    }

    private JSONObject snapshotPayload(String projectId) {
        ProjectRef ref = projectStore.get(projectId);
        JSONObject snap = new JSONObject();
        if (ref != null) {
            snap.put("nodes", new JSONArray(ref.nodes));
            snap.put("edges", new JSONArray(ref.edges));
            snap.put("comments", new JSONArray(ref.comments));
        } else {
            snap.put("nodes", new JSONArray());
            snap.put("edges", new JSONArray());
            snap.put("comments", new JSONArray());
        }
        return snap;
    }

    private JSONObject mutateAndSnapshot(String projectId, Runnable mutator, java.util.function.Consumer<String> broadcaster) {
        if (mutator != null) {
            mutator.run();
        }
        ProjectRef ref = projectStore.get(projectId);
        if (ref != null) {
            ref.dirty = true;
        }
        JSONObject snapshot = snapshotPayload(projectId);
        JSONObject resp = new JSONObject();
        resp.put("status", "ok");
        resp.put("snapshot", snapshot);
        if (broadcaster != null) {
            JSONObject evt = new JSONObject();
            evt.put("event", "sceneflow.snapshot");
            evt.put("projectId", projectId);
            evt.put("snapshot", snapshot);
            broadcaster.accept(evt.toString());
        }
        return resp;
    }

    private JSONArray serializeVars(ProjectRef ref) {
        if (ref == null || ref.runtimeProject == null || ref.runtimeProject.getSceneFlow() == null) {
            return new JSONArray();
        }
        JSONArray arr = new JSONArray();
        try {
            List<?> vars = ref.runtimeProject.getVarDefInSceneFlow();
            if (vars != null) {
                for (Object v : vars) {
                    JSONObject obj = new JSONObject();
                    try {
                        de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition vd =
                                (de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition) v;
                        obj.put("name", vd.getName());
                        obj.put("type", vd.getType());
                    } catch (Exception ignored) {
                        obj.put("name", "");
                        obj.put("type", "");
                    }
                    obj.put("value", "");
                    arr.put(obj);
                }
            }
        } catch (Exception exc) {
            sLogger.warning("Warning: cannot serialize vars: " + exc.getMessage());
        }
        return arr;
    }

    private JSONArray serializeCommands(List<de.dfki.vsm.model.sceneflow.glue.command.Command> commands) {
        if (commands == null) {
            return new JSONArray();
        }
        return new JSONArray(commands.stream().map(cmd -> {
            JSONObject obj = new JSONObject();
            obj.put("cmd", cmd.toString());
            return obj;
        }).collect(Collectors.toList()));
    }

    private String edgeLabel(AbstractEdge edge) {
        if (edge instanceof de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge) {
            return ((de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge) edge).getCondition().toString();
        }
        if (edge instanceof de.dfki.vsm.model.sceneflow.chart.edge.RandomEdge) {
            return Double.toString(((de.dfki.vsm.model.sceneflow.chart.edge.RandomEdge) edge).getProbability());
        }
        if (edge instanceof de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge) {
            return Long.toString(((de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge) edge).getTimeout());
        }
        if (edge instanceof de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge) {
            return ((de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge) edge).getCondition().toString();
        }
        return "";
    }

    private String mapEdgeType(AbstractEdge edge) {
        if (edge instanceof de.dfki.vsm.model.sceneflow.chart.edge.EpsilonEdge) {
            return "EEDGE";
        }
        if (edge instanceof de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge) {
            return "CEDGE";
        }
        if (edge instanceof de.dfki.vsm.model.sceneflow.chart.edge.RandomEdge) {
            return "PEDGE";
        }
        if (edge instanceof de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge) {
            return "TEDGE";
        }
        if (edge instanceof de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge) {
            return "IEDGE";
        }
        if (edge instanceof de.dfki.vsm.model.sceneflow.chart.edge.ForkingEdge) {
            return "FEDGE";
        }
        return "EDGE";
    }

    private String loadFile(String baseDir, String filename) {
        try {
            Path p = Paths.get(baseDir, filename);
            if (Files.exists(p)) {
                return Files.readString(p);
            }
        } catch (Exception ignored) {
        }
        return "";
    }

    private String ensureProject(String path, String name) {
        // Reuse existing entry by path if present.
        for (ProjectRef ref : projectStore.values()) {
            if (ref.path != null && !ref.path.isBlank() && ref.path.equals(path)) {
                return ref.id;
            }
        }

        // Try to load a real project if a path was given
        RunTimeProject rtp = null;
        if (path != null && !path.isBlank()) {
            try {
                rtp = new RunTimeProject(new java.io.File(path));
                rtp.parse(path);
                List<JSONObject> nodes = serializeNodes(rtp);
                List<JSONObject> edges = serializeEdges(rtp);
                List<JSONObject> comments = serializeComments(rtp);
                String id = UUID.randomUUID().toString();
                ProjectRef ref = new ProjectRef(id, name, path);
                ref.runtimeProject = rtp;
                ref.nodes = nodes;
                ref.edges = edges;
                ref.comments = comments;
                ref.runtimeState = "stopped";
                projectStore.put(id, ref);
                return id;
            } catch (Exception exc) {
                sLogger.warning("Warning: failed to load project from " + path + ": " + exc.getMessage());
            }
        }

        String id = UUID.randomUUID().toString();
        ProjectRef ref = new ProjectRef(id, name, path);
        ref.runtimeProject = rtp;
        ref.runtimeState = "stopped";
        ref.nodes = new ArrayList<>();
        ref.edges = new ArrayList<>();
        ref.comments = new ArrayList<>();
        projectStore.put(id, ref);
        return id;
    }

    private void markClean(String pid) {
        if (pid == null) return;
        ProjectRef ref = projectStore.get(pid);
        if (ref != null) {
            ref.dirty = false;
        }
    }

    private static class ProjectRef {
        final String id;
        String name;
        String path;
        boolean dirty;
        RunTimeProject runtimeProject;
        List<JSONObject> nodes = new ArrayList<>();
        List<JSONObject> edges = new ArrayList<>();
        List<JSONObject> comments = new ArrayList<>();
        String runtimeState = "stopped";

        ProjectRef(String id, String name, String path) {
            this.id = id;
            this.name = name == null ? "" : name;
            this.path = path == null ? "" : path;
            this.dirty = false;
        }
    }

    private void handleImage(Context ctx) {
        String file = ctx.pathParam("file");
        if (file == null || file.isEmpty()) {
            ctx.status(404);
            return;
        }
        String resource = "images/" + file;
        try (InputStream is = getClass().getClassLoader().getResourceAsStream(resource)) {
            if (is == null) {
                ctx.status(404);
                return;
            }
            byte[] bytes = is.readAllBytes();
            ctx.header(Header.CACHE_CONTROL, "public, max-age=3600");
            ctx.contentType(detectMime(file));
            ctx.result(bytes);
        } catch (Exception exc) {
            ctx.status(500).result("Error loading image");
            sLogger.warning("Warning: Cannot serve image '" + resource + "': " + exc.getMessage());
        }
    }

    private String detectMime(String file) {
        String lower = file.toLowerCase();
        if (lower.endsWith(".svg")) return "image/svg+xml";
        if (lower.endsWith(".png")) return "image/png";
        if (lower.endsWith(".jpg") || lower.endsWith(".jpeg")) return "image/jpeg";
        if (lower.endsWith(".gif")) return "image/gif";
        return "application/octet-stream";
    }

    private void broadcast(WsContext origin, String message) {
        for (WsContext session : wsSessions) {
            try {
                if (session.session.isOpen() && (origin == null || session != origin)) {
                    session.send(message);
                }
            } catch (Exception exc) {
                sLogger.warning("Warning: failed to broadcast WS message: " + exc.getMessage());
            }
        }
    }

    private Path resolveResourcePath(String directory) {
        // First try the working directory (useful during development).
        Path fsPath = Paths.get(directory);
        if (Files.exists(fsPath)) {
            return fsPath;
        }
        // Then try to resolve from the classpath (packaged in the jar).
        try {
            ClassLoader cl = getClass().getClassLoader();
            URL url = cl.getResource(directory.endsWith("/") ? directory : directory + "/");
            if (url != null) {
                URI uri = url.toURI();
                return Paths.get(uri);
            }
        } catch (Exception exc) {
            sLogger.warning("Warning: Cannot resolve resource path '" + directory + "': " + exc.getMessage());
        }
        return null;
    }

    private void writeJson(Context ctx, JSONObject obj) {
        ctx.contentType("application/json");
        ctx.result(obj.toString());
    }
}
