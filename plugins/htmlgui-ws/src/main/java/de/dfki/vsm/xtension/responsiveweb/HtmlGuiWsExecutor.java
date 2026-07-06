/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.responsiveweb;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import io.javalin.Javalin;
import io.javalin.http.staticfiles.Location;
import io.javalin.websocket.WsCloseContext;
import io.javalin.websocket.WsConnectContext;
import io.javalin.websocket.WsContext;
import io.javalin.websocket.WsMessageContext;
import org.eclipse.jetty.server.Connector;
import org.eclipse.jetty.server.HttpConnectionFactory;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.ServerConnector;
import org.eclipse.jetty.server.SslConnectionFactory;
import org.eclipse.jetty.util.ssl.SslContextFactory;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;

/**
 * @author Lenny Händler, Patrick Gebhard
 */
public class HtmlGuiWsExecutor extends ActivityExecutor {
    // The map of activity worker
    private final Map<String, ActivityWorker> mActivityWorkerMap = new HashMap<>();
    // Per-variable conversation logs for vsm-feed (appendMessage / clearFeed)
    private final Map<String, List<String>> mConvLogs = new HashMap<>();
    // The singleton logger instance
    protected final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final ArrayList<WsConnectContext> websockets = new ArrayList<>();
    private Javalin app;
    private String mPathToCertificate = "";
    private String mSceneflowInfoVar = "";
    private final static String svalueSeparatorChar = "#";
    private final static String sCmdSeperatorChar = "$";
    // Browser process reference for cleanup
    private Process mBrowserProcess = null;
    // Retry limit for variable updates that arrive before the interpreter is ready
    private static final int VAR_RETRY_ATTEMPTS = 20;
    private static final int VAR_RETRY_DELAY_MS = 250;

    public HtmlGuiWsExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    @Override
    public synchronized String marker(long id) {
        return "${'" + id + "'}$";
    }

    // get the value of a feature (added PG) - quick and dirty
    protected static String getActionFeatureValue(String name, List<ActionFeature> features) {
        return features.stream()
                .filter(af -> af.getKey().equalsIgnoreCase(name))
                .findFirst()
                .map(ActionFeature::getVal)
                .orElse("").replace("'", "");
    }

    @Override
    public void launch() {
        mLogger.message("Loading HTML GUI Executor (WebSocket) ...");
        final int ws_port = Integer.parseInt(Objects.requireNonNull(mConfig.getProperty("ws_port")));
        final int html_port = Integer.parseInt(Objects.requireNonNull(mConfig.getProperty("html_port")));
        // wss_port is only required when an SSL certificate is configured
        final String certProp = mConfig.getProperty("certificate");
        final int wss_port = (certProp != null && !certProp.isBlank() && mConfig.getProperty("wss_port") != null)
                ? Integer.parseInt(mConfig.getProperty("wss_port")) : 4040;
        String guiFilesProp   = mConfig.getProperty("guifiles",   "gui");
        String audioFilesProp = mConfig.getProperty("audiofiles", "audio");
        final String guiFiles   = (mProject.getProjectPath() + File.separator + guiFilesProp).replace("\\", "/");
        final String audioFiles = (mProject.getProjectPath() + File.separator + audioFilesProp).replace("\\", "/");
        final String sceneflowStateVar = mConfig.getProperty("sceneflowStateVar");
        mSceneflowInfoVar = mConfig.getProperty("sceneflowInfoVar");
        mPathToCertificate = mConfig.getProperty("certificate");

        final boolean guiFilesExist   = new File(guiFiles).isDirectory();
        final boolean audioFilesExist = new File(audioFiles).isDirectory();
        if (!guiFilesExist)   mLogger.message("No gui/ directory found — legacy HTML files will not be served.");
        if (!audioFilesExist) mLogger.message("No audio/ directory found — audio files will not be served.");

        if (mPathToCertificate != null && !mPathToCertificate.isBlank()) {
            app = Javalin.create(config -> {
                if (guiFilesExist)   config.staticFiles.add(guiFiles,   Location.EXTERNAL);
                if (audioFilesExist) config.staticFiles.add(audioFiles, Location.EXTERNAL);
                config.jetty.modifyWebSocketServletFactory(factory -> factory.setIdleTimeout(java.time.Duration.ofMinutes(10)));
                config.jetty.modifyServer(server -> {
                    ServerConnector sslConnector = new ServerConnector(server,
                            new SslConnectionFactory(getSslContextFactory(), "http/1.1"),
                            new HttpConnectionFactory());
                    sslConnector.setPort(wss_port);
                    ServerConnector connector = new ServerConnector(server);
                    connector.setPort(ws_port);
                    ServerConnector htmlConnector = new ServerConnector(server);
                    htmlConnector.setPort(html_port);
                    server.setConnectors(new Connector[]{sslConnector, connector, htmlConnector});
                });
            }).start();
        } else {
            app = Javalin.create(config -> {
                if (guiFilesExist)   config.staticFiles.add(guiFiles,   Location.EXTERNAL);
                if (audioFilesExist) config.staticFiles.add(audioFiles, Location.EXTERNAL);
                config.jetty.modifyWebSocketServletFactory(factory -> factory.setIdleTimeout(java.time.Duration.ofMinutes(10)));
                config.jetty.modifyServer(server -> {
                    ServerConnector connector = new ServerConnector(server);
                    connector.setPort(ws_port);
                    ServerConnector htmlConnector = new ServerConnector(server);
                    htmlConnector.setPort(html_port);
                    server.setConnectors(new Connector[]{connector, htmlConnector});
                });
            }).start();
        }

        app.get("/", ctx -> {
            ctx.redirect("/index.html");
        });

        app.ws("/ws", ws -> {
            ws.onConnect(ctx -> {
                this.addWs(ctx);
                mLogger.message("Connected to Browser");
                // Let sceneflow know that a client has connected.
                // applyVarUpdate handles the race condition where the interpreter
                // may not be ready yet when the browser connects immediately at launch.
                if (sceneflowStateVar != null && !sceneflowStateVar.isBlank()) {
                    applyVarUpdate(sceneflowStateVar, "true");
                }
            });
            ws.onMessage(this::handleMessage);
            ws.onClose(ctx -> {
                this.removeWs(ctx);

                mLogger.message("Closed");
                mLogger.message("Remove active (but not needed anymore) activity actions");
                synchronized (mActivityWorkerMap) {
                    mActivityWorkerMap.clear();
                    // wake me up ..
                    mActivityWorkerMap.notifyAll();
                }
            });
            ws.onError(ctx -> {
                Throwable t = ctx.error();
                mLogger.failure("WebSocket error: " + (t != null ? t.getMessage() : "unknown"));
            });
        });

        // --- Schema-driven screens endpoints ---

        // Serve media assets from project/screens-assets/
        final String assetsDir = mProject.getProjectPath() + File.separator + "screens-assets";
        app.get("/assets/{filename}", ctx -> {
            String filename = ctx.pathParam("filename");
            File dir  = new File(assetsDir);
            File file = new File(dir, filename);
            try {
                if (!file.getCanonicalPath().startsWith(dir.getCanonicalPath())) {
                    ctx.status(403).result("Forbidden"); return;
                }
            } catch (IOException e) {
                ctx.status(500); return;
            }
            if (file.exists() && file.isFile()) {
                ctx.result(new FileInputStream(file)).contentType(assetContentType(filename));
            } else {
                ctx.status(404).result("Asset not found: " + filename);
            }
        });

        // Serve screens.json from the project directory
        final String screensJsonPath = mProject.getProjectPath() + File.separator + "screens.json";
        app.get("/screens.json", ctx -> {
            File screensFile = new File(screensJsonPath);
            if (screensFile.exists()) {
                ctx.result(new FileInputStream(screensFile)).contentType("application/json");
            } else {
                ctx.status(404).result("{}");
            }
        });

        // Serve character-config.json — from project file if present, otherwise
        // synthesised from the charamel-ws plugin config (character_url + ws_port).
        final String characterConfigPath = mProject.getProjectPath() + File.separator + "character-config.json";
        app.get("/character-config.json", ctx -> {
            File f = new File(characterConfigPath);
            if (f.exists()) {
                ctx.result(new FileInputStream(f)).contentType("application/json");
                return;
            }
            String synthesised = buildCharacterConfigFromPlugins();
            if (synthesised != null) {
                ctx.contentType("application/json").result(synthesised);
            } else {
                ctx.status(404);
            }
        });

        // Character proxy: fetches the character HTML from an external URL server-side
        // and re-serves it from localhost.  When Vuppetmaster JS runs with origin
        // http://localhost it can connect to ws://localhost:3030 without Chrome's
        // Private Network Access (PNA) restrictions (private→private, no PNA).
        // Usage: /character-proxy?_src=<encodedBaseUrl>&<otherParams>
        // Other params (e.g. server=ws://…) are forwarded to the upstream page AND
        // preserved in the browser's location.search so Vuppetmaster can read them.
        app.get("/character-proxy", ctx -> {
            String srcUrl = ctx.queryParam("_src");
            if (srcUrl == null || srcUrl.isBlank()) {
                ctx.status(400).result("Missing _src parameter");
                return;
            }
            // Forward all params except _src to the upstream URL (e.g. server=...)
            StringBuilder upstreamQs = new StringBuilder();
            ctx.queryParamMap().forEach((k, vals) -> {
                if (!"_src".equals(k) && !vals.isEmpty()) {
                    if (upstreamQs.length() > 0) upstreamQs.append('&');
                    try {
                        upstreamQs.append(java.net.URLEncoder.encode(k, "UTF-8"))
                                  .append('=')
                                  .append(java.net.URLEncoder.encode(vals.get(0), "UTF-8"));
                    } catch (java.io.UnsupportedEncodingException ignored) {}
                }
            });
            String fetchUrl = srcUrl + (upstreamQs.length() > 0
                    ? (srcUrl.contains("?") ? "&" : "?") + upstreamQs : "");
            try {
                java.net.HttpURLConnection conn =
                    (java.net.HttpURLConnection) new java.net.URL(fetchUrl).openConnection();
                conn.setRequestProperty("User-Agent", "Mozilla/5.0 (VSM-character-proxy/1.0)");
                conn.setConnectTimeout(8000);
                conn.setReadTimeout(15000);
                if (conn.getResponseCode() != 200) {
                    ctx.status(502).result("character-proxy: upstream returned " + conn.getResponseCode());
                    return;
                }
                String ct = conn.getContentType();
                String charset = (ct != null && ct.contains("charset="))
                    ? ct.substring(ct.indexOf("charset=") + 8).trim() : "UTF-8";
                String html = new String(conn.getInputStream().readAllBytes(), charset);

                // Inject <base> as first child of <head> so relative URLs (scripts, CSS)
                // in the proxied HTML resolve back to the character's original origin.
                String basePath = srcUrl.contains("/")
                    ? srcUrl.substring(0, srcUrl.lastIndexOf('/') + 1) : srcUrl + "/";
                String baseTag = "<base href=\"" + basePath + "\">";
                if (html.toLowerCase().contains("<head>")) {
                    html = html.replaceFirst("(?i)<head>", "<head>" + baseTag);
                } else {
                    html = baseTag + html;
                }
                ctx.contentType("text/html; charset=utf-8").result(html);
            } catch (Exception e) {
                mLogger.failure("character-proxy fetch failed (" + fetchUrl + "): " + e.getMessage());
                ctx.status(502).result("character-proxy error: " + e.getMessage());
            }
        });

        // Serve infrastructure files from the plugin JAR classpath so they are
        // always up-to-date regardless of what the project's gui/ folder contains.
        app.get("/index.html", ctx -> {
            InputStream stream = getClass().getResourceAsStream("/renderer/index.html");
            if (stream != null) ctx.result(stream).contentType("text/html");
            else ctx.status(404);
        });

        app.get("/js/wsclient.js", ctx -> {
            InputStream stream = getClass().getResourceAsStream("/renderer/wsclient.js");
            if (stream != null) ctx.result(stream).contentType("application/javascript");
            else ctx.status(404);
        });

        app.get("/screens.html", ctx -> {
            InputStream stream = getClass().getResourceAsStream("/renderer/screens.html");
            if (stream != null) ctx.result(stream).contentType("text/html");
            else ctx.status(404);
        });

        app.get("/vsm-renderer.js", ctx -> {
            InputStream stream = getClass().getResourceAsStream("/renderer/vsm-renderer.js");
            if (stream != null) ctx.result(stream).contentType("application/javascript");
            else ctx.status(404);
        });

        // Auto-start browser if configured
        boolean autostartBrowser = "true".equalsIgnoreCase(mConfig.getProperty("autostart_browser"));
        if (autostartBrowser) {
            boolean fullscreen        = "true".equalsIgnoreCase(mConfig.getProperty("browser_fullscreen"));
            boolean disablePna        = "true".equalsIgnoreCase(mConfig.getProperty("browser_disable_pna"));
            boolean disableWebSec     = "true".equalsIgnoreCase(mConfig.getProperty("browser_disable_web_security"));
            String startPath = mConfig.getProperty("browser_start_path", "/");
            if (!startPath.startsWith("/")) startPath = "/" + startPath;
            String url = "http://127.0.0.1:" + html_port + startPath;

            List<String> extraFlags = new ArrayList<>();
            if (disablePna) {
                // Targeted: disable Chrome's Private Network Access enforcement.
                // Required when a character iframe from a public origin (e.g. vuppetmaster.de)
                // needs to connect to a local WebSocket server (e.g. Charamel WS on localhost).
                extraFlags.add("--disable-features=BlockInsecurePrivateNetworkRequests,PrivateNetworkAccessSendPreflights");
            }
            if (disableWebSec) {
                // Nuclear option: disables all web security (CORS, PNA, mixed-content).
                // Requires a separate user-data-dir. Use when browser_disable_pna is insufficient.
                extraFlags.add("--disable-web-security");
                extraFlags.add("--user-data-dir=" + System.getProperty("java.io.tmpdir")
                    + java.io.File.separator + "vsm-chrome");
            }
            launchBrowser(url, fullscreen, extraFlags);
        }
    }

    /**
     * Launches a browser with the specified URL.
     * Browser selection priority: per-plugin "browser" config → global ~/.vsm/global-config.json
     * "browser.app" → OS default. Chrome-specific flags (fullscreen, disable_pna,
     * disable_web_security) require Chrome and override the "default" setting.
     */
    private void launchBrowser(String url, boolean fullscreen, List<String> extraFlags) {
        boolean chromeSpecificFlagsRequested = fullscreen || !extraFlags.isEmpty();
        String browserPref = resolveBrowserPreference();
        boolean useOsDefault = !chromeSpecificFlagsRequested
                && (browserPref.isEmpty() || "default".equalsIgnoreCase(browserPref));

        if (useOsDefault) {
            try {
                if (java.awt.Desktop.isDesktopSupported()
                        && java.awt.Desktop.getDesktop().isSupported(java.awt.Desktop.Action.BROWSE)) {
                    java.awt.Desktop.getDesktop().browse(new java.net.URI(url));
                    mLogger.message("Opened default browser: " + url);
                    return;
                }
            } catch (Exception e) {
                mLogger.warning("Default browser open failed, falling back to Chrome: " + e.getMessage());
            }
        }

        // Custom executable path supplied
        List<String> command = new ArrayList<>();
        if (!browserPref.isEmpty()
                && !"default".equalsIgnoreCase(browserPref)
                && !"chrome".equalsIgnoreCase(browserPref)) {
            command.add(browserPref);
        } else {
            String chromePath = findChrome();
            if (chromePath == null) {
                mLogger.warning("Chrome not found. Cannot auto-start browser.");
                return;
            }
            command.add(chromePath);
        }

        if (fullscreen) command.add("--start-fullscreen");
        command.addAll(extraFlags);
        command.add("--new-window");
        command.add(url);

        try {
            mLogger.message("Launching browser: " + String.join(" ", command));
            ProcessBuilder pb = new ProcessBuilder(command);
            pb.inheritIO();
            mBrowserProcess = pb.start();
        } catch (IOException e) {
            mLogger.failure("Failed to launch browser: " + e.getMessage());
        }
    }

    /** Per-plugin config "browser" → global ~/.vsm/global-config.json "browser.app" → "". */
    private String resolveBrowserPreference() {
        String pluginPref = mConfig.getProperty("browser", "");
        if (pluginPref != null && !pluginPref.isBlank()) return pluginPref.trim();
        return readGlobalBrowserPref();
    }

    /** Reads "browser.app" from ~/.vsm.d/global-config.json without pulling in a JSON library. */
    private String readGlobalBrowserPref() {
        try {
            java.nio.file.Path cfg = java.nio.file.Paths.get(
                    System.getProperty("user.home"), ".vsm.d", "global-config.json");
            if (!java.nio.file.Files.exists(cfg)) return "";
            String content = java.nio.file.Files.readString(cfg);
            int browserIdx = content.indexOf("\"browser\"");
            if (browserIdx < 0) return "";
            int appIdx = content.indexOf("\"app\"", browserIdx);
            if (appIdx < 0) return "";
            int colon = content.indexOf(":", appIdx + 5);
            if (colon < 0) return "";
            int q1 = content.indexOf("\"", colon + 1);
            if (q1 < 0) return "";
            int q2 = content.indexOf("\"", q1 + 1);
            if (q2 < 0) return "";
            return content.substring(q1 + 1, q2);
        } catch (Exception e) {
            return "";
        }
    }

    /** Finds Chrome/Chromium executable on the current platform. */
    private String findChrome() {
        String os = System.getProperty("os.name", "").toLowerCase();
        if (os.contains("mac")) {
            String[] candidates = {
                "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
                "/Applications/Chromium.app/Contents/MacOS/Chromium",
                System.getProperty("user.home") + "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
            };
            for (String path : candidates) {
                if (new File(path).exists()) return path;
            }
        } else if (os.contains("win")) {
            String[] candidates = {
                System.getenv("ProgramFiles") + "\\Google\\Chrome\\Application\\chrome.exe",
                System.getenv("ProgramFiles(x86)") + "\\Google\\Chrome\\Application\\chrome.exe",
                System.getenv("LOCALAPPDATA") + "\\Google\\Chrome\\Application\\chrome.exe"
            };
            for (String path : candidates) {
                if (path != null && new File(path).exists()) return path;
            }
        } else if (os.contains("linux")) {
            for (String candidate : new String[]{"google-chrome", "google-chrome-stable", "chromium-browser", "chromium"}) {
                try {
                    Process p = Runtime.getRuntime().exec(new String[]{"which", candidate});
                    if (p.waitFor() == 0) return candidate;
                } catch (Exception ignored) {}
            }
        }
        return null;
    }

    /**
     * Synthesises a character-config.json payload from the charamel-ws plugin config.
     * Looks for a plugin whose class name contains "charamelWs", reads its
     * character_url (defaults to the public Vuppetmaster page) and ws_port,
     * and returns {"url": "<character_url>?server=ws://localhost:<ws_port>/ws"}.
     * Returns null if no charamel-ws plugin is configured.
     */
    private String buildCharacterConfigFromPlugins() {
        // NOTE: charamel-embed integrates via the schema-driven "character" key in screens.json
        // (character.srcVar → http://localhost:<port>/character.html?appName=...), NOT this parent
        // #character path — auto-synthesising it here too would load the character twice. This method
        // therefore only covers charamel-ws (external Charamel-hosted page). Projects that want the
        // parent path for charamel-embed can still ship a character-config.json manually.
        final String CHARAMEL_CLASS_FRAGMENT = "charamelWs";
        final String DEFAULT_CHARACTER_URL   = "https://vuppetmaster.de/dev/ubidenz/";
        for (PluginConfig pc : mProject.getProjectConfig().getPluginConfigList()) {
            if (!pc.getClassName().contains(CHARAMEL_CLASS_FRAGMENT)) continue;
            String baseUrl = pc.getProperty("character_url");
            if (baseUrl == null || baseUrl.isBlank()) baseUrl = DEFAULT_CHARACTER_URL;
            if (!baseUrl.endsWith("/")) baseUrl += "/";
            String wsPort = pc.getProperty("ws_port");
            if (wsPort == null || wsPort.isBlank()) wsPort = "3030";
            String fullUrl = baseUrl + "?server=ws://localhost:" + wsPort + "/ws";
            mLogger.message("character-config: synthesised from charamel-ws — " + fullUrl);
            return "{\"url\":\"" + fullUrl + "\"}";
        }
        return null;
    }

    /**
     * Applies a variable update from the browser. If the SceneFlow interpreter is not yet
     * ready (race condition: browser connected before launch() completed), spawns a daemon
     * thread to retry for up to VAR_RETRY_ATTEMPTS × VAR_RETRY_DELAY_MS milliseconds.
     */
    private void applyVarUpdate(String varName, String rawValue) {
        if (mProject.hasVariable(varName)) {
            if ("true".equalsIgnoreCase(rawValue) || "false".equalsIgnoreCase(rawValue)) {
                mProject.setVariable(varName, Boolean.parseBoolean(rawValue));
            } else {
                mProject.setVariable(varName, rawValue);
            }
        } else {
            final String fVar = varName, fVal = rawValue;
            Thread t = new Thread(() -> {
                for (int i = 0; i < VAR_RETRY_ATTEMPTS; i++) {
                    try { Thread.sleep(VAR_RETRY_DELAY_MS); } catch (InterruptedException e) { break; }
                    if (mProject.hasVariable(fVar)) {
                        if ("true".equalsIgnoreCase(fVal) || "false".equalsIgnoreCase(fVal)) {
                            mProject.setVariable(fVar, Boolean.parseBoolean(fVal));
                        } else {
                            mProject.setVariable(fVar, fVal);
                        }
                        return;
                    }
                }
                mLogger.warning("HtmlGuiWs: variable '" + fVar + "' not available after " + VAR_RETRY_ATTEMPTS + " retries");
            }, "vsm-varset-retry");
            t.setDaemon(true);
            t.start();
        }
    }

    private synchronized void handleMessage(WsMessageContext ctx) {
        String message = ctx.message();
        mLogger.message("Processing Browser GUI message: >" + message + "<");

        // varUpdate$<varName>$<value> — write directly to the named SceneFlow variable.
        // The value may contain '$', so split with limit 3.
        if (message.startsWith("varUpdate$")) {
            String[] parts = message.split("\\$", 3);
            if (parts.length == 3 && parts[1] != null && !parts[1].isBlank()) {
                // applyVarUpdate handles the race condition where the interpreter may not
                // be ready yet (e.g. browser connects immediately after plugin launch).
                applyVarUpdate(parts[1], parts[2]);
            }
            return; // do not also write to sceneflowInfoVar
        }

        // Let sceneflow know that a client has sent a message (legacy path).
        if (mProject.hasVariable(mSceneflowInfoVar)) {
            mProject.setVariable(mSceneflowInfoVar, message);
        }

        // PG - Comment: This should be handled by a Sceneflow model!
//        if (message.equals("stopwatch")) {
//            broadcast("./audio_gui.html"); //arbeitszeit
//        }
//        else if (message.equals("calendar")) {
//            broadcast("./ui_stimmungsbarometer.html"); //moodgraph
//        } else if (message.equals("phone")) {
//            broadcast("./slider_gui.html"); //slider
//        } else if (message.equals("chat")) {
//            broadcast("./emotion_gui.html"); //emotion
//        } else if (message.equals("persons")) {
//            broadcast("./days_gui.html"); // days
//        } else if (message.equals("day_Montag")) {
//            broadcast("./conv_gui.html"); //conv
//        } else if (message.equals("home")) { //default
//            broadcast("./index.html");
//        } else if (message.equals("person")) {
//            broadcast(("./username_gui.html"));
//        } else if (message.equals("diary")) {
//            broadcast(("./conv_gui.html"));
//        }
    }

    private synchronized void removeWs(WsCloseContext ctx) {
        websockets.remove(ctx);
    }

    private synchronized void addWs(WsConnectContext ws) {
        this.websockets.add(ws);
    }

    private synchronized void broadcast(String msg) {
        for (WsContext ws : websockets) {
            ws.send(msg);
        }
    }

    /** Appends one chat message to a named conversation log and broadcasts the update. */
    private void appendToConversationLog(String varName, String role, String speaker, String text) {
        appendToConversationLog(varName, role, speaker, text, null);
    }

    private void appendToConversationLog(String varName, String role, String speaker, String text, String timestamp) {
        StringBuilder msg = new StringBuilder("{");
        msg.append("\"role\":\"").append(escapeJson(role)).append("\"");
        msg.append(",\"text\":\"").append(escapeJson(text)).append("\"");
        if (speaker != null)
            msg.append(",\"speaker\":\"").append(escapeJson(speaker.replace("'", ""))).append("\"");
        if (timestamp != null)
            msg.append(",\"timestamp\":\"").append(escapeJson(timestamp.replace("'", ""))).append("\"");
        msg.append("}");
        synchronized (mConvLogs) {
            mConvLogs.computeIfAbsent(varName, k -> new ArrayList<>()).add(msg.toString());
            String jsonArray = buildJsonArray(mConvLogs.get(varName));
            if (mProject.hasVariable(varName)) mProject.setVariable(varName, jsonArray);
            broadcast("updateVar$" + varName + "$" + jsonArray);
        }
    }

    /** Builds a JSON array string from a list of already-serialised JSON objects. */
    private static String buildJsonArray(List<String> items) {
        if (items == null || items.isEmpty()) return "[]";
        StringBuilder sb = new StringBuilder("[");
        for (int i = 0; i < items.size(); i++) {
            if (i > 0) sb.append(",");
            sb.append(items.get(i));
        }
        sb.append("]");
        return sb.toString();
    }

    /** Escapes a string for safe embedding inside a JSON double-quoted value. */
    private static String escapeJson(String s) {
        if (s == null) return "";
        return s.replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t");
    }

    private static String assetContentType(String filename) {
        String f = filename.toLowerCase();
        if (f.endsWith(".jpg") || f.endsWith(".jpeg")) return "image/jpeg";
        if (f.endsWith(".png"))  return "image/png";
        if (f.endsWith(".gif"))  return "image/gif";
        if (f.endsWith(".webp")) return "image/webp";
        if (f.endsWith(".svg"))  return "image/svg+xml";
        if (f.endsWith(".mp4"))  return "video/mp4";
        if (f.endsWith(".webm")) return "video/webm";
        if (f.endsWith(".ogv"))  return "video/ogg";
        if (f.endsWith(".mp3"))  return "audio/mpeg";
        if (f.endsWith(".wav"))  return "audio/wav";
        if (f.endsWith(".oga") || f.endsWith(".ogg")) return "audio/ogg";
        return "application/octet-stream";
    }

    @Override
    public void unload() {
        websockets.clear();
        synchronized (mConvLogs) { mConvLogs.clear(); }
        app.stop();
        // Terminate browser process if we started one
        if (mBrowserProcess != null) {
            mLogger.message("Terminating browser process...");
            mBrowserProcess.destroy();
            try {
                // Give it a moment to terminate gracefully
                if (!mBrowserProcess.waitFor(2, java.util.concurrent.TimeUnit.SECONDS)) {
                    // Force kill if it doesn't terminate
                    mBrowserProcess.destroyForcibly();
                }
            } catch (InterruptedException e) {
                mBrowserProcess.destroyForcibly();
            }
            mBrowserProcess = null;
        }
    }

    private SslContextFactory.Server getSslContextFactory() {
        SslContextFactory.Server sslContextFactory = new SslContextFactory.Server();
        sslContextFactory.setKeyStorePath(this.getClass().getResource(mPathToCertificate).toExternalForm()); //default "/my-release-key.keystore"
        sslContextFactory.setKeyStorePassword("123456");
        return sslContextFactory;
    }

    @Override
    public void execute(AbstractActivity activity) {
        final String activity_actor = activity.getActor();

        if (activity instanceof SpeechActivity) {
            SpeechActivity sa = (SpeechActivity) activity;
            String text = sa.getTextOnly("${'").trim();
            LinkedList<String> timemarks = sa.getTimeMarks("${'");

            if (text.isEmpty()) {
                // No text — fire any registered marker activities immediately
                for (String tm : timemarks) {
                    mLogger.warning("Directly executing activity at timemark " + tm);
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                }
            } else {
                // If the agent has a `var` property, append speech to that conversation log
                de.dfki.vsm.model.project.AgentConfig agentCfg =
                        mProject.getAgentConfig(activity_actor);
                String varName = (agentCfg != null) ? agentCfg.getProperty("var") : null;
                if (varName != null && !varName.isBlank()) {
                    // @varName: resolve from a SceneFlow variable at speak time
                    if (text.startsWith("@")) {
                        de.dfki.vsm.runtime.interpreter.value.AbstractValue val =
                                mProject.getValueOf(text.substring(1));
                        text = (val != null) ? val.toString() : "";
                    }
                    String role    = agentCfg.getProperty("role",    "agent");
                    String speaker = agentCfg.getProperty("speaker");
                    appendToConversationLog(varName, role, speaker, text);
                }
                // Always fire any inline markers so co-located ActionObjects execute
                for (String tm : timemarks) {
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                }
            }
        } else {
            final String name = activity.getName();
            //final LinkedList<ActionFeature> features = activity.getFeatures();

            if (name.equalsIgnoreCase("set")) {
                String element = activity.get("element");
                String value = activity.get("value");
                if (value != null) {
                    value = value.replace("'", "");
                } else {
                    value = "";
                }
                broadcast(element + sCmdSeperatorChar + value);
            } else if (name.equalsIgnoreCase("setMoodGraph") ||
                    name.equalsIgnoreCase("setWorkHrsGraph")) {
                String element = activity.get("element");
                String day = activity.get("day");
                String type = activity.get("type");
                String value = activity.get("value");
                if (value != null) {
                    value = value.replace("'", "");
                } else {
                    value = "";
                }
                broadcast(element + sCmdSeperatorChar + name + svalueSeparatorChar + day + svalueSeparatorChar +
                        type + svalueSeparatorChar + value);
            } else if (name.equalsIgnoreCase("setSpeechBubble")) {
                String element = activity.get("element");
                String producer = activity.get("producer");
                String value = activity.get("value");
                if (value != null) {
                    value = value.replace("'", "");
                } else {
                    value = "";
                }
                broadcast(element + sCmdSeperatorChar + name + svalueSeparatorChar + producer + svalueSeparatorChar + value);
            } else if (name.equalsIgnoreCase("setMenuItem")) {
                //Dummy variable to match format of other cmds
                String element = "dummy_el";
                String id = activity.get("id");
                String value = activity.get("value").replace("'", "");
                String type = activity.get("type");
                if (type != null) {
                    type = type.replace("'", "");
                    broadcast(element + sCmdSeperatorChar + name + svalueSeparatorChar + id + svalueSeparatorChar + value + svalueSeparatorChar + type);
                } else {
                    broadcast(element + sCmdSeperatorChar + name + svalueSeparatorChar + id + svalueSeparatorChar + value);
                }
            } else if (name.equalsIgnoreCase("showElement")) {
                //Dummy variable to match format of other cmds
                String element = "dummy_el";
                String id = activity.get("id");
                broadcast(element + sCmdSeperatorChar + name + svalueSeparatorChar + id);
            } else if (name.equalsIgnoreCase("hideElement")) {
                //Dummy variable to match format of other cmds
                String element = "dummy_el";
                String id = activity.get("id");
                broadcast(element + sCmdSeperatorChar + name + svalueSeparatorChar + id);
            } else if (name.equalsIgnoreCase("setcss")) {
                //Dummy variable to match format of other cmds
                String element = "dummy_el";
                String property = getActionFeatureValue("var", activity.getFeatures()); //activity.get("var");
                String value = getActionFeatureValue("value", activity.getFeatures());
                broadcast(element + sCmdSeperatorChar + name + svalueSeparatorChar + property + svalueSeparatorChar + value);
            } else if (name.equalsIgnoreCase("muteMic")) {
                //Dummy variable to match format of other cmds
                String element = "dummy_el";
                broadcast(element + sCmdSeperatorChar + name);
            } else if (name.equalsIgnoreCase("openMic")) {
                //Dummy variable to match format of other cmds
                String element = "dummy_el";
                broadcast(element + sCmdSeperatorChar + name);
            } else if (name.equalsIgnoreCase("setAudioItem")) {
                String element = activity.get("element");
                String audio_src = "./" + activity.get("audio").replace("'", "");
                broadcast(element + sCmdSeperatorChar + name + svalueSeparatorChar + audio_src);
            } else if (name.equalsIgnoreCase("controlAudio")) {
                String element = activity.get("element");
                String control_type = activity.get("type");
                broadcast(element + sCmdSeperatorChar + name + svalueSeparatorChar + control_type);
            } else if (name.equalsIgnoreCase("stop")) {
                app.stop();
            } else if (name.equalsIgnoreCase("loadScreen")) {
                String screen = activity.get("screen").replace("'", "");
                broadcast("loadScreen$" + screen);
            } else if (name.equalsIgnoreCase("updateVar")) {
                String varName = activity.get("var").replace("'", "");
                String value   = activity.get("value").replace("'", "");
                broadcast("updateVar$" + varName + "$" + value);
            } else if (name.equalsIgnoreCase("appendMessage")) {
                // appendMessage(var='…', role='agent|user|system', text='…'[, speaker='…'][, timestamp='…'])
                String varName   = activity.get("var")  != null ? activity.get("var").replace("'", "")  : "";
                String role      = activity.get("role") != null ? activity.get("role").replace("'", "") : "agent";
                String text      = activity.get("text") != null ? activity.get("text").replace("'", "") : "";
                // @varName: read text from a SceneFlow variable at call time
                if (text.startsWith("@")) {
                    de.dfki.vsm.runtime.interpreter.value.AbstractValue val = mProject.getValueOf(text.substring(1));
                    text = (val != null) ? val.toString() : "";
                }
                String speaker   = activity.get("speaker");
                String timestamp = activity.get("timestamp");
                if (!varName.isEmpty()) {
                    appendToConversationLog(varName, role, speaker, text, timestamp);
                }

            } else if (name.equalsIgnoreCase("clearFeed")) {
                // clearFeed(var='…')  — empties the conversation log for a feed variable
                String varName = activity.get("var") != null ? activity.get("var").replace("'", "") : "";
                if (!varName.isEmpty()) {
                    synchronized (mConvLogs) {
                        mConvLogs.put(varName, new ArrayList<>());
                        if (mProject.hasVariable(varName)) mProject.setVariable(varName, "[]");
                        broadcast("updateVar$" + varName + "$[]");
                    }
                }

            } else if (name.equalsIgnoreCase("screensToFront")) {
                broadcast("screensToFront");
            } else if (name.equalsIgnoreCase("guiToFront")) {
                broadcast(name);
            } else if (name.equalsIgnoreCase("vcToFront")) {
                broadcast(name);
            } else if (!name.isEmpty()) { //check if name represents a webpage - must be configured in the device's agent as key, value pair.
                String guipage = mProject.getAgentConfig(activity_actor).getProperty(name);
                // send only if there is a stored html page
                if (guipage != null)
                    if (guipage.contains(".html")) {
                        broadcast(guipage);
                    }
            } else {
                mLogger.failure("Either agent " + activity_actor + " or content " + name + " is not valid.");
            }
        }
    }
}
