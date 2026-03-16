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
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.ServerConnector;
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
        final int wss_port = Integer.parseInt(Objects.requireNonNull(mConfig.getProperty("wss_port")));
        final int ws_port = Integer.parseInt(Objects.requireNonNull(mConfig.getProperty("ws_port")));
        final int html_port = Integer.parseInt(Objects.requireNonNull(mConfig.getProperty("html_port")));
        final String guiFiles = (mProject.getProjectPath() + File.separator + mConfig.getProperty("guifiles")).replace("\\", "/");
        final String audioFiles = (mProject.getProjectPath() + File.separator + mConfig.getProperty("audiofiles")).replace("\\", "/");
        final String sceneflowStateVar = mConfig.getProperty("sceneflowStateVar");
        mSceneflowInfoVar = mConfig.getProperty("sceneflowInfoVar");
        mPathToCertificate = mConfig.getProperty("certificate");

        if (mPathToCertificate != null) {
            app = Javalin.create(config -> {
                config.addStaticFiles(guiFiles, Location.EXTERNAL);
                config.addStaticFiles(audioFiles, Location.EXTERNAL);
                config.server(() -> {
                    Server server = new Server();
                    ServerConnector sslConnector = new ServerConnector(server, getSslContextFactory());
                    sslConnector.setPort(wss_port);
                    ServerConnector connector = new ServerConnector(server);
                    connector.setPort(ws_port);
                    ServerConnector htmlConnector = new ServerConnector(server);
                    htmlConnector.setPort(html_port);
                    server.setConnectors(new Connector[]{sslConnector, connector, htmlConnector});
                    return server;
                });
            }).start();
        } else {
            app = Javalin.create(config -> {
                config.addStaticFiles(guiFiles, Location.EXTERNAL);
                config.addStaticFiles(audioFiles, Location.EXTERNAL);
                config.server(() -> {
                    Server server = new Server();
                    ServerConnector connector = new ServerConnector(server);
                    connector.setPort(ws_port);
                    ServerConnector htmlConnector = new ServerConnector(server);
                    htmlConnector.setPort(html_port);
                    server.setConnectors(new Connector[]{connector, htmlConnector});
                    return server;
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
                // test
                //broadcast("./default_gui.html");
                // let sceneflow know that a client has connected
                if (mProject.hasVariable(sceneflowStateVar)) {
                    mProject.setVariable(sceneflowStateVar, true);
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
            ws.onError(ctx -> mLogger.failure("Error handling ws message exchange"));
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
            boolean fullscreen = "true".equalsIgnoreCase(mConfig.getProperty("browser_fullscreen"));
            boolean ignoreCertErrors = !"false".equalsIgnoreCase(mConfig.getProperty("browser_ignore_cert_errors"));
            String url = "http://127.0.0.1:" + html_port;
            launchBrowser(url, fullscreen, ignoreCertErrors);
        }
    }

    /**
     * Launches Chrome browser with the specified URL.
     * Supports macOS, Windows, and Linux.
     * The browser process is stored so it can be terminated when the plugin unloads.
     */
    private void launchBrowser(String url, boolean fullscreen, boolean ignoreCertErrors) {
        String os = System.getProperty("os.name", "").toLowerCase();
        List<String> command = new ArrayList<>();

        if (os.contains("mac")) {
            // macOS: launch Chrome directly to get a killable process
            String chromePath = findMacChrome();
            if (chromePath == null) {
                mLogger.warning("Chrome not found on macOS. Cannot auto-start browser.");
                return;
            }
            command.add(chromePath);
            if (ignoreCertErrors) {
                command.add("--ignore-certificate-errors");
            }
            if (fullscreen) {
                command.add("--start-fullscreen");
            }
            command.add("--new-window");
            command.add(url);
        } else if (os.contains("win")) {
            // Windows: launch Chrome directly to get a killable process
            String chromePath = findWindowsChrome();
            if (chromePath == null) {
                mLogger.warning("Chrome not found on Windows. Cannot auto-start browser.");
                return;
            }
            command.add(chromePath);
            if (ignoreCertErrors) {
                command.add("--ignore-certificate-errors");
            }
            if (fullscreen) {
                command.add("--start-fullscreen");
            }
            command.add("--new-window");
            command.add(url);
        } else if (os.contains("linux")) {
            // Linux: try google-chrome or chromium-browser
            String browser = findLinuxChrome();
            if (browser == null) {
                mLogger.warning("Chrome/Chromium not found on Linux. Cannot auto-start browser.");
                return;
            }
            command.add(browser);
            if (ignoreCertErrors) {
                command.add("--ignore-certificate-errors");
            }
            if (fullscreen) {
                command.add("--start-fullscreen");
            }
            command.add("--new-window");
            command.add(url);
        } else {
            mLogger.warning("Unsupported OS for browser auto-start: " + os);
            return;
        }

        try {
            mLogger.message("Launching browser: " + String.join(" ", command));
            ProcessBuilder pb = new ProcessBuilder(command);
            pb.inheritIO();
            mBrowserProcess = pb.start();
        } catch (IOException e) {
            mLogger.failure("Failed to launch browser: " + e.getMessage());
        }
    }

    /**
     * Finds Chrome executable on macOS.
     */
    private String findMacChrome() {
        String[] candidates = {
            "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
            "/Applications/Chromium.app/Contents/MacOS/Chromium",
            System.getProperty("user.home") + "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
        };
        for (String path : candidates) {
            if (new File(path).exists()) {
                return path;
            }
        }
        return null;
    }

    /**
     * Finds Chrome executable on Windows.
     */
    private String findWindowsChrome() {
        String[] candidates = {
            System.getenv("ProgramFiles") + "\\Google\\Chrome\\Application\\chrome.exe",
            System.getenv("ProgramFiles(x86)") + "\\Google\\Chrome\\Application\\chrome.exe",
            System.getenv("LOCALAPPDATA") + "\\Google\\Chrome\\Application\\chrome.exe"
        };
        for (String path : candidates) {
            if (path != null && new File(path).exists()) {
                return path;
            }
        }
        return null;
    }

    /**
     * Finds Chrome/Chromium executable on Linux.
     */
    private String findLinuxChrome() {
        String[] candidates = {
            "google-chrome",
            "google-chrome-stable",
            "chromium-browser",
            "chromium"
        };
        for (String candidate : candidates) {
            try {
                Process p = Runtime.getRuntime().exec(new String[]{"which", candidate});
                if (p.waitFor() == 0) {
                    return candidate;
                }
            } catch (Exception ignored) {
            }
        }
        return null;
    }

    private synchronized void handleMessage(WsMessageContext ctx) {
        String message = ctx.message();
        mLogger.message("Processing Browser GUI message: >" + message + "<");

        // varUpdate$<varName>$<value> — write directly to the named SceneFlow variable.
        // The value may contain '$', so split with limit 3.
        if (message.startsWith("varUpdate$")) {
            String[] parts = message.split("\\$", 3);
            if (parts.length == 3 && mProject.hasVariable(parts[1])) {
                mProject.setVariable(parts[1], parts[2]);
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

    private SslContextFactory getSslContextFactory() {
        SslContextFactory sslContextFactory = new SslContextFactory.Server();
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

            // If text is empty - assume activity has empty text but has marker activities registered
            if (text.isEmpty()) {
                for (String tm : timemarks) {
                    mLogger.warning("Directly executing activity at timemark " + tm);
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                }
            } else {
                mLogger.warning("No gui command with CMD markers send ...");
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
