package de.dfki.vsm.xtension.charamelEmbed;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import io.javalin.Javalin;
import io.javalin.websocket.WsCloseContext;
import io.javalin.websocket.WsContext;
import io.javalin.websocket.WsMessageContext;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Desktop transport: an embedded Jetty/Javalin server that hosts the VuppetMaster character page
 * ({@code /renderer/character.html} + {@code vm-adapter.js}) and exchanges command envelopes and
 * feedback with it over a WebSocket. In {@code --secure} mode the page is served over HTTPS/WSS
 * using the shared mkcert host certificate so the browser has the secure context the engine needs.
 *
 * <p>All Jetty/Javalin references live here so {@link CharamelEmbedExecutor} can class-load on
 * Android (where these classes are absent) via {@link AndroidBridgeTransport}.
 */
public final class JettyTransport implements CharamelTransport {

    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final Listener mListener;
    private final Set<WsContext> mWebSockets = ConcurrentHashMap.newKeySet();
    // Sessions whose connecting page loaded the URL with ?vsmPreview=1 — i.e. the authoring-time
    // SIA preview panel, as opposed to any other viewer (a "follow the player" audience page, a
    // plain browser tab, …). Filtered out of send() while mPreviewMuted is set.
    private final Set<WsContext> mPreviewWebSockets = ConcurrentHashMap.newKeySet();
    private volatile boolean mPreviewMuted = false;

    // Desktop-only config, read from the plugin config here so the executor holds no reference
    // to this class (it is created reflectively) and thus stays free of Jetty/Javalin/AWT.
    private final int mPort;
    private final String mLicenseKey;
    private final String mAppName;
    private final String mEngineUrl;
    private final boolean mAutostartBrowser;
    private final String mBrowserPref;
    private final boolean mFullscreen;

    private Javalin mApp;
    private Process mBrowserProcess;

    /**
     * Reflectively invoked by {@link CharamelEmbedExecutor} on desktop. The public constructor and
     * its {@code (PluginConfig, Listener)} signature form the contract the executor loads by name.
     */
    public JettyTransport(PluginConfig config, Listener listener) {
        this.mListener = listener;
        this.mPort = Integer.parseInt(Objects.requireNonNull(config.getProperty("port")));
        this.mLicenseKey = config.getProperty("licenseKey", "");
        this.mAppName = config.getProperty("appName", "");
        this.mEngineUrl = config.getProperty("engineUrl",
                "https://engine.vuppetmaster.com/api/engine/vuppetmaster.iife.js");
        this.mAutostartBrowser = "true".equalsIgnoreCase(config.getProperty("autostart_browser"));
        this.mBrowserPref = config.getProperty("browser", "");
        this.mFullscreen = "true".equalsIgnoreCase(config.getProperty("browser_fullscreen"));
    }

    @Override
    public void start() {
        // Secure mode (host started with --secure): serve the character page over TLS using the
        // shared mkcert host certificate. A remote browser then loads the character from an https
        // origin — a secure context, which VuppetMaster's engine requires (crypto.subtle).
        final boolean secureMode = de.dfki.vsm.runtime.tls.TlsRuntimeContext.isEnabled();
        Javalin app = Javalin.create(config -> {
            config.jetty.modifyWebSocketServletFactory(f -> f.setIdleTimeout(java.time.Duration.ofMinutes(10)));
            config.jetty.modifyServletContextHandler(handler ->
                handler.addFilter(new org.eclipse.jetty.servlet.FilterHolder(new PnaFilter()), "/*",
                    java.util.EnumSet.of(jakarta.servlet.DispatcherType.REQUEST)));
            if (secureMode) {
                config.jetty.modifyServer(server -> server.setConnectors(
                        new org.eclipse.jetty.server.Connector[]{ sharedTlsConnector(server, mPort) }));
            }
        });
        mApp = secureMode ? app.start() : app.start(mPort);
        if (secureMode) {
            mLogger.message("--secure: serving character page over https :" + mPort);
        }

        mApp.get("/", ctx -> ctx.redirect("/character.html"));
        mApp.get("/character.html", ctx -> serveResource(ctx, "/renderer/character.html", "text/html"));
        mApp.get("/vm-adapter.js", ctx -> serveResource(ctx, "/renderer/vm-adapter.js", "application/javascript"));

        // Injects license/appName/engine URL into the page without editing the HTML.
        mApp.get("/vsm-config.js", ctx -> ctx.contentType("application/javascript").result(
            "window.VSM_CONFIG=" +
                "{\"licenseKey\":\"" + escapeJson(mLicenseKey) + "\"," +
                 "\"appName\":\"" + escapeJson(mAppName) + "\"," +
                 "\"engineUrl\":\"" + escapeJson(mEngineUrl) + "\"};"));

        mApp.ws("/ws", ws -> {
            ws.onConnect(ctx -> {
                mWebSockets.add(ctx);
                if ("1".equals(ctx.queryParam("vsmPreview"))) mPreviewWebSockets.add(ctx);
                if (mListener != null) mListener.onConnected();
            });
            ws.onMessage(this::onWsMessage);
            ws.onClose(this::onWsClose);
            ws.onError(ctx -> {
                Throwable t = ctx.error();
                // A closed browser tab/window (rather than a clean WS close handshake) surfaces
                // here as an EOF-style exception with no message — routine, not a real problem, so
                // it's logged at info level instead of SEVERE (confirmed 2026-07-18: alarmed a user
                // who simply closed the preview's browser window mid-session).
                String message = t != null ? t.getMessage() : null;
                if (message != null) {
                    mLogger.failure("WebSocket error: " + message);
                } else {
                    mLogger.message("WebSocket closed (client disconnect): "
                            + (t != null ? t.getClass().getSimpleName() : "unknown"));
                }
            });
        });

        if (mAutostartBrowser) {
            String scheme = secureMode ? "https" : "http";
            launchBrowser(scheme + "://127.0.0.1:" + mPort + "/character.html");
        }
    }

    @Override
    public void send(String json) {
        // A socket whose peer vanished without a clean close handshake (tab force-closed, laptop
        // slept, network dropped) lingers in mWebSockets until Jetty's own 10-minute idle timeout
        // notices — onWsClose (the only other place these sets are pruned) never fires for it in
        // the meantime. Without the try/catch below, that one dead entry's ws.send() throws and
        // aborts this whole loop, silently starving every *live* socket that iterates after it —
        // including a preview page reloaded specifically to recover from this exact situation
        // (confirmed 2026-07-20: the "Reload preview" button alone didn't fix a wedged preview,
        // because the fresh session it created could still be starved by an older, undetected-dead
        // one still sitting in this set). Evict on failure so the set self-heals immediately instead
        // of waiting out the idle timeout.
        for (WsContext ws : mWebSockets) {
            if (mPreviewMuted && mPreviewWebSockets.contains(ws)) continue;
            try {
                ws.send(json);
            } catch (Exception e) {
                mLogger.warning("Dropping dead character-page WebSocket session: " + e.getMessage());
                mWebSockets.remove(ws);
                mPreviewWebSockets.remove(ws);
            }
        }
    }

    @Override
    public void setPreviewMuted(boolean muted) {
        mPreviewMuted = muted;
    }

    @Override
    public boolean isConnected() {
        return !mWebSockets.isEmpty();
    }

    @Override
    public int getPreviewPort() {
        return mApp != null ? mPort : -1;
    }

    @Override
    public void stop() {
        mWebSockets.clear();
        mPreviewWebSockets.clear();
        if (mApp != null) {
            mApp.stop();
            mApp = null;
        }
        if (mBrowserProcess != null) {
            mBrowserProcess.destroy();
            try {
                if (!mBrowserProcess.waitFor(2, java.util.concurrent.TimeUnit.SECONDS)) {
                    mBrowserProcess.destroyForcibly();
                }
            } catch (InterruptedException e) {
                mBrowserProcess.destroyForcibly();
            }
            mBrowserProcess = null;
        }
    }

    private void onWsMessage(WsMessageContext ctx) {
        if (mListener != null) mListener.onMessage(ctx.message());
    }

    private void onWsClose(WsCloseContext ctx) {
        mWebSockets.remove(ctx);
        mPreviewWebSockets.remove(ctx);
        if (mListener != null) mListener.onDisconnected();
    }

    private void serveResource(io.javalin.http.Context ctx, String path, String contentType) {
        InputStream stream = getClass().getResourceAsStream(path);
        if (stream != null) ctx.result(stream).contentType(contentType);
        else ctx.status(404);
    }

    /** TLS connector on the given port using the shared mkcert host keystore (--secure mode). */
    private static org.eclipse.jetty.server.ServerConnector sharedTlsConnector(
            org.eclipse.jetty.server.Server server, int port) {
        org.eclipse.jetty.util.ssl.SslContextFactory.Server factory =
                new org.eclipse.jetty.util.ssl.SslContextFactory.Server();
        factory.setKeyStorePath(de.dfki.vsm.runtime.tls.TlsRuntimeContext.getKeyStorePath());
        factory.setKeyStorePassword(de.dfki.vsm.runtime.tls.TlsRuntimeContext.getKeyStorePassword());
        factory.setKeyStoreType("PKCS12");
        org.eclipse.jetty.server.ServerConnector connector = new org.eclipse.jetty.server.ServerConnector(server,
                new org.eclipse.jetty.server.SslConnectionFactory(factory, "http/1.1"),
                new org.eclipse.jetty.server.HttpConnectionFactory());
        connector.setPort(port);
        return connector;
    }

    // ------------------------------------------------------------------ browser autostart

    /** Opens the character page in a browser. OS default unless a "browser" path is configured. */
    private void launchBrowser(String url) {
        String browserPref = mBrowserPref;
        boolean fullscreen = mFullscreen;
        if ((browserPref == null || browserPref.isBlank() || "default".equalsIgnoreCase(browserPref)) && !fullscreen) {
            try {
                if (java.awt.Desktop.isDesktopSupported()
                        && java.awt.Desktop.getDesktop().isSupported(java.awt.Desktop.Action.BROWSE)) {
                    java.awt.Desktop.getDesktop().browse(new java.net.URI(url));
                    mLogger.message("Opened default browser: " + url);
                    return;
                }
            } catch (Exception e) {
                mLogger.warning("Default browser open failed: " + e.getMessage());
            }
        }
        String chrome = (browserPref != null && !browserPref.isBlank()
                && !"default".equalsIgnoreCase(browserPref) && !"chrome".equalsIgnoreCase(browserPref))
                ? browserPref : findChrome();
        if (chrome == null) {
            mLogger.warning("Chrome not found; cannot auto-start browser.");
            return;
        }
        List<String> command = new ArrayList<>();
        command.add(chrome);
        if (fullscreen) command.add("--start-fullscreen");
        command.add("--new-window");
        command.add(url);
        try {
            ProcessBuilder pb = new ProcessBuilder(command);
            pb.inheritIO();
            mBrowserProcess = pb.start();
            mLogger.message("Launching browser: " + String.join(" ", command));
        } catch (IOException e) {
            mLogger.failure("Failed to launch browser: " + e.getMessage());
        }
    }

    private String findChrome() {
        String os = System.getProperty("os.name", "").toLowerCase();
        if (os.contains("mac")) {
            String[] candidates = {
                "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
                "/Applications/Chromium.app/Contents/MacOS/Chromium"
            };
            for (String p : candidates) if (new File(p).exists()) return p;
        } else if (os.contains("win")) {
            String[] candidates = {
                System.getenv("ProgramFiles") + "\\Google\\Chrome\\Application\\chrome.exe",
                System.getenv("ProgramFiles(x86)") + "\\Google\\Chrome\\Application\\chrome.exe"
            };
            for (String p : candidates) if (p != null && new File(p).exists()) return p;
        } else if (os.contains("linux")) {
            for (String c : new String[]{"google-chrome", "google-chrome-stable", "chromium-browser", "chromium"}) {
                try {
                    Process p = Runtime.getRuntime().exec(new String[]{"which", c});
                    if (p.waitFor() == 0) return c;
                } catch (Exception ignored) {}
            }
        }
        return null;
    }

    private static String escapeJson(String s) {
        if (s == null) return "";
        return s.replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t");
    }

    // Adds PNA/CORS headers so a page loading the engine from a public origin can reach this
    // localhost WebSocket under Chrome's Private Network Access rules (same approach as charamel-ws).
    private static final class PnaFilter implements jakarta.servlet.Filter {
        @Override
        public void doFilter(jakarta.servlet.ServletRequest req, jakarta.servlet.ServletResponse res,
                             jakarta.servlet.FilterChain chain)
                throws IOException, jakarta.servlet.ServletException {
            jakarta.servlet.http.HttpServletRequest httpReq = (jakarta.servlet.http.HttpServletRequest) req;
            jakarta.servlet.http.HttpServletResponse httpRes = (jakarta.servlet.http.HttpServletResponse) res;
            httpRes.setHeader("Access-Control-Allow-Origin", "*");
            httpRes.setHeader("Access-Control-Allow-Private-Network", "true");
            if ("OPTIONS".equalsIgnoreCase(httpReq.getMethod())) {
                httpRes.setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
                httpRes.setHeader("Access-Control-Allow-Headers",
                    "Upgrade, Connection, Sec-WebSocket-Key, Sec-WebSocket-Version, Sec-WebSocket-Protocol");
                httpRes.setStatus(jakarta.servlet.http.HttpServletResponse.SC_OK);
                return;
            }
            chain.doFilter(req, res);
        }
        @Override public void init(jakarta.servlet.FilterConfig fc) {}
        @Override public void destroy() {}
    }
}
