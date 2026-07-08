package de.dfki.vsm.vuppetmaster;

import android.annotation.SuppressLint;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import android.webkit.ConsoleMessage;
import android.webkit.JavascriptInterface;
import android.webkit.WebChromeClient;
import android.webkit.WebResourceRequest;
import android.webkit.WebResourceResponse;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.Button;
import android.widget.TextView;

import androidx.appcompat.app.AppCompatActivity;
import androidx.webkit.WebViewAssetLoader;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.json.JSONObject;

import de.dfki.vsm.runtime.CoreRuntime;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.api.android.AndroidRuntimeEndpoint;
import de.dfki.vsm.runtime.api.android.AndroidRuntimeServer;
import de.dfki.vsm.runtime.bootstrap.PlatformBootstrap;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.charamelEmbed.AndroidBridgeTransport;
import de.dfki.vsm.xtension.charamelEmbed.CharamelEmbedExecutor;

/**
 * M1: runs a bundled VSM project on-device and renders the VuppetMaster character in a WebView.
 *
 * <p>The character page ({@code character.html} + {@code vm-adapter.js}) is served to the WebView
 * over the secure {@code https://appassets.androidplatform.net} origin (via {@link WebViewAssetLoader})
 * — a secure context, which the VuppetMaster engine requires ({@code crypto.subtle}) to load the
 * model. {@code /vsm-config.js} is synthesized here from {@code BuildConfig} so the license key stays
 * out of the committed assets.
 *
 * <p>Transport: the {@link CharamelEmbedExecutor} runs on Android with an {@link AndroidBridgeTransport}.
 * VSM → page envelopes are pushed via {@code webView.evaluateJavascript("window.vsmDispatch(…)")};
 * page → VSM feedback arrives through the {@code AndroidVSM.send} JS bridge. The SceneFlow gates its
 * welcome utterance on the character-ready variable, so timing is robust regardless of model load time.
 */
public class MainActivity extends AppCompatActivity {

    private static final String TAG = "VSM-VuppetMaster";
    private static final String PROJECT_ASSET_DIR = "XeniaDemo";
    private static final String CHARACTER_AGENT = "Xenia";
    private static final String CHARACTER_URL = "https://appassets.androidplatform.net/character.html";

    // SceneFlow variables the UI reads/writes (must match project.xml / sceneflow.xml).
    private static final String VAR_READY = "avatar_ready";
    private static final String VAR_SPEAKING = "avatar_speaking";
    private static final String VAR_TURN = "turn_utterance";
    private static final String VAR_EMOTION = "emo_type";
    private static final String VAR_BACKGROUND = "bg_color";
    private static final String VAR_SCENE = "scene";

    private static final long MONITOR_INTERVAL_MS = 500L;

    // Runtime server for remote Web UI observation. Reachable from a desktop via:
    //   adb forward tcp:8091 tcp:8091   then connect the Web UI to http://127.0.0.1:8091
    private static final int RUNTIME_PORT = 8091;

    private final ExecutorService runtimeExecutor = Executors.newSingleThreadExecutor();
    private final Handler mainHandler = new Handler(Looper.getMainLooper());

    private WebView webView;
    private TextView statusText;
    private TextView runtimeInfo;

    private CoreRuntime runtime;
    private AndroidRuntimeEndpoint runtimeEndpoint;
    private AndroidRuntimeServer runtimeServer;
    private volatile AndroidBridgeTransport bridge;
    private boolean pageConnected;
    private Runnable monitorTask;

    @SuppressLint("SetJavaScriptEnabled")
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        webView = findViewById(R.id.characterWebView);
        statusText = findViewById(R.id.statusText);
        runtimeInfo = findViewById(R.id.runtimeInfo);
        wireControls();

        WebSettings settings = webView.getSettings();
        settings.setJavaScriptEnabled(true);
        settings.setDomStorageEnabled(true);
        // Let the engine start audio without an extra system gesture; the page's own overlay tap
        // still provides the AudioContext-unlock gesture the engine needs.
        settings.setMediaPlaybackRequiresUserGesture(false);

        final WebViewAssetLoader assetLoader = new WebViewAssetLoader.Builder()
                .addPathHandler("/", new WebViewAssetLoader.AssetsPathHandler(this))
                .build();

        webView.setWebViewClient(new CharacterWebViewClient(assetLoader));
        webView.setWebChromeClient(new WebChromeClient() {
            @Override
            public boolean onConsoleMessage(ConsoleMessage msg) {
                Log.d(TAG, "WebView console: " + msg.message()
                        + " @" + msg.sourceId() + ":" + msg.lineNumber());
                return true;
            }
        });
        // page → VSM feedback bridge (vm-adapter.js calls window.AndroidVSM.send(...))
        webView.addJavascriptInterface(new JsBridge(), "AndroidVSM");

        runtimeExecutor.execute(this::bootRuntime);
    }

    /** Wires the control panel: runtime controls + emotion/background buttons → SceneFlow vars. */
    private void wireControls() {
        // Runtime controls (android-stub pattern).
        ((Button) findViewById(R.id.btnStart)).setOnClickListener(v -> startRuntime());
        ((Button) findViewById(R.id.btnPause)).setOnClickListener(v -> togglePause((Button) v));
        ((Button) findViewById(R.id.btnStop)).setOnClickListener(v -> stopRuntime());

        // Emotions → emo_type; the SceneFlow "controls" supernode fires [Xenia emotion type=…].
        ((Button) findViewById(R.id.btnHappy)).setOnClickListener(v -> setSceneFlowVar(VAR_EMOTION, "happy"));
        ((Button) findViewById(R.id.btnSad)).setOnClickListener(v -> setSceneFlowVar(VAR_EMOTION, "sad"));
        ((Button) findViewById(R.id.btnAngry)).setOnClickListener(v -> setSceneFlowVar(VAR_EMOTION, "angry"));
        ((Button) findViewById(R.id.btnSurprise)).setOnClickListener(v -> setSceneFlowVar(VAR_EMOTION, "surprise"));
        ((Button) findViewById(R.id.btnSmile)).setOnClickListener(v -> setSceneFlowVar(VAR_EMOTION, "smile"));

        // Backgrounds → bg_color; the SceneFlow "controls" supernode fires [Xenia background color=…].
        ((Button) findViewById(R.id.btnBgBlue)).setOnClickListener(v -> setSceneFlowVar(VAR_BACKGROUND, "#1a2a6c"));
        ((Button) findViewById(R.id.btnBgRed)).setOnClickListener(v -> setSceneFlowVar(VAR_BACKGROUND, "#7a1f1f"));
        ((Button) findViewById(R.id.btnBgGreen)).setOnClickListener(v -> setSceneFlowVar(VAR_BACKGROUND, "#1f5f2f"));
        ((Button) findViewById(R.id.btnBgNone)).setOnClickListener(v -> setSceneFlowVar(VAR_BACKGROUND, "#000000"));

        // Scene → scene; the SceneFlow "controls" supernode plays the scene named by the variable.
        ((Button) findViewById(R.id.btnScene)).setOnClickListener(v -> setSceneFlowVar(VAR_SCENE, "machwas"));
    }

    // ---- runtime controls ----------------------------------------------------

    /** Sets a SceneFlow variable on the runtime thread (no-op if the runtime is not up). */
    private void setSceneFlowVar(String name, String value) {
        runtimeExecutor.execute(() -> {
            RunTimeProject p = (runtime != null) ? runtime.getRunTimeProject() : null;
            if (p != null && p.hasVariable(name)) {
                p.setVariable(name, value);
            }
        });
    }

    /** Start / restart via the endpoint (keeps remote Web UI state consistent). */
    private void startRuntime() {
        runtimeExecutor.execute(() -> {
            if (runtime == null) return;
            boolean wasRunning = runtime.isRunning();
            dispatch("Runtime.Start");
            // If it was stopped, the endpoint re-launched the project — that creates a fresh
            // character transport, so re-wire the WebView bridge. The page is still loaded, so
            // mark it connected and re-arm avatar_ready to replay the welcome.
            if (!wasRunning) {
                if (wireCharacterBridge() && bridge != null) {
                    bridge.markConnected();
                    RunTimeProject p = runtime.getRunTimeProject();
                    if (p.hasVariable(VAR_READY)) p.setVariable(VAR_READY, true);
                }
            }
            postStatus("Running");
        });
    }

    private void stopRuntime() {
        runtimeExecutor.execute(() -> {
            dispatch("Runtime.Stop");
            postStatus("Stopped");
        });
    }

    private void togglePause(Button button) {
        runtimeExecutor.execute(() -> {
            if (runtime == null) return;
            if (runtime.getRunTimeProject().isPaused()) {
                dispatch("Runtime.Resume");
                mainHandler.post(() -> button.setText("Pause"));
            } else {
                dispatch("Runtime.Pause");
                mainHandler.post(() -> button.setText("Resume"));
            }
        });
    }

    /** Dispatches a runtime command through the endpoint (so the remote Web UI stays in sync). */
    private void dispatch(String method) {
        if (runtimeEndpoint == null) return;
        JSONObject params = new JSONObject();
        try {
            params.put("projectId", runtimeEndpoint.projectId());
        } catch (Exception ignored) {
            // empty params is acceptable
        }
        runtimeEndpoint.dispatchCommand(method, params,
                runtimeServer == null ? null : runtimeServer.broadcaster());
    }

    /** Live readout of runtime state and bound SceneFlow variables (android-stub monitor pattern). */
    private void startMonitoring() {
        stopMonitoring();
        monitorTask = new Runnable() {
            @Override
            public void run() {
                RunTimeProject p = (runtime != null) ? runtime.getRunTimeProject() : null;
                if (p != null) {
                    String state = p.isRunning() ? (p.isPaused() ? "paused" : "running") : "stopped";
                    runtimeInfo.setText("runtime: " + state
                            + "  |  speaking=" + valueOf(p, VAR_SPEAKING)
                            + "  |  turn=" + valueOf(p, VAR_TURN));
                }
                mainHandler.postDelayed(this, MONITOR_INTERVAL_MS);
            }
        };
        mainHandler.post(monitorTask);
    }

    private void stopMonitoring() {
        if (monitorTask != null) {
            mainHandler.removeCallbacks(monitorTask);
            monitorTask = null;
        }
    }

    private static String valueOf(RunTimeProject p, String name) {
        if (!p.hasVariable(name)) return "—";
        AbstractValue v = p.getValueOf(name);
        return v == null ? "null" : String.valueOf(v.getValue());
    }

    /** Loads the project, wires the character bridge, then loads the WebView. */
    private void bootRuntime() {
        try {
            PlatformBootstrap.configureForAndroid();
            File projectDir = materializeProject();

            runtime = new CoreRuntime(projectDir);
            runtimeEndpoint = new AndroidRuntimeEndpoint(runtime, projectDir);

            // Runtime server for remote Web UI observation. Its event bridge registers on the
            // project's EventDispatcher, so runtime events flow to WS clients automatically.
            runtimeServer = new AndroidRuntimeServer(RUNTIME_PORT, runtimeEndpoint, "");
            runtimeServer.startServer();
            Log.i(TAG, "Runtime server on " + RUNTIME_PORT
                    + " — desktop: adb forward tcp:" + RUNTIME_PORT + " tcp:" + RUNTIME_PORT
                    + ", then connect Web UI to http://127.0.0.1:" + RUNTIME_PORT);

            // Start through the endpoint (launch()+start()) so remote state stays consistent. The
            // SceneFlow idles on avatar_ready, so it is safe to start before the page is wired.
            JSONObject start = dispatchResult("Runtime.Start");
            if (start == null || !"ok".equalsIgnoreCase(start.optString("status", ""))) {
                postStatus("Runtime start failed");
                return;
            }

            if (!wireCharacterBridge()) {
                return; // status already posted
            }

            mainHandler.post(this::startMonitoring);
            postStatus("Loading character…");
            mainHandler.post(() -> webView.loadUrl(CHARACTER_URL));
            // The page is marked connected in onPageFinished, once it can receive envelopes.
        } catch (Exception exc) {
            Log.e(TAG, "Boot failed", exc);
            postStatus("Boot error: " + exc.getMessage());
        }
    }

    /**
     * Obtains the character executor's Android bridge and wires the VSM → page envelope sink.
     * Called at boot and again after a restart (endpoint re-launch creates a fresh bridge).
     */
    private boolean wireCharacterBridge() {
        ActivityExecutor executor = runtime.getRunTimeProject().getAgentDevice(CHARACTER_AGENT);
        if (!(executor instanceof CharamelEmbedExecutor)) {
            postStatus("CharamelEmbedExecutor not loaded for agent '" + CHARACTER_AGENT + "'");
            return false;
        }
        AndroidBridgeTransport b = ((CharamelEmbedExecutor) executor).getAndroidBridge();
        if (b == null) {
            postStatus("Android bridge unavailable (not running on Android?)");
            return false;
        }
        // VSM → page: push each JSON envelope into the WebView on the UI thread.
        b.setEnvelopeSink(json -> mainHandler.post(() ->
                webView.evaluateJavascript("window.vsmDispatch(" + json + ")", null)));
        bridge = b;
        return true;
    }

    /** Like {@link #dispatch} but returns the endpoint's JSON response. */
    private JSONObject dispatchResult(String method) {
        if (runtimeEndpoint == null) return null;
        JSONObject params = new JSONObject();
        try {
            params.put("projectId", runtimeEndpoint.projectId());
        } catch (Exception ignored) {
            // empty params is acceptable
        }
        return runtimeEndpoint.dispatchCommand(method, params,
                runtimeServer == null ? null : runtimeServer.broadcaster());
    }

    /** Copies the bundled project from assets into internal storage and returns its directory. */
    private File materializeProject() throws IOException {
        File targetDir = new File(getFilesDir(), PROJECT_ASSET_DIR);
        if (!targetDir.exists() && !targetDir.mkdirs()) {
            throw new IOException("Cannot create project directory: " + targetDir);
        }
        String[] files = getAssets().list(PROJECT_ASSET_DIR);
        if (files == null || files.length == 0) {
            throw new IOException("No project files in assets/" + PROJECT_ASSET_DIR);
        }
        for (String name : files) {
            try (InputStream in = getAssets().open(PROJECT_ASSET_DIR + "/" + name);
                 FileOutputStream out = new FileOutputStream(new File(targetDir, name), false)) {
                byte[] buffer = new byte[8192];
                int read;
                while ((read = in.read(buffer)) >= 0) {
                    out.write(buffer, 0, read);
                }
            }
        }
        return targetDir;
    }

    /** window.VSM_CONFIG for the character page, built from BuildConfig (license key not in assets). */
    private String vsmConfigJs() {
        return "window.VSM_CONFIG={"
                + "\"licenseKey\":\"" + jsEscape(BuildConfig.VM_LICENSE_KEY) + "\","
                + "\"appName\":\"" + jsEscape(BuildConfig.VM_APP_NAME) + "\","
                + "\"engineUrl\":\"" + jsEscape(BuildConfig.VM_ENGINE_URL) + "\"};";
    }

    private static String jsEscape(String s) {
        if (s == null) return "";
        return s.replace("\\", "\\\\").replace("\"", "\\\"");
    }

    private void postStatus(String text) {
        Log.i(TAG, text);
        mainHandler.post(() -> statusText.setText(text));
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        stopMonitoring();
        if (runtimeServer != null) {
            runtimeServer.stopServer();
            runtimeServer = null;
        }
        if (runtime != null) {
            runtime.shutdown();
            runtime = null;
        }
        runtimeExecutor.shutdownNow();
    }

    // ---- page → VSM feedback bridge -----------------------------------------

    private final class JsBridge {
        @JavascriptInterface
        public void send(String message) {
            AndroidBridgeTransport b = bridge;
            if (b != null) b.feedback(message);
        }
    }

    // ---- WebView request handling -------------------------------------------

    private final class CharacterWebViewClient extends WebViewClient {
        private final WebViewAssetLoader assetLoader;

        CharacterWebViewClient(WebViewAssetLoader assetLoader) {
            this.assetLoader = assetLoader;
        }

        @Override
        public WebResourceResponse shouldInterceptRequest(WebView view, WebResourceRequest request) {
            String path = request.getUrl().getPath();
            if (path != null && path.endsWith("/vsm-config.js")) {
                return new WebResourceResponse("application/javascript", "utf-8",
                        new ByteArrayInputStream(vsmConfigJs().getBytes(StandardCharsets.UTF_8)));
            }
            return assetLoader.shouldInterceptRequest(request.getUrl());
        }

        @Override
        public void onPageFinished(WebView view, String url) {
            if (pageConnected || bridge == null) return;
            pageConnected = true;
            // The interpreter is already running (started at boot) and idling on avatar_ready.
            // Now that the page can receive envelopes, mark it connected; the executor flips
            // avatar_ready when the engine reports vm.ready, and the welcome plays.
            bridge.markConnected();
            postStatus("Running");
        }
    }
}
