package de.dfki.vsm.androidstub;

import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;
import android.text.Editable;
import android.text.TextWatcher;

import androidx.appcompat.app.AppCompatActivity;

import de.dfki.vsm.runtime.CoreRuntime;
import de.dfki.vsm.runtime.api.android.AndroidRuntimeEndpoint;
import de.dfki.vsm.runtime.api.android.AndroidRuntimeServer;
import de.dfki.vsm.runtime.bootstrap.PlatformBootstrap;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.xtension.androidGui.AndroidActivity;
import de.dfki.vsm.xtension.androidGui.AndroidGuiExecutor;
import de.dfki.vsm.xtension.androidGui.AndroidLabel;
import org.json.JSONObject;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class MainActivity extends AppCompatActivity {

    private static final String[] SIMPLE_PROJECT_FILES = {
            "project.xml",
            "sceneflow.xml",
            "scenescript.xml",
            "acticon.xml",
            "gesticon.xml",
            "visicon.xml",
            "editorconfig.xml"
    };

    private final ExecutorService runtimeExecutor = Executors.newSingleThreadExecutor();
    private final Handler mainHandler = new Handler(Looper.getMainLooper());

    private TextView statusText;
    private TextView logText;
    private TextView counterLabel;
    private TextView messageLabel;
    private EditText inputField;
    private Button submitButton;

    private CoreRuntime runtime;
    private AndroidRuntimeEndpoint runtimeEndpoint;
    private AndroidRuntimeServer runtimeServer;
    private Runnable monitorTask;
    private boolean androidGuiBound;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        statusText = findViewById(R.id.statusText);
        logText = findViewById(R.id.logText);
        counterLabel = findViewById(R.id.counterLabel);
        messageLabel = findViewById(R.id.messageLabel);
        inputField = findViewById(R.id.inputField);
        submitButton = findViewById(R.id.submitButton);
        Button startButton = findViewById(R.id.startButton);
        Button stopButton = findViewById(R.id.stopButton);

        startButton.setOnClickListener(v -> startRuntime());
        stopButton.setOnClickListener(v -> stopRuntime("Stopped by user"));

        initializeBackend();
        appendLog("Ready. Tap Start to run doc/SimpleProject with TimerExecutor.");
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        stopRuntime("Activity destroyed");
        if (runtimeServer != null) {
            runtimeServer.stopServer();
            runtimeServer = null;
        }
        runtimeExecutor.shutdownNow();
    }

    private void startRuntime() {
        if (runtime != null && runtime.isRunning()) {
            appendLog("Runtime is already running.");
            return;
        }

        statusText.setText("Starting runtime...");
        runtimeExecutor.execute(() -> {
            try {
                ensureBackendReady();

                JSONObject startResponse = runtimeEndpoint.dispatchCommand(
                        "Runtime.Start",
                        commandParams(runtimeEndpoint.projectId()),
                        runtimeServer == null ? null : runtimeServer.broadcaster()
                );
                if (!"ok".equalsIgnoreCase(startResponse.optString("status", ""))) {
                    postError("Runtime start failed: " + startResponse.optString("message", "unknown"));
                    return;
                }

                postStatus("Running");
                appendLogFromWorker("Runtime started using project at: " + runtimeEndpoint.projectPath());
                startMonitoring();
            } catch (Exception exc) {
                postError("Startup error: " + exc.getMessage());
            }
        });
    }

    private void initializeBackend() {
        runtimeExecutor.execute(() -> {
            try {
                ensureBackendReady();
                startMonitoring();
                postStatus("Idle");
            } catch (Exception exc) {
                postError("Backend init error: " + exc.getMessage());
            }
        });
    }

    private void ensureBackendReady() throws Exception {
        PlatformBootstrap.configureForAndroid();
        File projectDir = materializeSimpleProject();

        if (runtime == null) {
            runtime = new CoreRuntime(projectDir);
            runtimeEndpoint = new AndroidRuntimeEndpoint(runtime, projectDir);
            bindAndroidGuiBridge();
        }

        if (runtimeServer == null) {
            runtimeServer = new AndroidRuntimeServer(8091, runtimeEndpoint, "");
            runtimeServer.startServer();
            appendLogFromWorker("Android runtime server listening on port 8091.");
            appendLogFromWorker("From desktop: adb forward tcp:8091 tcp:8091");
            appendLogFromWorker("Then open Web UI against http://127.0.0.1:8091");
        }
    }

    private void bindAndroidGuiBridge() {
        if (runtime == null || runtime.getRunTimeProject() == null || androidGuiBound) {
            return;
        }
        final ActivityExecutor executor = runtime.getRunTimeProject().getAgentDevice("phone");
        if (!(executor instanceof AndroidGuiExecutor)) {
            appendLogFromWorker("AndroidGuiExecutor not loaded for agent 'phone'.");
            return;
        }
        final AndroidGuiExecutor guiExecutor = (AndroidGuiExecutor) executor;
        final AndroidActivity androidActivity = guiExecutor.getActivity("phone");
        guiExecutor.bindLauncher(name -> appendLogFromWorker("Android GUI show: " + name));

        final TextView counterView = counterLabel;
        if (counterView != null) {
            androidActivity.getLabels().put("counterLabel", new AndroidLabel() {
                @Override
                public void accept(String value) {
                    mainHandler.post(() -> counterView.setText(value == null ? "" : value));
                }
            });
        }

        final TextView messageView = messageLabel;
        if (messageView != null) {
            androidActivity.getLabels().put("messageLabel", new AndroidLabel() {
                @Override
                public void accept(String value) {
                    mainHandler.post(() -> messageView.setText(value == null ? "" : value));
                }
            });
        }

        final EditText inputView = inputField;
        if (inputView != null) {
            if (androidActivity.getEditFields().containsKey("inputField")) {
                androidActivity.getEditFields().get("inputField").setText = value ->
                        mainHandler.post(() -> {
                            final String next = value == null ? "" : value;
                            final String current = inputView.getText() == null ? "" : inputView.getText().toString();
                            if (!next.equals(current)) {
                                inputView.setText(next);
                                inputView.setSelection(next.length());
                            }
                        });
            }
            inputView.addTextChangedListener(new TextWatcher() {
                @Override
                public void beforeTextChanged(CharSequence s, int start, int count, int after) {
                }

                @Override
                public void onTextChanged(CharSequence s, int start, int before, int count) {
                }

                @Override
                public void afterTextChanged(Editable s) {
                    pushInputFieldToRuntime(androidActivity, s == null ? "" : s.toString());
                }
            });
            inputView.setOnFocusChangeListener((v, hasFocus) -> {
                if (!hasFocus) {
                    pushInputFieldToRuntime(androidActivity, inputView.getText() == null ? "" : inputView.getText().toString());
                }
            });
        }

        final Button submitView = submitButton;
        if (submitView != null && androidActivity.getButtons().containsKey("submitButton")) {
            androidActivity.getButtons().get("submitButton").setText = text ->
                    mainHandler.post(() -> submitView.setText(text == null ? "" : text));
            submitView.setOnClickListener(v -> {
                if (inputView != null) {
                    pushInputFieldToRuntime(androidActivity,
                            inputView.getText() == null ? "" : inputView.getText().toString());
                }
                if (androidActivity.getButtons().get("submitButton").onClick != null) {
                    androidActivity.getButtons().get("submitButton").onClick.accept(null);
                }
            });
        }

        androidGuiBound = true;
        appendLogFromWorker("AndroidGui bridge bound for agent 'phone'.");
    }

    private void pushInputFieldToRuntime(AndroidActivity androidActivity, String value) {
        if (androidActivity.getEditFields().containsKey("inputField")
                && androidActivity.getEditFields().get("inputField").textChanged != null) {
            androidActivity.getEditFields().get("inputField").textChanged.accept(value == null ? "" : value);
        }
    }

    private void stopRuntime(String reason) {
        CoreRuntime local = runtime;
        AndroidRuntimeEndpoint endpoint = runtimeEndpoint;
        if (local != null && endpoint != null) {
            runtimeExecutor.execute(() -> {
                endpoint.dispatchCommand(
                        "Runtime.Stop",
                        commandParams(endpoint.projectId()),
                        runtimeServer == null ? null : runtimeServer.broadcaster()
                );
                postStatus("Idle");
                appendLogFromWorker(reason);
            });
        } else {
            statusText.setText("Idle");
        }
    }

    private void startMonitoring() {
        stopMonitoring();
        monitorTask = new Runnable() {
            @Override
            public void run() {
                CoreRuntime local = runtime;
                AndroidRuntimeEndpoint endpoint = runtimeEndpoint;
                if (local == null || endpoint == null) {
                    return;
                }
                AbstractValue cnt = local.getRunTimeProject().getValueOf("cnt");
                AbstractValue time = local.getRunTimeProject().getValueOf("time");
                String cntText = cnt == null ? "null" : String.valueOf(cnt.getValue());
                String timeText = time == null ? "null" : String.valueOf(time.getValue());
                String state = endpoint.runtimeState();
                if (state == null || state.isBlank()) {
                    state = local.isRunning() ? "running" : "stopped";
                } else if ("running".equalsIgnoreCase(state) && !local.isRunning()) {
                    state = "stopped";
                }

                if ("running".equalsIgnoreCase(state)) {
                    statusText.setText("Running | cnt=" + cntText + " | time=" + timeText + "ms");
                } else if ("paused".equalsIgnoreCase(state)) {
                    statusText.setText("Paused | cnt=" + cntText + " | time=" + timeText + "ms");
                } else {
                    statusText.setText("Idle");
                }
                mainHandler.postDelayed(this, 500L);
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

    private File materializeSimpleProject() throws IOException {
        File targetDir = new File(getFilesDir(), "SimpleProject");
        if (!targetDir.exists() && !targetDir.mkdirs()) {
            throw new IOException("Cannot create project directory: " + targetDir);
        }

        for (String fileName : SIMPLE_PROJECT_FILES) {
            copyAsset("SimpleProject/" + fileName, new File(targetDir, fileName));
        }
        return targetDir;
    }

    private void copyAsset(String assetPath, File destination) throws IOException {
        try (InputStream in = getAssets().open(assetPath);
             FileOutputStream out = new FileOutputStream(destination, false)) {
            byte[] buffer = new byte[8192];
            int read;
            while ((read = in.read(buffer)) >= 0) {
                out.write(buffer, 0, read);
            }
        }
    }

    private void postStatus(String text) {
        mainHandler.post(() -> statusText.setText(text));
    }

    private void postError(String message) {
        mainHandler.post(() -> {
            statusText.setText("Error");
            appendLog(message);
        });
    }

    private void appendLogFromWorker(String message) {
        mainHandler.post(() -> appendLog(message));
    }

    private void appendLog(String message) {
        String old = logText.getText().toString();
        String entry = message + "\n";
        logText.setText(old + entry);
    }

    private JSONObject commandParams(String projectId) {
        JSONObject params = new JSONObject();
        try {
            params.put("projectId", projectId);
        } catch (Exception ignored) {
            // Fallback to empty params for safety.
        }
        return params;
    }
}
