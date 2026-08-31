package de.dfki.vsm.xtension.alma;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.interpreter.value.ListValue;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * Talks to a standalone ALMA affect server (ALMA2025's {@code de.affect.frontend.Server}) over
 * WebSocket instead of embedding the affect engine in-process. Appraisal actions on the SceneFlow
 * side are unchanged; only the transport to ALMA changed, from a direct Java call into a bundled
 * jar to an authenticated {@code /ws} connection.
 *
 * @author Patrick Gebhard
 */
public class ALMAExecutor extends ActivityExecutor implements AlmaWsClient.Listener {

    /**
     * Matches plugin-properties.json's "project" default and its "templates" block
     * (resourcePath "templates/", targetDirs ["alma"]) — the add-device dialog installs this file
     * via Project.Templates.Install, but that command never fires when a device is added through
     * the Flow Assistant instead, so launch() self-heals it here for that path.
     */
    private static final String DEFAULT_PROJECT_REL = "alma/default-project.xml";
    private static final String DEFAULT_PROJECT_RESOURCE = "/templates/" + DEFAULT_PROJECT_REL;

    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    private AlmaWsClient mClient;
    private ExecutorService mExecutor;
    private String mProjectXml;
    private String mProjectFileName;
    private String connectedVar;

    public ALMAExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    @Override
    public synchronized String marker(long id) {
        return "$(" + id + ")";
    }

    @Override
    public void launch() {
        String wsUrl = configOrDefault("ws_url", "");
        String tokenUrl = configOrDefault("keycloak_token_url", "");
        String clientId = configOrDefault("client_id", "");
        String clientSecret = configOrDefault("client_secret", "");
        String projectRel = configOrDefault("project", DEFAULT_PROJECT_REL);
        connectedVar = configOrDefault("connectedVar", "alma_connected");

        Path projectPath = Path.of(mProject.getProjectPath(), projectRel);
        mProjectFileName = projectPath.getFileName().toString();

        mClient = new AlmaWsClient(wsUrl, tokenUrl, clientId, clientSecret, this);
        mExecutor = Executors.newSingleThreadExecutor(r -> {
            Thread t = new Thread(r, "alma-ws");
            t.setDaemon(true);
            return t;
        });

        setBoolVar(connectedVar, false);

        // launch() runs synchronously on RunTimeProject's plugin-launch loop (the WS command
        // thread handling Runtime.Play) — connecting/authenticating must not block it.
        mExecutor.execute(() -> {
            try {
                if (DEFAULT_PROJECT_REL.equals(projectRel)) {
                    installDefaultProjectFileIfMissing(projectPath);
                }
                mProjectXml = Files.readString(projectPath);
                mClient.connectAndInit(mProjectXml, mProjectFileName);
                mLogger.message("[alma] connected to " + wsUrl);
                setBoolVar(connectedVar, true);
            } catch (Exception ex) {
                mLogger.failure("[alma] connect failed: " + ex.getMessage());
                setBoolVar(connectedVar, false);
            }
        });
    }

    @Override
    public void unload() {
        if (connectedVar != null && !connectedVar.isBlank()) {
            try {
                mProject.setVariable(connectedVar, false);
            } catch (Exception ignore) {
            }
        }
        if (mClient != null) {
            mClient.close();
        }
        if (mExecutor != null) {
            mExecutor.shutdownNow();
        }
    }

    @Override
    public void execute(AbstractActivity activity) {
        if (activity instanceof SpeechActivity) {
            SpeechActivity sa = (SpeechActivity) activity;
            String text = sa.getTextOnly("$(").trim();
            LinkedList<String> timemarks = sa.getTimeMarks("$(");

            // If text is empty - assume activity has empty text but has marker activities registered
            if (text.isEmpty()) {
                for (String tm : timemarks) {
                    mLogger.warning("[alma] directly executing activity at timemark " + tm);
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                }
            }
            return;
        }

        final String name = activity.getName();
        final LinkedList<ActionFeature> features = activity.getFeatures();

        if ("reset".equalsIgnoreCase(name)) {
            mLogger.message("[alma] reset for " + activity.getActor() + " (re-uploading project)");
            mExecutor.execute(() -> {
                try {
                    mClient.reset(mProjectXml, mProjectFileName);
                } catch (Exception ex) {
                    mLogger.failure("[alma] reset failed: " + ex.getMessage());
                }
            });
            return;
        }

        if (AlmaWsClient.isAppraisalTag(name)) {
            final String character = activity.getActor();
            String rawElicitor = getActionFeatureValue("elicitor", features);
            final String elicitor = rawElicitor.isEmpty() ? "Scene" : rawElicitor;
            mLogger.message("[alma] appraisal " + name + " for " + character + " (elicitor=" + elicitor + ")");
            mExecutor.execute(() -> mClient.sendAppraisal(character, name, "1.00", elicitor));
        }
    }

    @Override
    public void onAuthResult(boolean ok) {
        if (ok) {
            mLogger.message("[alma] authenticated");
        } else {
            mLogger.failure("[alma] authentication failed");
        }
    }

    @Override
    public void onError(String message) {
        mLogger.failure("[alma] " + message);
    }

    @Override
    public void onClose(int statusCode, String reason) {
        mLogger.warning("[alma] connection closed (" + statusCode + ") " + reason);
        setBoolVar(connectedVar, false);
    }

    @Override
    public void onAffectInfo(String character, String dominantEmotionType, double dominantEmotionIntensity,
                              String moodName, String moodTendencyName) {
        try {
            mProject.setVariable("almadominantemotion", dominantEmotionType);
            mProject.setVariable("almadominantemotionintensity", String.valueOf(dominantEmotionIntensity));
            mProject.setVariable("almamood", moodName);
            mProject.setVariable("almamoodtendency", moodTendencyName);
        } catch (Exception e) {
            mLogger.warning("[alma] could not set affect variables, project not running");
        }
    }

    @Override
    public void onEmotionVector(String character, List<String> activeEmotions) {
        try {
            LinkedList<AbstractValue> valueList = new LinkedList<>();
            for (String emotion : activeEmotions) {
                valueList.add(new StringValue(emotion));
            }
            mProject.setVariable("useremotions", new ListValue(valueList));
        } catch (Exception e) {
            mLogger.warning("[alma] could not set useremotions, project not running");
        }
    }

    private void installDefaultProjectFileIfMissing(Path projectPath) throws IOException {
        if (Files.exists(projectPath)) {
            return;
        }
        try (InputStream in = ALMAExecutor.class.getResourceAsStream(DEFAULT_PROJECT_RESOURCE)) {
            if (in == null) {
                return;
            }
            Files.createDirectories(projectPath.getParent());
            Files.copy(in, projectPath);
            mLogger.message("[alma] installed default project XML at " + projectPath);
        }
    }

    private String getActionFeatureValue(String name, LinkedList<ActionFeature> features) {
        for (ActionFeature af : features) {
            if (af.getKey().equalsIgnoreCase(name)) {
                return af.getVal();
            }
        }
        return "";
    }

    private String configOrDefault(String key, String fallback) {
        String v = mConfig.getProperty(key);
        return (v == null || v.isBlank()) ? fallback : v;
    }

    /**
     * Writes a Bool SceneFlow variable, retrying on the dedicated "alma-ws" thread if the
     * interpreter isn't ready yet (setVariable returns false until its configuration is active).
     * Safe to call from any thread (launch's async block, or a WebSocket callback thread) since it
     * only ever enqueues onto mExecutor, never blocks the caller.
     */
    private void setBoolVar(final String varName, final boolean value) {
        setBoolVar(varName, value, 20);
    }

    private void setBoolVar(final String varName, final boolean value, final int retriesLeft) {
        if (varName == null || varName.isBlank() || mExecutor == null) {
            return;
        }
        try {
            mExecutor.execute(() -> {
                try {
                    if (mProject.setVariable(varName, value)) {
                        return;
                    }
                } catch (Exception ignore) {
                }
                if (retriesLeft > 0) {
                    try {
                        Thread.sleep(250);
                    } catch (InterruptedException ie) {
                        Thread.currentThread().interrupt();
                        return;
                    }
                    setBoolVar(varName, value, retriesLeft - 1);
                }
            });
        } catch (Exception ignore) {
        }
    }
}
