package de.dfki.vsm.xtension.alma;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
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

    /**
     * VSM auto-registers one agent per ALMA CharacterAffect as "&lt;Character&gt;_alma" (see
     * AlmaAgentSyncService in core-webserver), but the ALMA server itself only knows the bare
     * character name from its own project XML — strip the suffix before it goes out the wire.
     */
    private static final String AGENT_SUFFIX = "_alma";

    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    private AlmaWsClient mClient;
    private ExecutorService mExecutor;
    private String mProjectXml;
    private String mProjectFileName;
    private String connectedVar;
    private volatile AlmaActRules mActRules = AlmaActRules.parse(null);

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
                mActRules = AlmaActRules.parse(mProjectXml);
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

        final String character = stripAgentSuffix(activity.getActor());

        if (AlmaWsClient.isAppraisalTag(name)) {
            String rawElicitor = getActionFeatureValue("elicitor", features);
            final String elicitor = rawElicitor.isEmpty() ? "Scene" : rawElicitor;
            final String intensity = intensityOrDefault(getActionFeatureValue("intensity", features));
            mLogger.message("[alma] appraisal " + name + " for " + character
                    + " (intensity=" + intensity + ", elicitor=" + elicitor + ")");
            mExecutor.execute(() -> mClient.sendAppraisal(character, name, intensity, elicitor));
            return;
        }

        // A named dialogue act (e.g. "Accuse", "Insult") authored on this character/agent, resolved
        // client-side to the basic appraisal tag(s) the wire protocol actually understands. A
        // "performer" feature picks the DirectAct/IndirectAct rule for the one who did it to this
        // character; without one, only that character's own SelfAct rule is tried. "hearer" names
        // who the act is addressed to/heard by and goes out on the wire alongside elicitor; it plays
        // no part in rule resolution.
        final String performer = getActionFeatureValue("performer", features);
        final List<String> resolvedTags = mActRules.resolve(character, name, performer);
        if (!resolvedTags.isEmpty()) {
            String rawElicitor = getActionFeatureValue("elicitor", features);
            final String elicitor = rawElicitor.isEmpty() ? "Scene" : rawElicitor;
            final String intensity = intensityOrDefault(getActionFeatureValue("intensity", features));
            final String hearer = getActionFeatureValue("hearer", features);
            mLogger.message("[alma] act " + name + " for " + character
                    + (performer.isEmpty() ? "" : " (performer=" + performer + ")")
                    + (hearer.isEmpty() ? "" : " (hearer=" + hearer + ")")
                    + " -> " + resolvedTags + " (intensity=" + intensity + ", elicitor=" + elicitor + ")");
            mExecutor.execute(() -> {
                for (String tag : resolvedTags) {
                    mClient.sendAppraisal(character, tag, intensity, elicitor, hearer);
                }
            });
        } else {
            // All 48 dialogue acts are offered as selectable commands for every ALMA-bound agent
            // (plugin-properties.json declares them statically, since which ones actually resolve
            // is per-character data this executor only has once connected) — so picking one a
            // character's project XML doesn't define a SelfAct/DirectAct/IndirectAct rule for is a
            // real, silent no-op an author needs to see, not a routing mistake to stay quiet about.
            mLogger.warning("[alma] act '" + name + "' has no rule for " + character
                    + (performer.isEmpty() ? "" : " with performer=" + performer) + ", nothing sent");
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
        if (character == null || character.isBlank()) {
            mLogger.warning("[alma] affect update with no character, ignoring");
            return;
        }
        String prefix = character + AGENT_SUFFIX + "_";
        try {
            mProject.setVariable(prefix + "dominantemotion", dominantEmotionType);
            mProject.setVariable(prefix + "dominantemotionintensity", String.valueOf(dominantEmotionIntensity));
            mProject.setVariable(prefix + "mood", moodName);
            mProject.setVariable(prefix + "moodtendency", moodTendencyName);
        } catch (Exception e) {
            mLogger.warning("[alma] could not set affect variables for " + character + ", project not running");
        }
    }

    @Override
    public void onEmotionVector(String character, List<String> activeEmotions) {
        if (character == null || character.isBlank()) {
            mLogger.warning("[alma] emotion vector with no character, ignoring");
            return;
        }
        try {
            // The declared variable is String-typed (see AlmaAgentSyncService/perAgentWrites) —
            // writing a ListValue into it throws "[] has wrong type" at SymbolEntry.write, so the
            // active emotions are joined into one comma-separated string instead.
            mProject.setVariable(character + AGENT_SUFFIX + "_emotions", String.join(",", activeEmotions));
        } catch (Exception e) {
            mLogger.warning("[alma] could not set emotions for " + character + ", project not running");
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

    private static String stripAgentSuffix(String actor) {
        if (actor != null && actor.endsWith(AGENT_SUFFIX)) {
            return actor.substring(0, actor.length() - AGENT_SUFFIX.length());
        }
        return actor;
    }

    private static String intensityOrDefault(String rawIntensity) {
        return rawIntensity == null || rawIntensity.isEmpty() ? "1" : rawIntensity;
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
