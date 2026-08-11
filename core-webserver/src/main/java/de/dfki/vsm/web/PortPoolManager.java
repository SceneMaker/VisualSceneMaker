package de.dfki.vsm.web;

import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import org.json.JSONObject;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * Phase 4 of doc/vsm-workspace-platform-plan.md: overrides a project's plugin port
 * configuration with dynamically-allocated ports from a shared pool, instead of trusting
 * whatever's literally written in that project's {@code project.xml} — so more than one
 * project can run concurrently on this one server process without two projects' plugins
 * fighting over the same port.
 *
 * <p><b>Off unless explicitly enabled</b> ({@code VSM_PORT_POOL_ENABLED}, implied by
 * {@code VSM_PLUGIN_PATH_PREFIX_ENABLED}) — see {@code mPoolEnabled}. On a desktop run the
 * authored ports are correct and must not be touched.
 *
 * <p>Allocation is keyed by an opaque "owner" string (in practice, the project's
 * {@code projectId}) and happens <b>once per owner, ever</b> — not once per Runtime.Start.
 * This is deliberate, not an oversight: {@code charamel-embed}'s transport is constructed
 * lazily on a project's *first* {@code launch()} and is never rebound on a later
 * Stop→Start of the same {@code RunTimeProject} instance (see {@code JettyTransport}).
 * Re-allocating (and rewriting the config) on a later launch would silently do nothing for
 * that plugin while the pool's bookkeeping believes a different port is now in use — so
 * callers must only invoke {@link #release} on genuine project teardown (close/unload the
 * project entirely), never on a plain Runtime.Stop that leaves the project open for a
 * later restart.
 *
 * <p>Any {@link PluginConfig} property whose key ends in "port" (case-insensitive) is
 * treated as a port to override — confirmed empirically to be a safe, general heuristic
 * across every plugin in this repo (htmlgui-ws's {@code html_port}/{@code ws_port}/
 * {@code wss_port}, charamel-embed's {@code port}, ssi's {@code logport}, etc. — the only
 * near-miss, voicetts's {@code portrait_url}, correctly doesn't match since it doesn't
 * *end* with "port").
 */
public class PortPoolManager {

    private static final LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();

    /**
     * Non-port synthetic property PortPoolManager writes onto a PluginConfig alongside its
     * port overrides — never matches the {@code *port} heuristic (Option C, doc/vsm-workspace-
     * platform-plan.md follow-up on Phase 5): tells a plugin's own served page (htmlgui-ws,
     * charamel-embed) the URL path prefix inner-nginx is routing it under, e.g.
     * {@code /plugin/<projectId>/<pluginInstanceName>/}, so its WebSocket client can connect
     * through that same prefix (swapping in {@code ws_port}) instead of a raw port the browser
     * may not even be able to reach directly in this deployment mode. Empty/absent when
     * path-prefix routing isn't in play — plugins must treat that as "use legacy direct-port
     * behavior", not assume nginx is present.
     */
    static final String PATH_PREFIX_PROPERTY = "_pathPrefix";

    /**
     * Companion to {@link #PATH_PREFIX_PROPERTY}, for URLs this manager can't rewrite itself:
     * project-authored content (screens.json's {@code character} key, SceneFlow assignments to
     * {@code character.srcVar}, ...) may contain literal URLs like
     * {@code http://localhost:3040/character.html} using the ORIGINAL project.xml port — the
     * "3040 means CharamelEmbedXenia" correlation only exists here, at allocation time, right
     * before the port property is overwritten. This property carries that correlation to the
     * pages that need it: a JSON object mapping each original literal port to the full
     * inner-nginx path prefix (including the port key) now serving that plugin, e.g.
     * {@code {"3040": "/plugin/<projectId>/CharamelEmbedXenia/port/"}}. htmlgui-ws injects it
     * into its served pages (window.VSM_GUI_CONFIG.portRewrites) so vsm-renderer.js can fix up
     * stale authored URLs at render time (confirmed broken 2026-07-29 without this — see
     * doc/vsm-deployment-next-steps.md section 1). Set on every config that got a port, since
     * one plugin's page (htmlgui-ws) embeds iframes pointing at OTHER plugins' ports.
     */
    static final String PORT_REWRITES_PROPERTY = "_portRewrites";

    private final int mPoolStart;
    private final int mPoolSize;
    private final Path mRegistryFile;
    private final boolean mPathPrefixEnabled;
    /**
     * Whether to override plugin ports at all. OFF by default, because port pooling only makes
     * sense for the multi-user server deployment: on a desktop run (./gradlew run) the authored
     * project.xml ports are exactly right, and overriding them silently breaks any project
     * content that references a plugin's port literally (screens.json's character URL,
     * srcVar values) — the {@code _portRewrites} machinery that repairs those is itself only
     * active behind path-prefix routing, i.e. only on the server. Confirmed 2026-08-11: a local
     * run allocated pool ports, charamel-embed bound one, and the character iframe kept
     * requesting the authored localhost:3040 and failed to load.
     */
    private final boolean mPoolEnabled;

    /** Every port not currently handed out. */
    private final TreeSet<Integer> mFreePorts = new TreeSet<>();
    /** ownerKey -> the ports currently allocated to it (needed to free them later). */
    private final Map<String, List<Integer>> mAllocations = new LinkedHashMap<>();
    /** ownerKey -> {"pluginName.key": port}, for the registry file (Phase 5's nginx reads this). */
    private final Map<String, Map<String, Integer>> mAllocationDetails = new LinkedHashMap<>();
    /** ownerKey -> every (config, key, original, allocated) — see {@link #withOriginalConfig}. */
    private final Map<String, List<AllocationRecord>> mAllocationRecords = new LinkedHashMap<>();

    public PortPoolManager() {
        this(intEnv("VSM_PORT_POOL_START", 20000), intEnv("VSM_PORT_POOL_SIZE", 200), resolveDefaultRegistryFile(),
                boolEnv("VSM_PLUGIN_PATH_PREFIX_ENABLED", false),
                // Path-prefix routing implies pooling (it exists to route the pooled ports), so
                // enabling it is enough — the deployment's compose already sets it and needs no
                // change. VSM_PORT_POOL_ENABLED allows pooling without prefix routing.
                boolEnv("VSM_PORT_POOL_ENABLED", false)
                        || boolEnv("VSM_PLUGIN_PATH_PREFIX_ENABLED", false));
    }

    /** Package-private, exercised directly by tests with a small pool and a temp registry file. */
    PortPoolManager(int poolStart, int poolSize, Path registryFile) {
        this(poolStart, poolSize, registryFile, false, true);
    }

    PortPoolManager(int poolStart, int poolSize, Path registryFile, boolean pathPrefixEnabled) {
        this(poolStart, poolSize, registryFile, pathPrefixEnabled, true);
    }

    PortPoolManager(int poolStart, int poolSize, Path registryFile, boolean pathPrefixEnabled,
                    boolean poolEnabled) {
        mPoolStart = poolStart;
        mPoolSize = poolSize;
        mRegistryFile = registryFile;
        mPathPrefixEnabled = pathPrefixEnabled;
        mPoolEnabled = poolEnabled;
        for (int i = 0; i < poolSize; i++) {
            mFreePorts.add(poolStart + i);
        }
    }

    private static int intEnv(String name, int defaultValue) {
        String raw = System.getenv(name);
        if (raw == null || raw.isBlank()) {
            return defaultValue;
        }
        try {
            return Integer.parseInt(raw.trim());
        } catch (NumberFormatException exc) {
            sLogger.warning("PortPoolManager: invalid " + name + "=" + raw + ", using default " + defaultValue);
            return defaultValue;
        }
    }

    private static boolean boolEnv(String name, boolean defaultValue) {
        String raw = System.getenv(name);
        return (raw == null || raw.isBlank()) ? defaultValue : Boolean.parseBoolean(raw.trim());
    }

    private static Path resolveDefaultRegistryFile() {
        String override = System.getenv("VSM_PORT_REGISTRY_FILE");
        if (override != null && !override.isBlank()) {
            return Paths.get(override);
        }
        return Paths.get(System.getProperty("user.home"), ".vsm.d", "port-registry.json");
    }

    public synchronized boolean isAllocated(String ownerKey) {
        return mAllocations.containsKey(ownerKey);
    }

    /**
     * No-op if {@code ownerKey} already has an allocation (first-launch-only semantics, see
     * class docs). Otherwise scans every {@code *port} property across {@code pluginConfigs},
     * allocates that many free ports, and overwrites each property in place via
     * {@link PluginConfig#setProperty}.
     *
     * @throws PortPoolExhaustedException if fewer ports are free than needed — an explicit
     *         error is surfaced to the caller rather than queueing (Decision 16).
     */
    public synchronized void ensureAllocated(String ownerKey, List<PluginConfig> pluginConfigs) {
        if (!mPoolEnabled) {
            // Desktop/single-user run: leave the authored project.xml ports completely alone
            // (see mPoolEnabled's docs). No allocation is recorded, so withOriginalConfig()
            // correctly becomes a pass-through for saves too.
            return;
        }
        if (isAllocated(ownerKey)) {
            return;
        }
        List<PortRef> portRefs = new ArrayList<>();
        for (PluginConfig pc : pluginConfigs) {
            for (ConfigFeature feature : pc.getEntryList()) {
                String key = feature.getKey();
                if (key != null && key.toLowerCase().endsWith("port")) {
                    portRefs.add(new PortRef(pc, key));
                }
            }
        }
        if (portRefs.isEmpty()) {
            mAllocations.put(ownerKey, List.of());
            mAllocationDetails.put(ownerKey, Map.of());
            return; // nothing to override, but remember we looked so we don't rescan every launch
        }
        if (portRefs.size() > mFreePorts.size()) {
            throw new PortPoolExhaustedException(
                    "Need " + portRefs.size() + " port(s) but only " + mFreePorts.size()
                            + " free (pool: " + mPoolStart + "-" + (mPoolStart + mPoolSize - 1) + ")");
        }
        List<Integer> allocated = new ArrayList<>(portRefs.size());
        Map<String, Integer> details = new LinkedHashMap<>();
        Set<PluginConfig> touchedConfigs = new LinkedHashSet<>();
        List<AllocationRecord> records = new ArrayList<>(portRefs.size());
        JSONObject portRewrites = new JSONObject();
        for (PortRef ref : portRefs) {
            int port = mFreePorts.pollFirst();
            allocated.add(port);
            // The original literal project.xml value, read at the only moment it still exists —
            // recorded (per class docs on PORT_REWRITES_PROPERTY) so pages can fix up
            // project-authored URLs that still reference it, and kept in mAllocationRecords so
            // withOriginalConfig() can restore it around a project save.
            String originalPort = ref.pluginConfig.getProperty(ref.key, "").trim();
            if (!originalPort.isEmpty() && originalPort.chars().allMatch(Character::isDigit)) {
                String prefix = "/plugin/" + ownerKey + "/" + ref.pluginConfig.getPluginName() + "/" + ref.key + "/";
                if (portRewrites.has(originalPort)) {
                    // Two plugins authored with the same literal port — ambiguous by construction;
                    // keep the first and say so rather than silently rewriting to the wrong one.
                    sLogger.warning("PortPoolManager: original port " + originalPort
                            + " appears more than once in " + ownerKey + "'s config — port-rewrite"
                            + " for it stays mapped to the first occurrence, not " + prefix);
                } else {
                    portRewrites.put(originalPort, prefix);
                }
            }
            ref.pluginConfig.setProperty(ref.key, String.valueOf(port));
            details.put(ref.pluginConfig.getPluginName() + "." + ref.key, port);
            touchedConfigs.add(ref.pluginConfig);
            records.add(new AllocationRecord(ref.pluginConfig, ref.key, originalPort, String.valueOf(port)));
        }
        if (mPathPrefixEnabled) {
            for (PluginConfig pc : touchedConfigs) {
                pc.setProperty(PATH_PREFIX_PROPERTY, "/plugin/" + ownerKey + "/" + pc.getPluginName() + "/");
                pc.setProperty(PORT_REWRITES_PROPERTY, portRewrites.toString());
            }
        }
        mAllocations.put(ownerKey, allocated);
        mAllocationDetails.put(ownerKey, details);
        mAllocationRecords.put(ownerKey, records);
        sLogger.message("PortPoolManager: allocated " + allocated + " to " + ownerKey);
        writeRegistry();
    }

    /**
     * Runs {@code action} (typically {@code RunTimeProject.write()}) with {@code ownerKey}'s
     * plugin configs temporarily restored to their ORIGINAL authored state — original port
     * values back in place, this manager's synthetic properties removed — then re-applies the
     * live allocation before returning, success or not.
     *
     * <p>Exists because {@code RunTimeProject.write()} serializes the live in-memory
     * {@code ProjectConfig} — the very object this manager mutates. Without this, saving a
     * launched project persists pool ports and {@code _pathPrefix}/{@code _portRewrites} into
     * {@code project.xml}, destroying the authored ports (confirmed on the real deployment
     * 2026-07-30: a save from the previous day had baked pool ports into the file, so the next
     * epoch's "originals" were pool ports and the port-rewrite map was self-referential
     * garbage). Callers in the save path must wrap every {@code write()} in this.
     *
     * <p>Synchronized (like all mutators here), so a save can't interleave with a concurrent
     * allocation. A plugin launching concurrently could in principle read the briefly-restored
     * originals, but every launch path calls {@code ensurePortsAllocated} first, whose
     * once-per-owner guard makes it a no-op here — the live values are re-applied before this
     * returns, and plugins only read their config during {@code launch()}.
     */
    public synchronized <T> T withOriginalConfig(String ownerKey, java.util.function.Supplier<T> action) {
        List<AllocationRecord> records = mAllocationRecords.get(ownerKey);
        if (records == null || records.isEmpty()) {
            return action.get();
        }
        Set<PluginConfig> touchedConfigs = new LinkedHashSet<>();
        Map<PluginConfig, String[]> synthetic = new LinkedHashMap<>();
        for (AllocationRecord rec : records) {
            if (touchedConfigs.add(rec.pluginConfig)) {
                synthetic.put(rec.pluginConfig, new String[]{
                        rec.pluginConfig.getProperty(PATH_PREFIX_PROPERTY),
                        rec.pluginConfig.getProperty(PORT_REWRITES_PROPERTY)});
            }
        }
        try {
            for (AllocationRecord rec : records) {
                rec.pluginConfig.setProperty(rec.key, rec.originalValue);
            }
            for (PluginConfig pc : touchedConfigs) {
                pc.getEntryList().removeIf(f -> PATH_PREFIX_PROPERTY.equals(f.getKey())
                        || PORT_REWRITES_PROPERTY.equals(f.getKey()));
            }
            return action.get();
        } finally {
            for (AllocationRecord rec : records) {
                rec.pluginConfig.setProperty(rec.key, rec.allocatedValue);
            }
            for (Map.Entry<PluginConfig, String[]> e : synthetic.entrySet()) {
                if (e.getValue()[0] != null) {
                    e.getKey().setProperty(PATH_PREFIX_PROPERTY, e.getValue()[0]);
                }
                if (e.getValue()[1] != null) {
                    e.getKey().setProperty(PORT_REWRITES_PROPERTY, e.getValue()[1]);
                }
            }
        }
    }

    /** No-op if {@code ownerKey} has no allocation. Only call on genuine project teardown — see class docs. */
    public synchronized void release(String ownerKey) {
        List<Integer> ports = mAllocations.remove(ownerKey);
        mAllocationDetails.remove(ownerKey);
        mAllocationRecords.remove(ownerKey);
        if (ports == null || ports.isEmpty()) {
            return;
        }
        mFreePorts.addAll(ports);
        sLogger.message("PortPoolManager: released " + ports + " from " + ownerKey);
        writeRegistry();
    }

    public synchronized int freeCount() {
        return mFreePorts.size();
    }

    private void writeRegistry() {
        JSONObject root = new JSONObject();
        for (Map.Entry<String, Map<String, Integer>> e : mAllocationDetails.entrySet()) {
            root.put(e.getKey(), new JSONObject(e.getValue()));
        }
        try {
            Path parent = mRegistryFile.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            Path tmp = mRegistryFile.resolveSibling(mRegistryFile.getFileName() + ".tmp");
            Files.writeString(tmp, root.toString(2), StandardCharsets.UTF_8);
            Files.move(tmp, mRegistryFile, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
        } catch (IOException exc) {
            // The registry is Phase 5's nginx's problem, not launching the project's — don't
            // fail the caller (which already has real, overridden ports in memory) over this.
            sLogger.warning("PortPoolManager: failed to write registry " + mRegistryFile + ": " + exc.getMessage());
        }
    }

    private static final class PortRef {
        final PluginConfig pluginConfig;
        final String key;

        PortRef(PluginConfig pluginConfig, String key) {
            this.pluginConfig = pluginConfig;
            this.key = key;
        }
    }

    private static final class AllocationRecord {
        final PluginConfig pluginConfig;
        final String key;
        final String originalValue;
        final String allocatedValue;

        AllocationRecord(PluginConfig pluginConfig, String key, String originalValue, String allocatedValue) {
            this.pluginConfig = pluginConfig;
            this.key = key;
            this.originalValue = originalValue;
            this.allocatedValue = allocatedValue;
        }
    }

    public static final class PortPoolExhaustedException extends RuntimeException {
        public PortPoolExhaustedException(String message) {
            super(message);
        }
    }
}
