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
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

/**
 * Phase 4 of doc/vsm-workspace-platform-plan.md: overrides a project's plugin port
 * configuration with dynamically-allocated ports from a shared pool, instead of trusting
 * whatever's literally written in that project's {@code project.xml} — so more than one
 * project can run concurrently on this one server process without two projects' plugins
 * fighting over the same port.
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

    private final int mPoolStart;
    private final int mPoolSize;
    private final Path mRegistryFile;

    /** Every port not currently handed out. */
    private final TreeSet<Integer> mFreePorts = new TreeSet<>();
    /** ownerKey -> the ports currently allocated to it (needed to free them later). */
    private final Map<String, List<Integer>> mAllocations = new LinkedHashMap<>();
    /** ownerKey -> {"pluginName.key": port}, for the registry file (Phase 5's nginx reads this). */
    private final Map<String, Map<String, Integer>> mAllocationDetails = new LinkedHashMap<>();

    public PortPoolManager() {
        this(intEnv("VSM_PORT_POOL_START", 20000), intEnv("VSM_PORT_POOL_SIZE", 200), resolveDefaultRegistryFile());
    }

    /** Package-private, exercised directly by tests with a small pool and a temp registry file. */
    PortPoolManager(int poolStart, int poolSize, Path registryFile) {
        mPoolStart = poolStart;
        mPoolSize = poolSize;
        mRegistryFile = registryFile;
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
        for (PortRef ref : portRefs) {
            int port = mFreePorts.pollFirst();
            allocated.add(port);
            ref.pluginConfig.setProperty(ref.key, String.valueOf(port));
            details.put(ref.pluginConfig.getPluginName() + "." + ref.key, port);
        }
        mAllocations.put(ownerKey, allocated);
        mAllocationDetails.put(ownerKey, details);
        sLogger.message("PortPoolManager: allocated " + allocated + " to " + ownerKey);
        writeRegistry();
    }

    /** No-op if {@code ownerKey} has no allocation. Only call on genuine project teardown — see class docs. */
    public synchronized void release(String ownerKey) {
        List<Integer> ports = mAllocations.remove(ownerKey);
        mAllocationDetails.remove(ownerKey);
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

    public static final class PortPoolExhaustedException extends RuntimeException {
        public PortPoolExhaustedException(String message) {
            super(message);
        }
    }
}
