package de.dfki.vsm.web;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Restricts which projects a non-admin user may reach — Phase 2 of
 * doc/vsm-workspace-platform-plan.md (Decision 5: admin-assigned projects,
 * no self-serve creation; Decision 6: an in-app admin role; Decision 14: a
 * flat file, no database).
 *
 * <p>File format ({@code VSM_ASSIGNMENTS_FILE}, default
 * {@code ~/.vsm.d/project-assignments.json}):
 * <pre>{@code
 * {
 *   "users": {
 *     "alice": { "admin": false, "projects": ["/app/project/Confidence - SIA Layer"] },
 *     "bob":   { "admin": true,  "projects": [] }
 *   }
 * }
 * }</pre>
 *
 * <p>Re-read from disk on every check rather than cached — there is no admin UI yet
 * (that's Phase 3), so a human is expected to hand-edit this file, and edits should
 * take effect without a server restart.
 *
 * <p>Two distinct failure modes, deliberately different:
 * <ul>
 *   <li><b>File genuinely absent</b> — fails <em>open</em> (unrestricted, matching
 *       pre-Phase-2 behavior). This is the "Phase 2 shipped, nobody has created the
 *       assignment file yet" bootstrap case — turning Phase 2 on does not retroactively
 *       restrict anything until the file actually exists. A warning is logged once.</li>
 *   <li><b>File exists but is unreadable or malformed</b> — fails <em>closed</em> (nobody,
 *       not even admins, gets through). A hand-edited file that exists is a signal of
 *       deliberate intent to restrict access; silently falling back to unrestricted on a
 *       typo would be a much worse failure than a loud lockout that gets noticed and fixed.</li>
 * </ul>
 */
public class ProjectAssignmentTable {

    private static final LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();

    private final Path mFile;
    private final AtomicBoolean mLoggedMissing = new AtomicBoolean(false);

    public ProjectAssignmentTable() {
        this(resolveDefaultFile());
    }

    /** Package-private, exercised directly by tests against a temp file. */
    ProjectAssignmentTable(Path file) {
        mFile = file;
    }

    private static Path resolveDefaultFile() {
        String override = System.getenv("VSM_ASSIGNMENTS_FILE");
        if (override != null && !override.isBlank()) {
            return Paths.get(override);
        }
        return Paths.get(System.getProperty("user.home"), ".vsm.d", "project-assignments.json");
    }

    /** True if {@code userId} may reach {@code projectPath} — admins can reach everything. */
    public boolean canAccess(String userId, String projectPath) {
        if (userId == null || projectPath == null) {
            return false;
        }
        Map<String, UserEntry> users = load();
        if (users == null) {
            return true; // file absent — unrestricted (bootstrap case)
        }
        UserEntry entry = users.get(userId);
        return entry != null && (entry.admin || entry.projects.contains(projectPath.trim()));
    }

    /**
     * The explicit project paths assigned to {@code userId}, for the landing page's "Your
     * projects" list. Only the user's own {@code projects} entries — NOT a synthesis of
     * "everything an admin can reach" (an admin's list is whatever was explicitly assigned to
     * them, which may be empty even though {@link #canAccess} lets them open anything). Empty
     * set when the file is absent/unreadable or the user isn't listed — the landing page just
     * shows nothing extra, which is the right fail-safe.
     */
    public Set<String> projectsFor(String userId) {
        if (userId == null) {
            return new HashSet<>();
        }
        Map<String, UserEntry> users = load();
        if (users == null) {
            return new HashSet<>();
        }
        UserEntry entry = users.get(userId);
        return entry == null ? new HashSet<>() : new HashSet<>(entry.projects);
    }

    public boolean isAdmin(String userId) {
        if (userId == null) {
            return false;
        }
        Map<String, UserEntry> users = load();
        if (users == null) {
            return false; // "unrestricted" means reachable, not admin
        }
        UserEntry entry = users.get(userId);
        return entry != null && entry.admin;
    }

    /**
     * All known users, for the Phase 3 admin panel. Never {@code null} — an absent/unreadable
     * file just means an empty {@code "users"} object, since listing is how an admin discovers
     * there's nothing configured yet (as opposed to {@link #canAccess}/{@link #isAdmin}, where
     * absent vs. broken must be distinguishable to pick fail-open vs. fail-closed).
     */
    public JSONObject listUsersAsJson() {
        Map<String, UserEntry> users = load();
        JSONObject root = new JSONObject();
        root.put("users", usersToJson(users != null ? users : new HashMap<>()));
        return root;
    }

    /** Inserts or replaces {@code userId}'s entry, then writes the file (atomically). */
    public synchronized void setUser(String userId, boolean admin, Set<String> projects) {
        Map<String, UserEntry> users = load();
        if (users == null) {
            users = new HashMap<>();
        }
        Set<String> trimmed = new HashSet<>();
        for (String p : projects) {
            if (p != null && !p.isBlank()) {
                trimmed.add(p.trim());
            }
        }
        users.put(userId, new UserEntry(admin, trimmed));
        writeAll(users);
    }

    /** No-op if {@code userId} isn't present, or the file doesn't exist yet. */
    public synchronized void removeUser(String userId) {
        Map<String, UserEntry> users = load();
        if (users == null || !users.containsKey(userId)) {
            return;
        }
        users.remove(userId);
        writeAll(users);
    }

    private JSONObject usersToJson(Map<String, UserEntry> users) {
        JSONObject usersJson = new JSONObject();
        for (Map.Entry<String, UserEntry> e : users.entrySet()) {
            JSONObject u = new JSONObject();
            u.put("admin", e.getValue().admin);
            u.put("projects", new JSONArray(e.getValue().projects));
            usersJson.put(e.getKey(), u);
        }
        return usersJson;
    }

    /** Write-temp-then-atomic-move so a concurrent read never sees a half-written file. */
    private void writeAll(Map<String, UserEntry> users) {
        JSONObject root = new JSONObject();
        root.put("users", usersToJson(users));
        try {
            Path parent = mFile.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            Path tmp = mFile.resolveSibling(mFile.getFileName() + ".tmp");
            Files.writeString(tmp, root.toString(2), StandardCharsets.UTF_8);
            Files.move(tmp, mFile, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
            mLoggedMissing.set(false); // file now exists — re-arm the "missing" warning if it's ever deleted again
        } catch (IOException exc) {
            throw new UncheckedIOException("Failed to write " + mFile, exc);
        }
    }

    /** {@code null} means "not configured — fail open"; a map (possibly empty) means "fail closed for anyone not in it". */
    private Map<String, UserEntry> load() {
        if (!Files.isReadable(mFile)) {
            if (mLoggedMissing.compareAndSet(false, true)) {
                sLogger.warning("ProjectAssignmentTable: " + mFile
                        + " not found — access is UNRESTRICTED until it's created.");
            }
            return null;
        }
        try {
            String raw = Files.readString(mFile, StandardCharsets.UTF_8);
            JSONObject root = new JSONObject(raw);
            JSONObject users = root.optJSONObject("users");
            Map<String, UserEntry> result = new HashMap<>();
            if (users == null) {
                return result;
            }
            for (String userId : users.keySet()) {
                JSONObject u = users.optJSONObject(userId);
                if (u == null) {
                    continue;
                }
                boolean admin = u.optBoolean("admin", false);
                Set<String> projects = new HashSet<>();
                JSONArray arr = u.optJSONArray("projects");
                if (arr != null) {
                    for (int i = 0; i < arr.length(); i++) {
                        String p = arr.optString(i, null);
                        if (p != null && !p.isBlank()) {
                            projects.add(p.trim());
                        }
                    }
                }
                result.put(userId, new UserEntry(admin, projects));
            }
            return result;
        } catch (IOException | RuntimeException exc) {
            // File exists but is broken (bad permissions, malformed JSON, ...) — fail closed
            // rather than silently reverting to unrestricted on what's likely a typo.
            sLogger.warning("ProjectAssignmentTable: " + mFile + " exists but failed to parse ("
                    + exc.getMessage() + ") — access is DENIED for everyone until it's fixed.");
            return new HashMap<>();
        }
    }

    private static final class UserEntry {
        final boolean admin;
        final Set<String> projects;

        UserEntry(boolean admin, Set<String> projects) {
            this.admin = admin;
            this.projects = projects;
        }
    }
}
