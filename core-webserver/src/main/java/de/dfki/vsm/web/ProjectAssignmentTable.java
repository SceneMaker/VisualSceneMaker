package de.dfki.vsm.web;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
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
