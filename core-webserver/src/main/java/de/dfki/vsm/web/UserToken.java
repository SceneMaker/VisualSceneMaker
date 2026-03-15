package de.dfki.vsm.web;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * Immutable named-user credential for authentication and role-based access
 * control in {@link SessionGate}.
 *
 * <p>A {@code UserToken} is created exclusively by {@link SessionGate} and
 * represents one active bearer credential bound to a user identity.  The
 * bearer token string ({@link #token}) is the secret that clients include in
 * {@code Authorization: Bearer <token>} headers or {@code ?token=} query
 * parameters.  All other fields are non-secret metadata.</p>
 *
 * <h2>Role model</h2>
 * <p>Standard role strings are defined as constants on {@link SessionGate}:
 * {@link SessionGate#ROLE_EDITOR}, {@link SessionGate#ROLE_VIEWER}, and
 * {@link SessionGate#ROLE_RUNTIME_ADMIN}.  Custom roles are supported — the
 * gate does not restrict which strings appear in the set.</p>
 *
 * <p>Corresponds to Component 7 of
 * {@code doc/collaborative-multisession-plan.md}.</p>
 */
public final class UserToken {

    /** Opaque, URL-safe bearer token string; the secret held by the client. */
    public final String token;

    /** Stable user identifier (e.g. login name, UUID, or email). */
    public final String userId;

    /** Human-readable name shown in presence indicators and audit logs. */
    public final String displayName;

    /**
     * Unmodifiable set of roles granted to this user.  Standard roles:
     * {@link SessionGate#ROLE_EDITOR}, {@link SessionGate#ROLE_VIEWER},
     * {@link SessionGate#ROLE_RUNTIME_ADMIN}.
     */
    public final Set<String> roles;

    /** Wall-clock milliseconds when this token was provisioned. */
    public final long createdAt;

    UserToken(String token, String userId, String displayName,
              Set<String> roles, long createdAt) {
        this.token = token;
        this.userId = userId;
        this.displayName = (displayName != null && !displayName.isBlank())
                ? displayName : userId;
        this.roles = Collections.unmodifiableSet(new HashSet<>(roles));
        this.createdAt = createdAt;
    }

    // ------------------------------------------------------------------
    // Role check
    // ------------------------------------------------------------------

    /**
     * Returns {@code true} if this token grants {@code role}.
     * Comparison is case-sensitive.
     *
     * @param role role string to test; {@code null} always returns {@code false}
     */
    public boolean hasRole(String role) {
        return role != null && roles.contains(role);
    }

    // ------------------------------------------------------------------
    // Serialisation
    // ------------------------------------------------------------------

    /**
     * Serialises this token to JSON, <em>omitting the bearer token string</em>.
     * Safe for audit logs, list responses, and WebSocket broadcasts where
     * the secret must not be re-transmitted.
     *
     * <p>Fields: {@code userId}, {@code displayName}, {@code roles},
     * {@code createdAt}.</p>
     */
    public JSONObject toJson() {
        return buildJson(false);
    }

    /**
     * Serialises this token to JSON, <em>including the bearer token string</em>.
     * Use only for provisioning responses sent directly to the token owner.
     *
     * <p>Fields: {@code token}, {@code userId}, {@code displayName},
     * {@code roles}, {@code createdAt}.</p>
     */
    public JSONObject toJsonWithToken() {
        return buildJson(true);
    }

    private JSONObject buildJson(boolean includeToken) {
        JSONObject obj = new JSONObject();
        if (includeToken) {
            obj.put("token", token);
        }
        obj.put("userId", userId);
        obj.put("displayName", displayName);
        obj.put("createdAt", createdAt);
        JSONArray rolesArr = new JSONArray();
        for (String r : roles) {
            rolesArr.put(r);
        }
        obj.put("roles", rolesArr);
        return obj;
    }
}
