package de.dfki.vsm.web;

import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Token-based identity and role registry that sits in front of all WebSocket
 * and REST handlers.
 *
 * <p>Callers resolve an incoming bearer token to a {@link UserToken} and then
 * check the required role before executing a command.  Example pattern inside
 * a Javalin handler:</p>
 *
 * <pre>{@code
 * UserToken user = sessionGate.resolve(bearerToken);
 * if (user == null)            { ctx.status(401); return; }
 * if (!user.hasRole(ROLE_EDITOR)) { ctx.status(403); return; }
 * }</pre>
 *
 * <h2>Token lifecycle</h2>
 * <ol>
 *   <li>Admin calls {@link #provision} or {@link #provisionWithToken} to
 *       create a token for a user.</li>
 *   <li>Client includes the token in requests;
 *       {@link #resolve} looks it up.</li>
 *   <li>Admin calls {@link #revoke} or {@link #revokeAll} to invalidate.</li>
 * </ol>
 *
 * <h2>Legacy compatibility</h2>
 * <p>{@link #provisionWithToken} accepts the exact token string passed via the
 * {@code --token} CLI flag, preserving single-shared-token compatibility while
 * the named-user model is adopted gradually.</p>
 *
 * <h2>Thread safety</h2>
 * <p>All operations are safe for concurrent use.  Token resolution and
 * provisioning are backed by {@link ConcurrentHashMap} with atomic
 * {@code putIfAbsent}.  {@link #revokeAll} iterates a snapshot and uses
 * conditional remove, so it is safe against concurrent modifications.</p>
 *
 * <p>Corresponds to Component 7 of
 * {@code doc/collaborative-multisession-plan.md}.</p>
 */
public final class SessionGate {

    // ------------------------------------------------------------------
    // Standard role constants
    // ------------------------------------------------------------------

    /** May send editing commands (node/edge mutations, script edits). */
    public static final String ROLE_EDITOR = "editor";

    /** Receives broadcast updates but editing commands are rejected. */
    public static final String ROLE_VIEWER = "viewer";

    /** May start, stop, pause, and resume runtime execution. */
    public static final String ROLE_RUNTIME_ADMIN = "runtime-admin";

    // ------------------------------------------------------------------
    // State
    // ------------------------------------------------------------------

    /** Number of random bytes per generated token — 256-bit entropy. */
    private static final int TOKEN_BYTES = 32;

    private static final SecureRandom SECURE_RANDOM = new SecureRandom();

    /** Maps bearer token string → {@link UserToken}. */
    private final ConcurrentHashMap<String, UserToken> tokenMap = new ConcurrentHashMap<>();

    // ------------------------------------------------------------------
    // Provision
    // ------------------------------------------------------------------

    /**
     * Generates a fresh cryptographically random token for a user and
     * registers it in the gate.
     *
     * <p>The token is a 43-character URL-safe Base64 string (256-bit
     * entropy, no padding characters) suitable for use in HTTP headers and
     * query parameters without escaping.</p>
     *
     * @param userId      stable user identifier; must be non-blank
     * @param displayName human-readable name; derived from {@code userId}
     *                    when blank or {@code null}
     * @param roles       set of roles granted to the user; must not be null
     * @return the newly provisioned {@link UserToken} (contains the token
     *         string in {@link UserToken#token})
     * @throws IllegalArgumentException if {@code userId} is blank
     * @throws NullPointerException     if {@code roles} is null
     */
    public UserToken provision(String userId, String displayName, Set<String> roles) {
        requireNonBlank(userId, "userId");
        Objects.requireNonNull(roles, "roles must not be null");
        String token = generateToken();
        UserToken ut = new UserToken(token, userId, displayName, roles,
                System.currentTimeMillis());
        tokenMap.put(token, ut);
        return ut;
    }

    /**
     * Registers a caller-supplied token string rather than generating one.
     * Intended for the {@code --token} CLI flag and deterministic test setups.
     *
     * @param token       the exact bearer token to register; must be non-blank
     * @param userId      stable user identifier; must be non-blank
     * @param displayName human-readable name; may be null
     * @param roles       set of roles; must not be null
     * @return the provisioned {@link UserToken}
     * @throws IllegalArgumentException if {@code token} or {@code userId} is blank
     * @throws NullPointerException     if {@code roles} is null
     * @throws IllegalStateException    if {@code token} is already registered
     */
    public UserToken provisionWithToken(String token, String userId,
                                        String displayName, Set<String> roles) {
        requireNonBlank(token, "token");
        requireNonBlank(userId, "userId");
        Objects.requireNonNull(roles, "roles must not be null");
        UserToken ut = new UserToken(token, userId, displayName, roles,
                System.currentTimeMillis());
        UserToken existing = tokenMap.putIfAbsent(token, ut);
        if (existing != null) {
            throw new IllegalStateException("Token is already registered");
        }
        return ut;
    }

    // ------------------------------------------------------------------
    // Resolve
    // ------------------------------------------------------------------

    /**
     * Resolves a bearer token string to its {@link UserToken}.
     *
     * @param token the bearer token from an {@code Authorization} header or
     *              {@code ?token=} query parameter
     * @return the matching {@link UserToken}, or {@code null} if the token is
     *         unknown, blank, or {@code null}
     */
    public UserToken resolve(String token) {
        if (token == null || token.isBlank()) return null;
        return tokenMap.get(token);
    }

    // ------------------------------------------------------------------
    // Revoke
    // ------------------------------------------------------------------

    /**
     * Revokes a single token, making it immediately invalid for all
     * subsequent requests.
     *
     * @param token the token string to revoke; {@code null} is accepted and
     *              returns {@code false}
     * @return {@code true} if the token existed and was removed
     */
    public boolean revoke(String token) {
        if (token == null) return false;
        return tokenMap.remove(token) != null;
    }

    /**
     * Revokes all tokens belonging to {@code userId}.  Useful when a user's
     * access is terminated or their credentials are rotated.
     *
     * @param userId the user whose tokens should be invalidated; blank values
     *               are ignored and return {@code 0}
     * @return the number of tokens that were removed
     */
    public int revokeAll(String userId) {
        if (userId == null || userId.isBlank()) return 0;
        int count = 0;
        for (Map.Entry<String, UserToken> entry : tokenMap.entrySet()) {
            if (userId.equals(entry.getValue().userId)) {
                if (tokenMap.remove(entry.getKey(), entry.getValue())) {
                    count++;
                }
            }
        }
        return count;
    }

    // ------------------------------------------------------------------
    // Role check
    // ------------------------------------------------------------------

    /**
     * Shorthand for {@code resolve(token).hasRole(role)}: resolves the bearer
     * token and checks whether it grants {@code role} in a single call.
     *
     * @param token bearer token string; {@code null} returns {@code false}
     * @param role  role to test; {@code null} returns {@code false}
     * @return {@code true} only if the token is valid and grants the role
     */
    public boolean hasRole(String token, String role) {
        UserToken ut = resolve(token);
        return ut != null && ut.hasRole(role);
    }

    // ------------------------------------------------------------------
    // Query
    // ------------------------------------------------------------------

    /**
     * Returns an unmodifiable snapshot of all currently provisioned tokens.
     * The list may be used to populate an admin console or audit log.
     * Note: {@link UserToken#toJson()} omits the bearer token string; use
     * {@link UserToken#toJsonWithToken()} only for direct provisioning
     * responses.
     */
    public List<UserToken> listTokens() {
        return Collections.unmodifiableList(new ArrayList<>(tokenMap.values()));
    }

    /** Number of currently active (provisioned, not revoked) tokens. */
    public int size() {
        return tokenMap.size();
    }

    /** Removes all provisioned tokens. Intended for testing and server shutdown. */
    public void clear() {
        tokenMap.clear();
    }

    // ------------------------------------------------------------------
    // Internal helpers
    // ------------------------------------------------------------------

    /**
     * Generates a 43-character URL-safe Base64 token (256 random bits,
     * no padding).
     */
    private static String generateToken() {
        byte[] bytes = new byte[TOKEN_BYTES];
        SECURE_RANDOM.nextBytes(bytes);
        return Base64.getUrlEncoder().withoutPadding().encodeToString(bytes);
    }

    private static void requireNonBlank(String value, String name) {
        if (value == null || value.isBlank()) {
            throw new IllegalArgumentException(name + " must not be blank");
        }
    }
}
