package de.dfki.vsm.web;

import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Functional tests for {@link SessionGate} and {@link UserToken}.
 */
class SessionGateTest {

    private SessionGate gate;

    @BeforeEach
    void setUp() {
        gate = new SessionGate();
    }

    // =====================================================================
    // UserToken — construction and accessors
    // =====================================================================

    @Test
    void userTokenPreservesFields() {
        UserToken ut = gate.provision("alice", "Alice Wonderland",
                Set.of(SessionGate.ROLE_EDITOR));
        assertEquals("alice", ut.userId);
        assertEquals("Alice Wonderland", ut.displayName);
        assertTrue(ut.roles.contains(SessionGate.ROLE_EDITOR));
        assertTrue(ut.createdAt > 0);
        assertNotNull(ut.token);
        assertFalse(ut.token.isBlank());
    }

    @Test
    void userTokenFallsBackDisplayNameToUserId() {
        UserToken ut = gate.provision("alice", null, Set.of(SessionGate.ROLE_VIEWER));
        assertEquals("alice", ut.displayName);
    }

    @Test
    void userTokenFallsBackBlankDisplayName() {
        UserToken ut = gate.provision("alice", "   ", Set.of(SessionGate.ROLE_VIEWER));
        assertEquals("alice", ut.displayName);
    }

    @Test
    void userTokenRolesAreUnmodifiable() {
        UserToken ut = gate.provision("alice", null, Set.of(SessionGate.ROLE_EDITOR));
        assertThrows(UnsupportedOperationException.class,
                () -> ut.roles.add("admin"));
    }

    @Test
    void userTokenHasRoleReturnsTrueForGrantedRole() {
        UserToken ut = gate.provision("alice", null,
                Set.of(SessionGate.ROLE_EDITOR, SessionGate.ROLE_VIEWER));
        assertTrue(ut.hasRole(SessionGate.ROLE_EDITOR));
        assertTrue(ut.hasRole(SessionGate.ROLE_VIEWER));
    }

    @Test
    void userTokenHasRoleReturnsFalseForMissingRole() {
        UserToken ut = gate.provision("alice", null, Set.of(SessionGate.ROLE_VIEWER));
        assertFalse(ut.hasRole(SessionGate.ROLE_EDITOR));
        assertFalse(ut.hasRole(SessionGate.ROLE_RUNTIME_ADMIN));
    }

    @Test
    void userTokenHasRoleReturnsFalseForNull() {
        UserToken ut = gate.provision("alice", null, Set.of(SessionGate.ROLE_EDITOR));
        assertFalse(ut.hasRole(null));
    }

    @Test
    void userTokenSupportsMultipleRoles() {
        Set<String> roles = new HashSet<>();
        roles.add(SessionGate.ROLE_EDITOR);
        roles.add(SessionGate.ROLE_VIEWER);
        roles.add(SessionGate.ROLE_RUNTIME_ADMIN);
        UserToken ut = gate.provision("admin", null, roles);
        assertTrue(ut.hasRole(SessionGate.ROLE_EDITOR));
        assertTrue(ut.hasRole(SessionGate.ROLE_VIEWER));
        assertTrue(ut.hasRole(SessionGate.ROLE_RUNTIME_ADMIN));
    }

    // =====================================================================
    // UserToken — toJson / toJsonWithToken
    // =====================================================================

    @Test
    void toJsonOmitsToken() {
        UserToken ut = gate.provision("alice", "Alice", Set.of(SessionGate.ROLE_EDITOR));
        JSONObject json = ut.toJson();
        assertFalse(json.has("token"), "toJson must not expose the bearer token");
    }

    @Test
    void toJsonContainsMandatoryFields() {
        UserToken ut = gate.provision("alice", "Alice", Set.of(SessionGate.ROLE_EDITOR));
        JSONObject json = ut.toJson();
        assertEquals("alice", json.getString("userId"));
        assertEquals("Alice", json.getString("displayName"));
        assertTrue(json.has("createdAt"));
        assertTrue(json.has("roles"));
    }

    @Test
    void toJsonRolesArrayContainsGrantedRoles() {
        UserToken ut = gate.provision("alice", null,
                Set.of(SessionGate.ROLE_EDITOR, SessionGate.ROLE_VIEWER));
        List<Object> rolesInJson = ut.toJson().getJSONArray("roles").toList();
        assertTrue(rolesInJson.contains(SessionGate.ROLE_EDITOR));
        assertTrue(rolesInJson.contains(SessionGate.ROLE_VIEWER));
    }

    @Test
    void toJsonWithTokenIncludesToken() {
        UserToken ut = gate.provision("alice", null, Set.of(SessionGate.ROLE_EDITOR));
        JSONObject json = ut.toJsonWithToken();
        assertTrue(json.has("token"));
        assertEquals(ut.token, json.getString("token"));
    }

    @Test
    void toJsonWithTokenAlsoContainsUserFields() {
        UserToken ut = gate.provision("alice", "Alice", Set.of(SessionGate.ROLE_EDITOR));
        JSONObject json = ut.toJsonWithToken();
        assertEquals("alice", json.getString("userId"));
        assertEquals("Alice", json.getString("displayName"));
    }

    // =====================================================================
    // SessionGate — provision
    // =====================================================================

    @Test
    void provisionGeneratesNonBlankToken() {
        UserToken ut = gate.provision("alice", null, Set.of(SessionGate.ROLE_VIEWER));
        assertNotNull(ut.token);
        assertFalse(ut.token.isBlank());
    }

    @Test
    void provisionTokenIsUrlSafe() {
        // URL-safe Base64 uses A-Z a-z 0-9 - _  (no +, /, =)
        UserToken ut = gate.provision("alice", null, Set.of(SessionGate.ROLE_VIEWER));
        assertTrue(ut.token.matches("[A-Za-z0-9_\\-]+"),
                "Token must contain only URL-safe characters; got: " + ut.token);
    }

    @Test
    void provisionTokenHas43Characters() {
        // 32 bytes → 43 URL-safe Base64 chars (no padding)
        UserToken ut = gate.provision("alice", null, Set.of(SessionGate.ROLE_VIEWER));
        assertEquals(43, ut.token.length());
    }

    @Test
    void provisionTwoCallsProduceDifferentTokens() {
        UserToken a = gate.provision("alice", null, Set.of(SessionGate.ROLE_VIEWER));
        UserToken b = gate.provision("bob", null, Set.of(SessionGate.ROLE_VIEWER));
        assertNotEquals(a.token, b.token);
    }

    @Test
    void provisionSameUserTwiceProducesDifferentTokens() {
        UserToken first = gate.provision("alice", null, Set.of(SessionGate.ROLE_VIEWER));
        UserToken second = gate.provision("alice", null, Set.of(SessionGate.ROLE_VIEWER));
        assertNotEquals(first.token, second.token);
    }

    @Test
    void provisionRefusesBlankUserId() {
        assertThrows(IllegalArgumentException.class,
                () -> gate.provision("", null, Set.of(SessionGate.ROLE_VIEWER)));
        assertThrows(IllegalArgumentException.class,
                () -> gate.provision("   ", null, Set.of(SessionGate.ROLE_VIEWER)));
        assertThrows(IllegalArgumentException.class,
                () -> gate.provision(null, null, Set.of(SessionGate.ROLE_VIEWER)));
    }

    @Test
    void provisionRefusesNullRoles() {
        assertThrows(NullPointerException.class,
                () -> gate.provision("alice", null, null));
    }

    @Test
    void provisionWithEmptyRolesIsAllowed() {
        UserToken ut = gate.provision("alice", null, Collections.emptySet());
        assertNotNull(ut);
        assertTrue(ut.roles.isEmpty());
    }

    // =====================================================================
    // SessionGate — provisionWithToken
    // =====================================================================

    @Test
    void provisionWithTokenStoresExactToken() {
        UserToken ut = gate.provisionWithToken("my-secret-token", "alice", null,
                Set.of(SessionGate.ROLE_EDITOR));
        assertEquals("my-secret-token", ut.token);
    }

    @Test
    void provisionWithTokenIsResolvable() {
        gate.provisionWithToken("my-secret-token", "alice", null,
                Set.of(SessionGate.ROLE_EDITOR));
        UserToken resolved = gate.resolve("my-secret-token");
        assertNotNull(resolved);
        assertEquals("alice", resolved.userId);
    }

    @Test
    void provisionWithTokenLegacyCliFlag() {
        // Simulates: java -jar vsm.jar --token=hardcoded-shared-secret
        gate.provisionWithToken("hardcoded-shared-secret", "admin", "Admin",
                Set.of(SessionGate.ROLE_EDITOR, SessionGate.ROLE_RUNTIME_ADMIN));
        assertTrue(gate.hasRole("hardcoded-shared-secret", SessionGate.ROLE_EDITOR));
        assertTrue(gate.hasRole("hardcoded-shared-secret", SessionGate.ROLE_RUNTIME_ADMIN));
    }

    @Test
    void provisionWithTokenRejectsBlankToken() {
        assertThrows(IllegalArgumentException.class,
                () -> gate.provisionWithToken("", "alice", null,
                        Set.of(SessionGate.ROLE_VIEWER)));
        assertThrows(IllegalArgumentException.class,
                () -> gate.provisionWithToken(null, "alice", null,
                        Set.of(SessionGate.ROLE_VIEWER)));
    }

    @Test
    void provisionWithTokenRejectsBlankUserId() {
        assertThrows(IllegalArgumentException.class,
                () -> gate.provisionWithToken("tok", "", null,
                        Set.of(SessionGate.ROLE_VIEWER)));
    }

    @Test
    void provisionWithTokenThrowsOnDuplicateToken() {
        gate.provisionWithToken("dup-token", "alice", null, Set.of(SessionGate.ROLE_VIEWER));
        assertThrows(IllegalStateException.class,
                () -> gate.provisionWithToken("dup-token", "bob", null,
                        Set.of(SessionGate.ROLE_VIEWER)));
    }

    // =====================================================================
    // SessionGate — resolve
    // =====================================================================

    @Test
    void resolveReturnsTokenAfterProvision() {
        UserToken ut = gate.provision("alice", null, Set.of(SessionGate.ROLE_EDITOR));
        UserToken resolved = gate.resolve(ut.token);
        assertNotNull(resolved);
        assertSame(ut, resolved);
    }

    @Test
    void resolveReturnsNullForUnknownToken() {
        assertNull(gate.resolve("totally-unknown-token"));
    }

    @Test
    void resolveReturnsNullForNull() {
        assertNull(gate.resolve(null));
    }

    @Test
    void resolveReturnsNullForBlank() {
        assertNull(gate.resolve(""));
        assertNull(gate.resolve("   "));
    }

    @Test
    void resolveAfterRevokeReturnsNull() {
        UserToken ut = gate.provision("alice", null, Set.of(SessionGate.ROLE_VIEWER));
        gate.revoke(ut.token);
        assertNull(gate.resolve(ut.token));
    }

    // =====================================================================
    // SessionGate — revoke
    // =====================================================================

    @Test
    void revokeReturnsTrueWhenTokenExists() {
        UserToken ut = gate.provision("alice", null, Set.of(SessionGate.ROLE_VIEWER));
        assertTrue(gate.revoke(ut.token));
    }

    @Test
    void revokeReturnsFalseForUnknownToken() {
        assertFalse(gate.revoke("not-registered"));
    }

    @Test
    void revokeReturnsFalseForNull() {
        assertFalse(gate.revoke(null));
    }

    @Test
    void revokeDecreasesSize() {
        UserToken ut = gate.provision("alice", null, Set.of(SessionGate.ROLE_VIEWER));
        assertEquals(1, gate.size());
        gate.revoke(ut.token);
        assertEquals(0, gate.size());
    }

    @Test
    void revokeOnlyRemovesTargetToken() {
        UserToken alice = gate.provision("alice", null, Set.of(SessionGate.ROLE_VIEWER));
        UserToken bob = gate.provision("bob", null, Set.of(SessionGate.ROLE_VIEWER));
        gate.revoke(alice.token);
        assertNull(gate.resolve(alice.token));
        assertNotNull(gate.resolve(bob.token));
    }

    // =====================================================================
    // SessionGate — revokeAll
    // =====================================================================

    @Test
    void revokeAllRemovesAllTokensForUser() {
        UserToken t1 = gate.provision("alice", null, Set.of(SessionGate.ROLE_VIEWER));
        UserToken t2 = gate.provision("alice", null, Set.of(SessionGate.ROLE_EDITOR));
        int removed = gate.revokeAll("alice");
        assertEquals(2, removed);
        assertNull(gate.resolve(t1.token));
        assertNull(gate.resolve(t2.token));
    }

    @Test
    void revokeAllReturnsCountOfRemovedTokens() {
        gate.provision("alice", null, Set.of(SessionGate.ROLE_VIEWER));
        gate.provision("alice", null, Set.of(SessionGate.ROLE_VIEWER));
        gate.provision("alice", null, Set.of(SessionGate.ROLE_VIEWER));
        assertEquals(3, gate.revokeAll("alice"));
    }

    @Test
    void revokeAllDoesNotAffectOtherUsers() {
        UserToken alice = gate.provision("alice", null, Set.of(SessionGate.ROLE_VIEWER));
        UserToken bob = gate.provision("bob", null, Set.of(SessionGate.ROLE_EDITOR));
        gate.revokeAll("alice");
        assertNull(gate.resolve(alice.token));
        assertNotNull(gate.resolve(bob.token));
    }

    @Test
    void revokeAllReturnsZeroForUnknownUser() {
        assertEquals(0, gate.revokeAll("ghost"));
    }

    @Test
    void revokeAllReturnsZeroForBlankUserId() {
        assertEquals(0, gate.revokeAll(""));
        assertEquals(0, gate.revokeAll(null));
    }

    // =====================================================================
    // SessionGate — hasRole
    // =====================================================================

    @Test
    void hasRoleReturnsTrueForValidTokenWithRole() {
        UserToken ut = gate.provision("alice", null, Set.of(SessionGate.ROLE_EDITOR));
        assertTrue(gate.hasRole(ut.token, SessionGate.ROLE_EDITOR));
    }

    @Test
    void hasRoleReturnsFalseForValidTokenWithoutRole() {
        UserToken ut = gate.provision("alice", null, Set.of(SessionGate.ROLE_VIEWER));
        assertFalse(gate.hasRole(ut.token, SessionGate.ROLE_EDITOR));
    }

    @Test
    void hasRoleReturnsFalseForUnknownToken() {
        assertFalse(gate.hasRole("bogus-token", SessionGate.ROLE_EDITOR));
    }

    @Test
    void hasRoleReturnsFalseForNullToken() {
        assertFalse(gate.hasRole(null, SessionGate.ROLE_EDITOR));
    }

    @Test
    void hasRoleReturnsFalseForNullRole() {
        UserToken ut = gate.provision("alice", null, Set.of(SessionGate.ROLE_EDITOR));
        assertFalse(gate.hasRole(ut.token, null));
    }

    @Test
    void hasRoleReturnsFalseAfterRevoke() {
        UserToken ut = gate.provision("alice", null, Set.of(SessionGate.ROLE_EDITOR));
        gate.revoke(ut.token);
        assertFalse(gate.hasRole(ut.token, SessionGate.ROLE_EDITOR));
    }

    // =====================================================================
    // SessionGate — listTokens
    // =====================================================================

    @Test
    void listTokensIsEmptyInitially() {
        assertTrue(gate.listTokens().isEmpty());
    }

    @Test
    void listTokensContainsAllProvisioned() {
        UserToken alice = gate.provision("alice", null, Set.of(SessionGate.ROLE_EDITOR));
        UserToken bob = gate.provision("bob", null, Set.of(SessionGate.ROLE_VIEWER));
        List<UserToken> tokens = gate.listTokens();
        assertEquals(2, tokens.size());
        assertTrue(tokens.contains(alice));
        assertTrue(tokens.contains(bob));
    }

    @Test
    void listTokensIsUnmodifiable() {
        gate.provision("alice", null, Set.of(SessionGate.ROLE_VIEWER));
        assertThrows(UnsupportedOperationException.class,
                () -> gate.listTokens().remove(0));
    }

    @Test
    void listTokensReflectsRevocations() {
        UserToken alice = gate.provision("alice", null, Set.of(SessionGate.ROLE_EDITOR));
        gate.provision("bob", null, Set.of(SessionGate.ROLE_VIEWER));
        gate.revoke(alice.token);
        List<UserToken> tokens = gate.listTokens();
        assertEquals(1, tokens.size());
        assertFalse(tokens.contains(alice));
    }

    // =====================================================================
    // SessionGate — size and clear
    // =====================================================================

    @Test
    void sizeStartsAtZero() {
        assertEquals(0, gate.size());
    }

    @Test
    void sizeIncrementsOnProvision() {
        gate.provision("alice", null, Set.of(SessionGate.ROLE_VIEWER));
        assertEquals(1, gate.size());
        gate.provision("bob", null, Set.of(SessionGate.ROLE_VIEWER));
        assertEquals(2, gate.size());
    }

    @Test
    void clearRemovesAllTokens() {
        gate.provision("alice", null, Set.of(SessionGate.ROLE_EDITOR));
        gate.provision("bob", null, Set.of(SessionGate.ROLE_VIEWER));
        gate.clear();
        assertEquals(0, gate.size());
        assertTrue(gate.listTokens().isEmpty());
    }

    @Test
    void clearOnEmptyGateIsHarmless() {
        assertDoesNotThrow(() -> gate.clear());
    }

    // =====================================================================
    // Role-based access scenarios
    // =====================================================================

    @Test
    void editorCanEditButViewerCannot() {
        UserToken editor = gate.provision("alice", null, Set.of(SessionGate.ROLE_EDITOR));
        UserToken viewer = gate.provision("bob", null, Set.of(SessionGate.ROLE_VIEWER));

        assertTrue(gate.hasRole(editor.token, SessionGate.ROLE_EDITOR));
        assertFalse(gate.hasRole(editor.token, SessionGate.ROLE_VIEWER));

        assertTrue(gate.hasRole(viewer.token, SessionGate.ROLE_VIEWER));
        assertFalse(gate.hasRole(viewer.token, SessionGate.ROLE_EDITOR));
    }

    @Test
    void runtimeAdminCanControlRuntimeButNotEdit() {
        UserToken rtAdmin = gate.provision("ops", null,
                Set.of(SessionGate.ROLE_RUNTIME_ADMIN));

        assertTrue(gate.hasRole(rtAdmin.token, SessionGate.ROLE_RUNTIME_ADMIN));
        assertFalse(gate.hasRole(rtAdmin.token, SessionGate.ROLE_EDITOR));
        assertFalse(gate.hasRole(rtAdmin.token, SessionGate.ROLE_VIEWER));
    }

    @Test
    void superUserHasAllRoles() {
        UserToken su = gate.provision("super", null,
                Set.of(SessionGate.ROLE_EDITOR,
                       SessionGate.ROLE_VIEWER,
                       SessionGate.ROLE_RUNTIME_ADMIN));

        assertTrue(gate.hasRole(su.token, SessionGate.ROLE_EDITOR));
        assertTrue(gate.hasRole(su.token, SessionGate.ROLE_VIEWER));
        assertTrue(gate.hasRole(su.token, SessionGate.ROLE_RUNTIME_ADMIN));
    }

    @Test
    void unauthenticatedRequestIsRejected() {
        // Simulates a handler checking: if resolve returns null → 401
        assertNull(gate.resolve("missing-token"));
        assertFalse(gate.hasRole("missing-token", SessionGate.ROLE_VIEWER));
    }

    @Test
    void tokenRotationScenario() {
        // Admin provisions old token, then rotates (revoke + provision)
        UserToken oldToken = gate.provisionWithToken("old-shared-secret", "admin", null,
                Set.of(SessionGate.ROLE_EDITOR, SessionGate.ROLE_RUNTIME_ADMIN));

        assertTrue(gate.hasRole("old-shared-secret", SessionGate.ROLE_EDITOR));

        // Rotation: revoke old, issue new
        gate.revoke(oldToken.token);
        UserToken newToken = gate.provision("admin", "Admin",
                Set.of(SessionGate.ROLE_EDITOR, SessionGate.ROLE_RUNTIME_ADMIN));

        assertFalse(gate.hasRole("old-shared-secret", SessionGate.ROLE_EDITOR),
                "Old token must no longer grant access");
        assertTrue(gate.hasRole(newToken.token, SessionGate.ROLE_EDITOR),
                "New token must grant access");
    }

    // =====================================================================
    // Thread safety
    // =====================================================================

    @Test
    void concurrentProvisionProducesUniqueTokens() throws InterruptedException {
        int threads = 40;
        CountDownLatch start = new CountDownLatch(1);
        CountDownLatch done = new CountDownLatch(threads);
        Set<String> tokens = ConcurrentHashMap.newKeySet();
        AtomicInteger duplicates = new AtomicInteger(0);

        ExecutorService exec = Executors.newFixedThreadPool(threads);
        for (int i = 0; i < threads; i++) {
            final int idx = i;
            exec.submit(() -> {
                try {
                    start.await();
                    UserToken ut = gate.provision("user-" + idx, null,
                            Set.of(SessionGate.ROLE_VIEWER));
                    if (!tokens.add(ut.token)) duplicates.incrementAndGet();
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                } finally {
                    done.countDown();
                }
            });
        }
        start.countDown();
        assertTrue(done.await(5, TimeUnit.SECONDS));
        exec.shutdown();

        assertEquals(0, duplicates.get(), "All provisioned tokens must be unique");
        assertEquals(threads, gate.size());
    }

    @Test
    void concurrentProvisionAndResolveAreSafe() throws InterruptedException {
        int threads = 30;
        CountDownLatch start = new CountDownLatch(1);
        CountDownLatch done = new CountDownLatch(threads);
        AtomicInteger nullResolutions = new AtomicInteger(0);

        // Pre-provision one known token
        UserToken known = gate.provision("alice", null, Set.of(SessionGate.ROLE_EDITOR));
        String knownTokenStr = known.token;

        ExecutorService exec = Executors.newFixedThreadPool(threads);
        for (int i = 0; i < threads; i++) {
            final int idx = i;
            exec.submit(() -> {
                try {
                    start.await();
                    if (idx % 2 == 0) {
                        // Even threads resolve the known token
                        if (gate.resolve(knownTokenStr) == null) {
                            nullResolutions.incrementAndGet();
                        }
                    } else {
                        // Odd threads provision new tokens
                        gate.provision("user-" + idx, null, Set.of(SessionGate.ROLE_VIEWER));
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                } finally {
                    done.countDown();
                }
            });
        }
        start.countDown();
        assertTrue(done.await(5, TimeUnit.SECONDS));
        exec.shutdown();

        assertEquals(0, nullResolutions.get(),
                "Known token must always resolve during concurrent provision");
    }

    @Test
    void concurrentRevokeAllIsSafe() throws InterruptedException {
        // Give alice 50 tokens, then revoke all from multiple threads
        for (int i = 0; i < 50; i++) {
            gate.provision("alice", null, Set.of(SessionGate.ROLE_VIEWER));
        }
        gate.provision("bob", null, Set.of(SessionGate.ROLE_VIEWER));

        int threads = 10;
        CountDownLatch start = new CountDownLatch(1);
        CountDownLatch done = new CountDownLatch(threads);
        AtomicInteger totalRemoved = new AtomicInteger(0);

        ExecutorService exec = Executors.newFixedThreadPool(threads);
        for (int i = 0; i < threads; i++) {
            exec.submit(() -> {
                try {
                    start.await();
                    totalRemoved.addAndGet(gate.revokeAll("alice"));
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                } finally {
                    done.countDown();
                }
            });
        }
        start.countDown();
        assertTrue(done.await(5, TimeUnit.SECONDS));
        exec.shutdown();

        assertEquals(50, totalRemoved.get(),
                "All 50 alice tokens must be revoked exactly once");
        // Bob's token survives
        assertEquals(1, gate.size());
    }
}
