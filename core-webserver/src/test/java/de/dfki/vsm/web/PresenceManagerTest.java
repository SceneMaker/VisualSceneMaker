package de.dfki.vsm.web;

import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Functional tests for {@link PresenceManager} and {@link UserPresence}.
 */
class PresenceManagerTest {

    private PresenceManager pm;

    @BeforeEach
    void setUp() {
        pm = new PresenceManager();
    }

    // ------------------------------------------------------------------
    // Initial state
    // ------------------------------------------------------------------

    @Test
    void newManagerIsEmpty() {
        assertEquals(0, pm.size());
        assertTrue(pm.getAll().isEmpty());
    }

    // ------------------------------------------------------------------
    // join()
    // ------------------------------------------------------------------

    @Test
    void joinAddsUser() {
        pm.join("user1", "Alice");
        assertEquals(1, pm.size());
        assertTrue(pm.isPresent("user1"));
    }

    @Test
    void joinAssignsDisplayName() {
        UserPresence p = pm.join("user1", "Alice");
        assertEquals("Alice", p.displayName);
    }

    @Test
    void joinDeriveDisplayNameWhenNull() {
        UserPresence p = pm.join("abcdefghij", null);
        assertEquals("User-abcdefgh", p.displayName);
    }

    @Test
    void joinDeriveDisplayNameWhenBlank() {
        UserPresence p = pm.join("abcdefghij", "   ");
        assertEquals("User-abcdefgh", p.displayName);
    }

    @Test
    void joinAssignsColorFromPalette() {
        UserPresence p = pm.join("user1", "Alice");
        assertNotNull(p.color);
        assertTrue(p.color.startsWith("#"));
        assertEquals(PresenceManager.COLOR_PALETTE[0], p.color);
    }

    @Test
    void joinCyclesThroughPalette() {
        for (int i = 0; i < PresenceManager.COLOR_PALETTE.length; i++) {
            pm.join("user" + i, null);
        }
        // Next user wraps around to index 0
        UserPresence wrapped = pm.join("extra", null);
        assertEquals(PresenceManager.COLOR_PALETTE[0], wrapped.color);
    }

    @Test
    void joinIsIdempotentReturnsExistingRecord() {
        UserPresence first = pm.join("user1", "Alice");
        UserPresence second = pm.join("user1", "Bob");  // re-join should not overwrite
        assertSame(first, second);
        assertEquals("Alice", first.displayName);  // name unchanged
        assertEquals(1, pm.size());
    }

    @Test
    void joinRefusesBlankUserId() {
        assertThrows(IllegalArgumentException.class, () -> pm.join("", "Alice"));
        assertThrows(IllegalArgumentException.class, () -> pm.join("   ", "Alice"));
        assertThrows(IllegalArgumentException.class, () -> pm.join(null, "Alice"));
    }

    @Test
    void joinUpdatesLastSeenOnReJoin() throws InterruptedException {
        UserPresence p = pm.join("user1", "Alice");
        long first = p.lastSeen;
        Thread.sleep(2);
        pm.join("user1", "Alice");  // re-join
        assertTrue(p.lastSeen >= first);
    }

    // ------------------------------------------------------------------
    // leave()
    // ------------------------------------------------------------------

    @Test
    void leaveRemovesUser() {
        pm.join("user1", "Alice");
        UserPresence removed = pm.leave("user1");
        assertNotNull(removed);
        assertEquals("user1", removed.userId);
        assertFalse(pm.isPresent("user1"));
        assertEquals(0, pm.size());
    }

    @Test
    void leaveOnAbsentUserReturnsNull() {
        assertNull(pm.leave("nobody"));
    }

    // ------------------------------------------------------------------
    // update()
    // ------------------------------------------------------------------

    @Test
    void updateSetsActiveNodeId() {
        pm.join("user1", "Alice");
        UserPresence p = pm.update("user1", "node-42", null);
        assertNotNull(p);
        assertEquals("node-42", p.activeNodeId);
    }

    @Test
    void updateClearsActiveNodeIdWithNull() {
        pm.join("user1", "Alice");
        pm.update("user1", "node-42", null);
        pm.update("user1", null, null);
        assertNull(pm.get("user1").activeNodeId);
    }

    @Test
    void updateSetsViewport() {
        pm.join("user1", "Alice");
        JSONObject vp = new JSONObject();
        vp.put("x", 10);
        vp.put("y", 20);
        vp.put("width", 800);
        vp.put("height", 600);
        pm.update("user1", null, vp);

        JSONObject stored = pm.get("user1").viewport;
        assertNotNull(stored);
        assertEquals(10, stored.getInt("x"));
    }

    @Test
    void updateMakesDefensiveCopyOfViewport() {
        pm.join("user1", "Alice");
        JSONObject vp = new JSONObject();
        vp.put("x", 10);
        pm.update("user1", null, vp);

        vp.put("x", 999);  // mutate original

        assertEquals(10, pm.get("user1").viewport.getInt("x"));  // stored copy unchanged
    }

    @Test
    void updateDoesNotChangeViewportWhenNull() {
        pm.join("user1", "Alice");
        JSONObject vp = new JSONObject();
        vp.put("x", 5);
        pm.update("user1", null, vp);

        pm.update("user1", "node-1", null);  // null viewport → no change

        assertEquals(5, pm.get("user1").viewport.getInt("x"));
    }

    @Test
    void updateOnAbsentUserReturnsNull() {
        assertNull(pm.update("ghost", "node-1", null));
    }

    @Test
    void updateRefreshesLastSeen() throws InterruptedException {
        pm.join("user1", "Alice");
        long before = pm.get("user1").lastSeen;
        Thread.sleep(2);
        pm.update("user1", "node-1", null);
        assertTrue(pm.get("user1").lastSeen > before);
    }

    // ------------------------------------------------------------------
    // getAll() ordering
    // ------------------------------------------------------------------

    @Test
    void getAllPreservesJoinOrder() {
        pm.join("userA", "Alice");
        pm.join("userB", "Bob");
        pm.join("userC", "Charlie");

        List<UserPresence> all = pm.getAll();
        assertEquals(3, all.size());
        assertEquals("userA", all.get(0).userId);
        assertEquals("userB", all.get(1).userId);
        assertEquals("userC", all.get(2).userId);
    }

    @Test
    void getAllIsUnmodifiable() {
        pm.join("user1", "Alice");
        List<UserPresence> all = pm.getAll();
        assertThrows(UnsupportedOperationException.class, () -> all.remove(0));
    }

    // ------------------------------------------------------------------
    // UserPresence.toJson()
    // ------------------------------------------------------------------

    @Test
    void toJsonContainsMandatoryFields() {
        UserPresence p = pm.join("user1", "Alice");
        JSONObject json = p.toJson();

        assertEquals("user1", json.getString("userId"));
        assertEquals("Alice", json.getString("displayName"));
        assertTrue(json.getString("color").startsWith("#"));
        assertTrue(json.has("lastSeen"));
    }

    @Test
    void toJsonOmitsActiveNodeIdWhenNull() {
        UserPresence p = pm.join("user1", "Alice");
        assertFalse(p.toJson().has("activeNodeId"));
    }

    @Test
    void toJsonIncludesActiveNodeIdWhenSet() {
        pm.join("user1", "Alice");
        pm.update("user1", "node-7", null);
        UserPresence p = pm.get("user1");
        assertEquals("node-7", p.toJson().getString("activeNodeId"));
    }

    @Test
    void toJsonOmitsViewportWhenNull() {
        UserPresence p = pm.join("user1", "Alice");
        assertFalse(p.toJson().has("viewport"));
    }

    @Test
    void toJsonIncludesViewportWhenSet() {
        pm.join("user1", "Alice");
        JSONObject vp = new JSONObject();
        vp.put("x", 0);
        vp.put("y", 0);
        vp.put("width", 1280);
        vp.put("height", 720);
        pm.update("user1", null, vp);
        JSONObject json = pm.get("user1").toJson();
        assertTrue(json.has("viewport"));
        assertEquals(1280, json.getJSONObject("viewport").getInt("width"));
    }

    // ------------------------------------------------------------------
    // deriveDisplayName()
    // ------------------------------------------------------------------

    @Test
    void deriveDisplayNameTruncatesLongId() {
        String name = PresenceManager.deriveDisplayName("abcdefghijklmnop");
        assertEquals("User-abcdefgh", name);
    }

    @Test
    void deriveDisplayNameHandlesShortId() {
        String name = PresenceManager.deriveDisplayName("abc");
        assertEquals("User-abc", name);
    }

    @Test
    void deriveDisplayNameHandlesNullOrBlank() {
        assertEquals("User", PresenceManager.deriveDisplayName(null));
        assertEquals("User", PresenceManager.deriveDisplayName(""));
        assertEquals("User", PresenceManager.deriveDisplayName("   "));
    }

    // ------------------------------------------------------------------
    // Multiple users — independent presence
    // ------------------------------------------------------------------

    @Test
    void multipleUsersHaveUniqueColors() {
        UserPresence a = pm.join("userA", null);
        UserPresence b = pm.join("userB", null);
        UserPresence c = pm.join("userC", null);
        assertNotEquals(a.color, b.color);
        assertNotEquals(b.color, c.color);
    }

    @Test
    void leaveOneDoesNotAffectOthers() {
        pm.join("userA", "Alice");
        pm.join("userB", "Bob");
        pm.leave("userA");
        assertTrue(pm.isPresent("userB"));
        assertEquals(1, pm.size());
    }
}
