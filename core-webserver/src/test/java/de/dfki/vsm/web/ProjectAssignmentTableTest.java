package de.dfki.vsm.web;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

class ProjectAssignmentTableTest {

    private Path dir;

    @BeforeEach
    void setUp() throws IOException {
        dir = Files.createTempDirectory("vsm-assignments-test");
    }

    @AfterEach
    void tearDown() throws IOException {
        try (var walk = Files.walk(dir)) {
            walk.sorted(Comparator.reverseOrder()).forEach(p -> {
                try {
                    Files.delete(p);
                } catch (IOException ignored) {
                    // best-effort cleanup
                }
            });
        }
    }

    private ProjectAssignmentTable writeAndLoad(String json) throws IOException {
        Path file = dir.resolve("project-assignments.json");
        Files.writeString(file, json);
        return new ProjectAssignmentTable(file);
    }

    @Test
    void failsOpenWhenFileMissing() {
        ProjectAssignmentTable table = new ProjectAssignmentTable(dir.resolve("does-not-exist.json"));

        assertTrue(table.canAccess("anyone", "/any/path"));
        assertFalse(table.isAdmin("anyone")); // "unrestricted" means reachable, not admin
    }

    @Test
    void assignedUserCanAccessOnlyTheirProject() throws IOException {
        ProjectAssignmentTable table = writeAndLoad(
                "{\"users\": {\"alice\": {\"admin\": false, \"projects\": [\"/app/project/Confidence - SIA Layer\"]}}}");

        assertTrue(table.canAccess("alice", "/app/project/Confidence - SIA Layer"));
        assertFalse(table.canAccess("alice", "/app/project/Someone Elses Project"));
        assertFalse(table.isAdmin("alice"));
    }

    @Test
    void projectsForReturnsTheUsersExplicitAssignments() throws IOException {
        ProjectAssignmentTable table = writeAndLoad(
                "{\"users\": {\"gebhard\": {\"admin\": true, \"projects\": [\"/app/project/Confidence - SIA Layer\"]},"
                        + " \"reinwarth\": {\"admin\": false, \"projects\": [\"/app/project/A\", \"/app/project/B\"]}}}");

        assertEquals(java.util.Set.of("/app/project/Confidence - SIA Layer"), table.projectsFor("gebhard"));
        assertEquals(java.util.Set.of("/app/project/A", "/app/project/B"), table.projectsFor("reinwarth"));
        // Unknown user and (deliberately) an admin's list is only what's explicitly assigned —
        // NOT "everything an admin can reach".
        assertTrue(table.projectsFor("mallory").isEmpty());
    }

    @Test
    void projectsForIsEmptyWhenFileMissing() {
        ProjectAssignmentTable table = new ProjectAssignmentTable(dir.resolve("does-not-exist.json"));
        assertTrue(table.projectsFor("anyone").isEmpty());
    }

    @Test
    void unknownUserCannotAccessAnything() throws IOException {
        ProjectAssignmentTable table = writeAndLoad(
                "{\"users\": {\"alice\": {\"admin\": false, \"projects\": [\"/app/project/X\"]}}}");

        assertFalse(table.canAccess("mallory", "/app/project/X"));
        assertFalse(table.isAdmin("mallory"));
    }

    @Test
    void adminCanAccessAnyPathRegardlessOfProjectsList() throws IOException {
        ProjectAssignmentTable table = writeAndLoad(
                "{\"users\": {\"bob\": {\"admin\": true, \"projects\": []}}}");

        assertTrue(table.isAdmin("bob"));
        assertTrue(table.canAccess("bob", "/app/project/Anything At All"));
    }

    @Test
    void reReadsFileOnEveryCheckWithoutServerRestart() throws IOException {
        Path file = dir.resolve("project-assignments.json");
        Files.writeString(file, "{\"users\": {\"alice\": {\"admin\": false, \"projects\": []}}}");
        ProjectAssignmentTable table = new ProjectAssignmentTable(file);
        assertFalse(table.canAccess("alice", "/app/project/X"));

        Files.writeString(file, "{\"users\": {\"alice\": {\"admin\": false, \"projects\": [\"/app/project/X\"]}}}");
        assertTrue(table.canAccess("alice", "/app/project/X"));
    }

    @Test
    void malformedJsonFailsClosedRatherThanThrowing() throws IOException {
        // Different from a genuinely absent file: an existing-but-broken file signals
        // deliberate intent to restrict, so a typo should lock everyone out (loudly),
        // not silently fall back to unrestricted.
        Path file = dir.resolve("project-assignments.json");
        Files.writeString(file, "{ this is not valid json");
        ProjectAssignmentTable table = new ProjectAssignmentTable(file);

        assertDoesNotThrow(() -> table.canAccess("anyone", "/any/path"));
        assertFalse(table.canAccess("anyone", "/any/path"));
        assertFalse(table.isAdmin("anyone"));
    }

    @Test
    void setUserCreatesTheFileWhenAbsent() {
        Path file = dir.resolve("project-assignments.json");
        ProjectAssignmentTable table = new ProjectAssignmentTable(file);
        assertFalse(Files.exists(file));

        table.setUser("alice", false, Set.of("/app/project/X"));

        assertTrue(Files.exists(file));
        assertTrue(table.canAccess("alice", "/app/project/X"));
        assertFalse(table.canAccess("bob", "/app/project/X"));
    }

    @Test
    void setUserUpdatesExistingEntryWithoutLosingOtherUsers() throws IOException {
        ProjectAssignmentTable table = writeAndLoad(
                "{\"users\": {\"alice\": {\"admin\": false, \"projects\": [\"/app/project/X\"]}}}");

        table.setUser("bob", true, Set.of());

        assertTrue(table.canAccess("alice", "/app/project/X")); // untouched
        assertTrue(table.isAdmin("bob"));

        table.setUser("alice", false, Set.of("/app/project/Y")); // replace, not merge

        assertFalse(table.canAccess("alice", "/app/project/X"));
        assertTrue(table.canAccess("alice", "/app/project/Y"));
        assertTrue(table.isAdmin("bob")); // still untouched
    }

    @Test
    void removeUserDeletesOnlyThatEntry() throws IOException {
        ProjectAssignmentTable table = writeAndLoad(
                "{\"users\": {\"alice\": {\"admin\": false, \"projects\": [\"/app/project/X\"]},"
                        + "\"bob\": {\"admin\": true, \"projects\": []}}}");

        table.removeUser("alice");

        assertFalse(table.canAccess("alice", "/app/project/X"));
        assertTrue(table.isAdmin("bob")); // untouched
    }

    @Test
    void removeUserOnMissingFileIsANoOp() {
        ProjectAssignmentTable table = new ProjectAssignmentTable(dir.resolve("does-not-exist.json"));

        assertDoesNotThrow(() -> table.removeUser("alice"));
        assertFalse(Files.exists(dir.resolve("does-not-exist.json"))); // still doesn't exist
    }

    @Test
    void listUsersAsJsonReflectsCurrentState() throws IOException {
        ProjectAssignmentTable table = writeAndLoad(
                "{\"users\": {\"alice\": {\"admin\": false, \"projects\": [\"/app/project/X\"]}}}");

        var before = table.listUsersAsJson().getJSONObject("users");
        assertTrue(before.has("alice"));
        assertFalse(before.has("bob"));

        table.setUser("bob", true, Set.of());

        var after = table.listUsersAsJson().getJSONObject("users");
        assertTrue(after.has("alice"));
        assertTrue(after.has("bob"));
        assertTrue(after.getJSONObject("bob").getBoolean("admin"));
    }

    @Test
    void listUsersAsJsonNeverThrowsEvenWhenUnconfigured() {
        ProjectAssignmentTable table = new ProjectAssignmentTable(dir.resolve("does-not-exist.json"));

        var users = table.listUsersAsJson().getJSONObject("users");
        assertEquals(0, users.keySet().size());
    }
}
