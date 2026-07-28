package de.dfki.vsm.web;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;

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
}
