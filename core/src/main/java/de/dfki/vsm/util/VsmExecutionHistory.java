package de.dfki.vsm.util;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Instant;

/**
 * Tracks how many times each project has been run on this machine.
 * Stored in ~/.vsm/execution-history.json, keyed by project UUID.
 *
 * Format:
 *   { "executions": { "<uuid>": { "count": 3, "last": "ISO8601" } } }
 */
public final class VsmExecutionHistory {

    private static final Path HISTORY_FILE =
            Paths.get(System.getProperty("user.home"), ".vsm", "execution-history.json");

    private VsmExecutionHistory() {}

    /** Returns how many times the project with the given UUID has been run on this machine. */
    public static int getExecutionCount(String projectUUID) {
        if (projectUUID == null || projectUUID.isBlank()) return 0;
        try {
            String json = readFile();
            if (json == null) return 0;
            String block = extractBlock(json, projectUUID);
            if (block == null) return 0;
            return parseIntField(block, "count");
        } catch (Exception e) {
            return 0;
        }
    }

    /** Increments the execution count for the given project UUID and records the current timestamp. */
    public static void recordExecution(String projectUUID) {
        if (projectUUID == null || projectUUID.isBlank()) return;
        try {
            String json = readFile();
            if (json == null) json = "{\"executions\":{}}";

            int execsStart = json.indexOf("\"executions\"");
            if (execsStart < 0) json = "{\"executions\":{}}";

            int current = getExecutionCount(projectUUID);
            String entry = "\"" + projectUUID + "\":{\"count\":" + (current + 1)
                    + ",\"last\":\"" + Instant.now().toString() + "\"}";

            json = upsertEntry(json, projectUUID, entry);
            writeFile(json);
        } catch (Exception ignored) {}
    }

    // --- simple JSON manipulation without library dependency ---

    private static String readFile() {
        try {
            if (!Files.exists(HISTORY_FILE)) return null;
            return Files.readString(HISTORY_FILE, StandardCharsets.UTF_8).trim();
        } catch (Exception e) {
            return null;
        }
    }

    private static void writeFile(String json) {
        try {
            Path dir = HISTORY_FILE.getParent();
            if (!Files.exists(dir)) Files.createDirectories(dir);
            Files.writeString(HISTORY_FILE, json, StandardCharsets.UTF_8);
        } catch (Exception ignored) {}
    }

    /** Finds the JSON object for the given key inside the "executions" object. */
    private static String extractBlock(String json, String key) {
        String search = "\"" + key + "\":{";
        int idx = json.indexOf(search);
        if (idx < 0) return null;
        int start = idx + search.length() - 1; // points at '{'
        int depth = 0;
        int end = start;
        for (int i = start; i < json.length(); i++) {
            char c = json.charAt(i);
            if (c == '{') depth++;
            else if (c == '}') { depth--; if (depth == 0) { end = i; break; } }
        }
        return json.substring(start, end + 1);
    }

    private static int parseIntField(String block, String field) {
        String search = "\"" + field + "\":";
        int idx = block.indexOf(search);
        if (idx < 0) return 0;
        int start = idx + search.length();
        int end = start;
        while (end < block.length() && (Character.isDigit(block.charAt(end)))) end++;
        if (end == start) return 0;
        return Integer.parseInt(block.substring(start, end));
    }

    /** Inserts or replaces the entry for the given key inside "executions". */
    private static String upsertEntry(String json, String key, String entry) {
        String search = "\"" + key + "\":{";
        int idx = json.indexOf(search);
        if (idx >= 0) {
            // Replace existing entry
            int start = idx + search.length() - 1;
            int depth = 0;
            int end = start;
            for (int i = start; i < json.length(); i++) {
                char c = json.charAt(i);
                if (c == '{') depth++;
                else if (c == '}') { depth--; if (depth == 0) { end = i; break; } }
            }
            return json.substring(0, idx) + entry + json.substring(end + 1);
        }

        // Insert new entry into executions object
        int execsStart = json.indexOf("\"executions\":{");
        if (execsStart < 0) {
            execsStart = json.indexOf("\"executions\": {");
        }
        if (execsStart < 0) return json;

        int braceOpen = json.indexOf("{", execsStart + "\"executions\":".length());
        if (braceOpen < 0) return json;

        // Find matching closing brace for executions
        int depth = 0;
        int execsEnd = braceOpen;
        for (int i = braceOpen; i < json.length(); i++) {
            char c = json.charAt(i);
            if (c == '{') depth++;
            else if (c == '}') { depth--; if (depth == 0) { execsEnd = i; break; } }
        }

        String inside = json.substring(braceOpen + 1, execsEnd).trim();
        String separator = inside.isEmpty() ? "" : ",";
        return json.substring(0, braceOpen + 1) + inside + separator + entry + json.substring(execsEnd);
    }
}
