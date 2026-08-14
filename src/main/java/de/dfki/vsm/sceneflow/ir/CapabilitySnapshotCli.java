package de.dfki.vsm.sceneflow.ir;

import de.dfki.vsm.web.CapabilitySnapshotBuilder;
import org.json.JSONArray;
import org.json.JSONObject;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Writes a capability snapshot for a project directory.
 *
 * <p>Backs {@code ./gradlew generateCapabilitySnapshot}. The generator used to be inline Groovy in
 * build.gradle that re-parsed the project XML; it now reads the same model the server does, so a
 * build-time snapshot and a snapshot served over HTTP cannot drift apart.
 */
public final class CapabilitySnapshotCli {

    private CapabilitySnapshotCli() {
    }

    public static void main(final String[] args) {
        if (args.length < 2) {
            System.err.println("Usage: CapabilitySnapshotCli <projectDir> <outFile> [schemaFile]");
            System.exit(2);
            return;
        }
        final Path projectDir = Path.of(args[0]);
        final Path outFile = Path.of(args[1]);
        final Path schemaFile = args.length > 2 && !args[2].isBlank() ? Path.of(args[2]) : null;

        try {
            final JSONObject snapshot = CapabilitySnapshotBuilder.buildFromDirectory(projectDir);

            final List<String> drift = schemaFile == null
                    ? List.of()
                    : findSchemaDrift(snapshot, schemaFile);
            if (!drift.isEmpty()) {
                System.err.println("FAILED: capability snapshot does not match " + schemaFile + ":");
                drift.forEach(line -> System.err.println("  " + line));
                System.err.println("Update the schema and the builder together.");
                System.exit(1);
                return;
            }

            if (outFile.getParent() != null) {
                Files.createDirectories(outFile.getParent());
            }
            Files.writeString(outFile, snapshot.toString(2) + System.lineSeparator());

            System.out.println("OK: capability snapshot written to " + outFile.toAbsolutePath());
            System.out.println("Source project directory: " + projectDir.toAbsolutePath());
            System.out.println("Scenes: " + snapshot.getJSONObject("script").getJSONArray("scenes").length()
                    + " · agents: " + snapshot.getJSONObject("project").getJSONArray("agents").length()
                    + " · nodes: " + snapshot.getJSONObject("flow").getJSONArray("nodes").length());
        } catch (Exception exc) {
            System.err.println("FAILED: " + exc.getMessage());
            System.exit(1);
        }
    }

    /**
     * Reports any key the snapshot emits that the schema does not declare.
     *
     * <p>The schema sets {@code additionalProperties:false} throughout, so an undeclared key makes
     * every snapshot invalid. This drift went unnoticed once already, when the schema pinned variable
     * types to an enum the generator had outgrown, because nothing ever checked.
     */
    private static List<String> findSchemaDrift(final JSONObject snapshot, final Path schemaFile)
            throws Exception {
        final List<String> drift = new ArrayList<>();
        if (!Files.exists(schemaFile)) {
            return drift;
        }
        final JSONObject schema = new JSONObject(Files.readString(schemaFile));
        final JSONObject properties = schema.optJSONObject("properties");
        if (properties == null) {
            return drift;
        }

        compareKeys("", properties, snapshot, drift);
        for (String section : new String[] {"project", "script", "flow"}) {
            final JSONObject declared = properties.optJSONObject(section);
            final JSONObject emitted = snapshot.optJSONObject(section);
            if (declared != null && emitted != null && declared.optJSONObject("properties") != null) {
                compareKeys(section, declared.getJSONObject("properties"), emitted, drift);
            }
        }

        final JSONObject sceneItem = properties.optJSONObject("script") == null
                ? null
                : properties.getJSONObject("script").optJSONObject("properties");
        final JSONArray scenes = snapshot.getJSONObject("script").optJSONArray("scenes");
        if (sceneItem != null && scenes != null && !scenes.isEmpty()) {
            final JSONObject declaredScene = sceneItem.optJSONObject("scenes") == null
                    ? null
                    : sceneItem.getJSONObject("scenes").optJSONObject("items");
            if (declaredScene != null && declaredScene.optJSONObject("properties") != null) {
                compareKeys("script.scenes[]", declaredScene.getJSONObject("properties"),
                        scenes.getJSONObject(0), drift);
            }
        }

        final String pinnedVersion = properties.optJSONObject("snapshotVersion") == null
                ? null
                : properties.getJSONObject("snapshotVersion").optString("const", null);
        final String actualVersion = snapshot.optString("snapshotVersion", "");
        if (pinnedVersion != null && !pinnedVersion.equals(actualVersion)) {
            drift.add("snapshotVersion is " + actualVersion + " but the schema pins " + pinnedVersion);
        }
        return drift;
    }

    private static void compareKeys(
            final String path,
            final JSONObject declared,
            final JSONObject emitted,
            final List<String> drift) {
        for (String key : emitted.keySet()) {
            if (!declared.has(key)) {
                drift.add((path.isEmpty() ? "" : path + ".") + key
                        + " is emitted but not declared in the schema");
            }
        }
    }
}
