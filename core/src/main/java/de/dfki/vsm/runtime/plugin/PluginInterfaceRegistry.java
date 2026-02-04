package de.dfki.vsm.runtime.plugin;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public final class PluginInterfaceRegistry {

    private static final LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();
    private static final String DESCRIPTOR_NAME = "sceneflow-interface.json";

    private final Map<String, JSONObject> descriptors;
    private final List<String> errors;
    private final List<String> roots;

    private PluginInterfaceRegistry(
            Map<String, JSONObject> descriptors,
            List<String> errors,
            List<String> roots
    ) {
        this.descriptors = descriptors;
        this.errors = errors;
        this.roots = roots;
    }

    public static PluginInterfaceRegistry loadForProject(Path projectPath) {
        List<Path> roots = resolvePluginRoots(projectPath);
        Map<String, JSONObject> descriptors = new LinkedHashMap<>();
        List<String> errors = new ArrayList<>();

        for (Path root : roots) {
            if (root == null || !Files.isDirectory(root)) {
                continue;
            }
            try (DirectoryStream<Path> pluginDirs = Files.newDirectoryStream(root)) {
                for (Path pluginDir : pluginDirs) {
                    if (!Files.isDirectory(pluginDir)) {
                        continue;
                    }
                    Path descriptorPath = pluginDir.resolve(DESCRIPTOR_NAME);
                    if (!Files.exists(descriptorPath)) {
                        continue;
                    }
                    try {
                        String raw = Files.readString(descriptorPath, StandardCharsets.UTF_8);
                        JSONObject descriptor = new JSONObject(raw);
                        String pluginId = descriptor.optJSONObject("plugin") != null
                                ? descriptor.optJSONObject("plugin").optString("id", pluginDir.getFileName().toString())
                                : pluginDir.getFileName().toString();
                        if (!descriptors.containsKey(pluginId)) {
                            descriptors.put(pluginId, descriptor);
                        }
                    } catch (Exception ex) {
                        errors.add("Failed to parse " + descriptorPath + ": " + ex.getMessage());
                        sLogger.warning("Plugin descriptor parse failed: " + descriptorPath + " (" + ex.getMessage() + ")");
                    }
                }
            } catch (IOException ex) {
                errors.add("Failed to read plugin directory " + root + ": " + ex.getMessage());
                sLogger.warning("Plugin descriptor load failed: " + root + " (" + ex.getMessage() + ")");
            }
        }

        List<String> rootStrings = roots.stream()
                .filter(r -> r != null && Files.exists(r))
                .map(Path::toString)
                .toList();
        return new PluginInterfaceRegistry(descriptors, errors, rootStrings);
    }

    public JSONObject toJson() {
        JSONObject out = new JSONObject();
        JSONArray list = new JSONArray();
        for (JSONObject descriptor : descriptors.values()) {
            list.put(descriptor);
        }
        out.put("interfaces", list);
        out.put("errors", new JSONArray(errors));
        out.put("roots", new JSONArray(roots));
        return out;
    }

    private static List<Path> resolvePluginRoots(Path projectPath) {
        List<Path> roots = new ArrayList<>();
        Set<Path> seen = new LinkedHashSet<>();

        if (projectPath != null) {
            Path base = Files.isDirectory(projectPath) ? projectPath : projectPath.getParent();
            int depth = 0;
            while (base != null && depth < 6) {
                Path candidate = base.resolve("plugins");
                if (Files.isDirectory(candidate) && seen.add(candidate)) {
                    roots.add(candidate);
                    break;
                }
                base = base.getParent();
                depth++;
            }
        }

        Path cwdPlugins = Paths.get("").toAbsolutePath().resolve("plugins");
        if (Files.isDirectory(cwdPlugins) && seen.add(cwdPlugins)) {
            roots.add(cwdPlugins);
        }

        return roots;
    }
}
