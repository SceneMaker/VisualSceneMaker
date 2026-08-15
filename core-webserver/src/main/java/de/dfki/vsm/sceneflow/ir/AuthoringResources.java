package de.dfki.vsm.sceneflow.ir;

import org.json.JSONObject;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Reads the JSON documents the generator is configured by: the pattern catalogue and the
 * semantic-rule mapping.
 *
 * <p>They are authored in {@code doc/} and copied onto the classpath at build time. The classpath is
 * the only source that works wherever the server happens to run, but a caller may still name a file
 * directly, which is how tests supply a stand-in catalogue. A repository-relative path is tried last
 * so the Gradle tasks keep working when run from a checkout.
 */
public final class AuthoringResources {

    private AuthoringResources() {
    }

    /** Reads {@code fileName} from an explicit path if given, else the classpath, else {@code doc/}. */
    public static JSONObject read(final Path explicitPath, final String fileName) {
        if (explicitPath != null && Files.isRegularFile(explicitPath)) {
            try {
                return new JSONObject(Files.readString(explicitPath));
            } catch (IOException | RuntimeException ignored) {
                return null;
            }
        }
        final JSONObject fromClasspath = readClasspath("/sceneflow/" + fileName);
        if (fromClasspath != null) {
            return fromClasspath;
        }
        final Path inCheckout = Path.of("doc", fileName);
        if (Files.isRegularFile(inCheckout)) {
            try {
                return new JSONObject(Files.readString(inCheckout));
            } catch (IOException | RuntimeException ignored) {
                return null;
            }
        }
        return null;
    }

    private static JSONObject readClasspath(final String resource) {
        try (InputStream in = AuthoringResources.class.getResourceAsStream(resource)) {
            if (in == null) {
                return null;
            }
            return new JSONObject(new String(in.readAllBytes(), StandardCharsets.UTF_8));
        } catch (IOException | RuntimeException ignored) {
            return null;
        }
    }
}
