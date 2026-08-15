package de.dfki.vsm.testsupport;

import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Locates repository fixtures independently of the working directory a test runs in.
 *
 * <p>Gradle starts a module's tests in that module's directory, so a plain {@code Path.of("doc/…")}
 * only resolves when the tests live in the root project. Walking up until {@code doc/DesignPatterns}
 * appears keeps the same fixtures reachable from any module and from an IDE run.
 */
public final class TestRepoPaths {

    private static final Path REPO_ROOT = locateRepoRoot();

    private TestRepoPaths() {
    }

    /** Resolves a path relative to the repository's {@code doc} directory. */
    public static Path doc(final String relative) {
        return REPO_ROOT.resolve("doc").resolve(relative);
    }

    /** The repository checkout root. */
    public static Path repoRoot() {
        return REPO_ROOT;
    }

    private static Path locateRepoRoot() {
        Path candidate = Path.of("").toAbsolutePath();
        while (candidate != null) {
            if (Files.isDirectory(candidate.resolve("doc/DesignPatterns"))) {
                return candidate;
            }
            candidate = candidate.getParent();
        }
        throw new IllegalStateException(
                "Cannot locate the repository root from " + Path.of("").toAbsolutePath());
    }
}
