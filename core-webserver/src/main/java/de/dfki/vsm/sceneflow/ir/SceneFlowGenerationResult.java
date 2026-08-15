package de.dfki.vsm.sceneflow.ir;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public final class SceneFlowGenerationResult {

    private final boolean success;
    private final Path outputPath;
    private final List<SceneFlowGenerationAttempt> attempts;

    public SceneFlowGenerationResult(
            final boolean success,
            final Path outputPath,
            final List<SceneFlowGenerationAttempt> attempts) {
        this.success = success;
        this.outputPath = outputPath;
        this.attempts = attempts == null
                ? List.of()
                : Collections.unmodifiableList(new ArrayList<>(attempts));
    }

    public boolean isSuccess() {
        return success;
    }

    public Path getOutputPath() {
        return outputPath;
    }

    public List<SceneFlowGenerationAttempt> getAttempts() {
        return attempts;
    }
}

