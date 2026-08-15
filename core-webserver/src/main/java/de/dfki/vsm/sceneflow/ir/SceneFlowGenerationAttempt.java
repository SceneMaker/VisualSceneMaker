package de.dfki.vsm.sceneflow.ir;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public final class SceneFlowGenerationAttempt {

    private final int attempt;
    private final String irSource;
    private final List<SemanticIssue> semanticIssues;
    private final String compileError;
    private final boolean success;

    public SceneFlowGenerationAttempt(
            final int attempt,
            final String irSource,
            final List<SemanticIssue> semanticIssues,
            final String compileError,
            final boolean success) {
        this.attempt = attempt;
        this.irSource = irSource;
        this.semanticIssues = semanticIssues == null
                ? List.of()
                : Collections.unmodifiableList(new ArrayList<>(semanticIssues));
        this.compileError = compileError;
        this.success = success;
    }

    public int getAttempt() {
        return attempt;
    }

    public String getIrSource() {
        return irSource;
    }

    public List<SemanticIssue> getSemanticIssues() {
        return semanticIssues;
    }

    public String getCompileError() {
        return compileError;
    }

    public boolean isSuccess() {
        return success;
    }
}

