package de.dfki.vsm.sceneflow.ir;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public final class SemanticValidationResult {
    private final List<SemanticIssue> issues = new ArrayList<>();

    public void addIssue(final String code, final String path, final String message) {
        issues.add(new SemanticIssue(code, path, message));
    }

    public List<SemanticIssue> getIssues() {
        return Collections.unmodifiableList(issues);
    }

    public boolean hasErrors() {
        return !issues.isEmpty();
    }
}
