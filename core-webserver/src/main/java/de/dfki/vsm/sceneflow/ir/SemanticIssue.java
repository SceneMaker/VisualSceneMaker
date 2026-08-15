package de.dfki.vsm.sceneflow.ir;

public final class SemanticIssue {
    private final String code;
    private final String path;
    private final String message;
    private final String severity;

    public SemanticIssue(final String code, final String path, final String message) {
        this(code, path, message, "error");
    }

    public SemanticIssue(final String code, final String path, final String message, final String severity) {
        this.code = code;
        this.path = path;
        this.message = message;
        this.severity = severity == null || severity.isBlank() ? "error" : severity;
    }

    public String getCode() {
        return code;
    }

    public String getPath() {
        return path;
    }

    public String getMessage() {
        return message;
    }

    public String getSeverity() {
        return severity;
    }
}
