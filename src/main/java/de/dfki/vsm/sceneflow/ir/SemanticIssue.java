package de.dfki.vsm.sceneflow.ir;

public final class SemanticIssue {
    private final String code;
    private final String path;
    private final String message;

    public SemanticIssue(final String code, final String path, final String message) {
        this.code = code;
        this.path = path;
        this.message = message;
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
}
