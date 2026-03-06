package de.dfki.vsm.web.analysis;

public enum FlowSemanticKind {
    NOT_END("not-end"),
    DEFINITE_END("definite-end"),
    POTENTIAL_END("potential-end");

    private final String jsonValue;

    FlowSemanticKind(String jsonValue) {
        this.jsonValue = jsonValue;
    }

    public String getJsonValue() {
        return jsonValue;
    }
}
