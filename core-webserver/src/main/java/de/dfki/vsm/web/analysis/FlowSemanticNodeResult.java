package de.dfki.vsm.web.analysis;

import de.dfki.vsm.model.sceneflow.chart.BasicNode;

public final class FlowSemanticNodeResult {
    private final BasicNode node;
    private final FlowSemanticKind kind;
    private final String reasonCode;
    private final String reasonText;

    public FlowSemanticNodeResult(BasicNode node, FlowSemanticKind kind, String reasonCode, String reasonText) {
        this.node = node;
        this.kind = kind;
        this.reasonCode = reasonCode;
        this.reasonText = reasonText;
    }

    public BasicNode getNode() {
        return node;
    }

    public FlowSemanticKind getKind() {
        return kind;
    }

    public String getReasonCode() {
        return reasonCode;
    }

    public String getReasonText() {
        return reasonText;
    }
}
