package de.dfki.vsm.web.analysis;

import de.dfki.vsm.model.sceneflow.chart.BasicNode;

import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.Map;

public final class FlowSemanticResult {
    private final IdentityHashMap<BasicNode, FlowSemanticNodeResult> nodeResults = new IdentityHashMap<>();

    public void put(FlowSemanticNodeResult result) {
        if (result == null || result.getNode() == null) {
            return;
        }
        nodeResults.put(result.getNode(), result);
    }

    public FlowSemanticNodeResult get(BasicNode node) {
        return nodeResults.get(node);
    }

    public Map<BasicNode, FlowSemanticNodeResult> asMap() {
        return Collections.unmodifiableMap(nodeResults);
    }
}
