package de.dfki.vsm.web.analysis;

import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public final class SceneFlowFlowSemanticService {
    private final GuardCoverageAnalyzer guardCoverageAnalyzer = new GuardCoverageAnalyzer();

    public FlowSemanticResult analyze(SuperNode currentSuperNode) {
        FlowSemanticResult result = new FlowSemanticResult();
        if (currentSuperNode == null) {
            return result;
        }

        IdentityHashMap<BasicNode, FlowSemanticNodeResult> cache = new IdentityHashMap<>();
        classifyNode(currentSuperNode, cache, result);
        for (BasicNode node : currentSuperNode.getNodeAndSuperNodeList()) {
            classifyNode(node, cache, result);
        }
        return result;
    }

    private FlowSemanticNodeResult classifyNode(BasicNode node,
                                                IdentityHashMap<BasicNode, FlowSemanticNodeResult> cache,
                                                FlowSemanticResult result) {
        if (node == null) {
            return null;
        }
        FlowSemanticNodeResult cached = cache.get(node);
        if (cached != null) {
            return cached;
        }

        FlowSemanticNodeResult local = classifyLocalNode(node);
        FlowSemanticNodeResult resolved = local;
        if (node instanceof SuperNode superNode && local.getKind() != FlowSemanticKind.NOT_END) {
            FlowSemanticNodeResult internal = classifyReachableInternalFlow(superNode, cache, result);
            if (internal != null) {
                resolved = mergeSuperNodeResult(node, local, internal);
            }
        }

        cache.put(node, resolved);
        result.put(resolved);
        return resolved;
    }

    private FlowSemanticNodeResult classifyLocalNode(BasicNode node) {
        if (node.isHistoryNode()) {
            return new FlowSemanticNodeResult(node, FlowSemanticKind.NOT_END, "HISTORY_NODE",
                    "History nodes are not treated as flow end markers.");
        }

        if (hasGuaranteedContinuation(node)) {
            return new FlowSemanticNodeResult(node, FlowSemanticKind.NOT_END, "GUARANTEED_OUTGOING_EDGE",
                    "This node has a guaranteed outgoing continuation.");
        }

        GuardCoverageAnalyzer.CoverageResult coverage = guardCoverageAnalyzer.analyze(node.getCEdgeList());
        switch (coverage.getKind()) {
            case TRUE_BRANCH:
                return new FlowSemanticNodeResult(node, FlowSemanticKind.NOT_END, "CONDITIONAL_TRUE_BRANCH",
                        "A conditional branch with guard 'true' guarantees continuation.");
            case FULL_BOOLEAN_COVERAGE:
                return new FlowSemanticNodeResult(node, FlowSemanticKind.NOT_END, "CONDITIONAL_BOOLEAN_FULL_COVERAGE",
                        "Conditional branches cover both values of the same boolean variable.");
            case PARTIAL:
                return new FlowSemanticNodeResult(node, FlowSemanticKind.POTENTIAL_END, "CONDITIONAL_PARTIAL_COVERAGE",
                        "Conditional branches do not cover all values, so execution may stop here.");
            case UNSUPPORTED:
                return new FlowSemanticNodeResult(node, FlowSemanticKind.POTENTIAL_END, "CONDITIONAL_UNSUPPORTED_GUARD",
                        "Conditional branches cannot be proven exhaustive, so execution may stop here.");
            case NONE:
            default:
                break;
        }

        if (!node.getIEdgeList().isEmpty()) {
            return new FlowSemanticNodeResult(node, FlowSemanticKind.DEFINITE_END, "INTERRUPT_ONLY",
                    "Only interrupt edges leave this node, so normal execution can end here.");
        }

        return new FlowSemanticNodeResult(node, FlowSemanticKind.DEFINITE_END, "NO_OUTGOING",
                "There is no guaranteed outgoing continuation from this node.");
    }

    private boolean hasGuaranteedContinuation(BasicNode node) {
        return node.getDedge() != null || !node.getPEdgeList().isEmpty() || !node.getFEdgeList().isEmpty();
    }

    private FlowSemanticNodeResult classifyReachableInternalFlow(SuperNode superNode,
                                                                 IdentityHashMap<BasicNode, FlowSemanticNodeResult> cache,
                                                                 FlowSemanticResult result) {
        LinkedHashSet<BasicNode> reachableNodes = collectReachableInternalNodes(superNode);
        if (reachableNodes.isEmpty()) {
            return null;
        }
        FlowSemanticNodeResult potential = null;
        FlowSemanticNodeResult definite = null;
        for (BasicNode internalNode : reachableNodes) {
            if (internalNode == null || internalNode.isHistoryNode()) {
                continue;
            }
            FlowSemanticNodeResult internalResult = classifyNode(internalNode, cache, result);
            if (internalResult == null || internalResult.getKind() == FlowSemanticKind.NOT_END) {
                continue;
            }
            if (internalResult.getKind() == FlowSemanticKind.DEFINITE_END) {
                if (definite == null) {
                    definite = new FlowSemanticNodeResult(superNode, FlowSemanticKind.DEFINITE_END,
                            "SUPERNODE_INTERNAL_DEFINITE_END",
                            "The reachable internal subflow can end at node '" + safeNodeName(internalNode) + "'.");
                }
            } else if (potential == null) {
                potential = new FlowSemanticNodeResult(superNode, FlowSemanticKind.POTENTIAL_END,
                        "SUPERNODE_INTERNAL_POTENTIAL_END",
                        "The reachable internal subflow may end at node '" + safeNodeName(internalNode) + "'.");
            }
        }
        // POTENTIAL_END takes precedence over DEFINITE_END: if any reachable node
        // is a potential (not guaranteed) dead end, the supernode's classification
        // cannot be promoted to DEFINITE_END.
        if (potential != null) {
            return potential;
        }
        if (definite != null) {
            return definite;
        }
        return new FlowSemanticNodeResult(superNode, FlowSemanticKind.NOT_END,
                "SUPERNODE_INTERNAL_CONTINUATION",
                "The reachable internal subflow has no detected end node.");
    }

    private LinkedHashSet<BasicNode> collectReachableInternalNodes(SuperNode superNode) {
        LinkedHashSet<BasicNode> visited = new LinkedHashSet<>();
        ArrayDeque<BasicNode> queue = new ArrayDeque<>();
        List<BasicNode> seeds = new ArrayList<>();

        for (BasicNode startNode : superNode.getStartNodeMap().values()) {
            if (startNode != null && startNode.getParentNode() == superNode && !startNode.isHistoryNode()) {
                seeds.add(startNode);
            }
        }
        if (seeds.isEmpty()) {
            for (BasicNode child : superNode.getNodeAndSuperNodeList()) {
                if (child != null && !child.isHistoryNode()) {
                    seeds.add(child);
                }
            }
        }

        for (BasicNode seed : seeds) {
            if (visited.add(seed)) {
                queue.add(seed);
            }
        }

        while (!queue.isEmpty()) {
            BasicNode current = queue.removeFirst();
            for (AbstractEdge edge : current.getEdgeList()) {
                BasicNode target = edge != null ? edge.getTargetNode() : null;
                if (target == null || target.getParentNode() != superNode || target.isHistoryNode()) {
                    continue;
                }
                if (visited.add(target)) {
                    queue.addLast(target);
                }
            }
        }

        return visited;
    }

    private FlowSemanticNodeResult mergeSuperNodeResult(BasicNode node,
                                                        FlowSemanticNodeResult local,
                                                        FlowSemanticNodeResult internal) {
        if (local.getKind() == FlowSemanticKind.NOT_END) {
            return local;
        }
        if (internal.getKind() == FlowSemanticKind.NOT_END) {
            return internal;
        }
        if (local.getKind() == FlowSemanticKind.POTENTIAL_END || internal.getKind() == FlowSemanticKind.POTENTIAL_END) {
            return new FlowSemanticNodeResult(node, FlowSemanticKind.POTENTIAL_END,
                    internal.getReasonCode(),
                    internal.getReasonText() + " The supernode has no guaranteed outgoing continuation.");
        }
        return new FlowSemanticNodeResult(node, FlowSemanticKind.DEFINITE_END,
                internal.getReasonCode(),
                internal.getReasonText() + " The supernode has no guaranteed outgoing continuation.");
    }

    private String safeNodeName(BasicNode node) {
        String name = node.getName();
        if (name != null && !name.isBlank()) {
            return name;
        }
        String id = node.getId();
        return id != null && !id.isBlank() ? id : "<unnamed>";
    }
}
