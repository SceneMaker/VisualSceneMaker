package de.dfki.vsm.web;

import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.EpsilonEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.ForkingEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.RandomEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgeArrow;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgeGraphics;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgePoint;
import de.dfki.vsm.model.sceneflow.chart.graphics.node.NodeGraphics;
import de.dfki.vsm.model.sceneflow.chart.graphics.node.NodePosition;
import de.dfki.vsm.runtime.project.RunTimeProject;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Manages dock point allocation, edge layout geometry, and edge normalization/straightening.
 * Extracted from WebUiServer to separate edge layout concerns from WebSocket/HTTP handling.
 */
public final class EdgeLayoutService {

    // Track occupied dock points per node - each dock point can only be used by ONE edge endpoint
    private final Map<String, Set<Integer>> mOccupiedDockPoints = new ConcurrentHashMap<>();
    private final Map<String, EdgePairSplit> mEdgePairSplits = new ConcurrentHashMap<>();

    // Dock point index for start sign (left side of node)
    static final int START_SIGN_DOCK_INDEX = 6;

    // --- Dock point geometry ---

    double[][] computeDockPoints(int nodeWidth, int nodeHeight, boolean isSuperNode) {
        double[][] dockPoints = new double[24][2];
        double halfW = nodeWidth / 2.0;
        double halfH = nodeHeight / 2.0;

        for (int i = 0; i < 24; i++) {
            double angle = i * Math.PI / 12.0 + Math.PI;
            if (isSuperNode) {
                double xa = Math.sin(angle);
                double ya = Math.cos(angle);
                double dockX, dockY;
                if (Math.abs(xa) <= Math.abs(ya)) {
                    double fy = 1.0 / Math.abs(ya);
                    dockY = halfH * Math.signum(ya) + halfH;
                    dockX = Math.round(xa * fy * halfW) + halfW;
                } else {
                    double fx = 1.0 / Math.abs(xa);
                    dockY = Math.round(ya * fx * halfH) + halfH;
                    dockX = halfW * Math.signum(xa) + halfW;
                }
                dockPoints[i][0] = dockX;
                dockPoints[i][1] = dockY;
            } else {
                double dockX = Math.round((Math.sin(angle) * 0.5 + 0.5) * nodeWidth);
                double dockY = Math.round((Math.cos(angle) * 0.5 + 0.5) * nodeHeight);
                dockPoints[i][0] = dockX;
                dockPoints[i][1] = dockY;
            }
        }
        return dockPoints;
    }

    double[] getDockPointPosition(double nodeX, double nodeY, int nodeWidth, int nodeHeight,
                                   boolean isSuperNode, int dockIndex) {
        double[][] dockPoints = computeDockPoints(nodeWidth, nodeHeight, isSuperNode);
        if (dockIndex >= 0 && dockIndex < dockPoints.length) {
            return new double[] { nodeX + dockPoints[dockIndex][0], nodeY + dockPoints[dockIndex][1] };
        }
        return new double[] { nodeX + nodeWidth / 2.0, nodeY + nodeHeight / 2.0 };
    }

    int findDockPointIndex(double nodeX, double nodeY, int nodeWidth, int nodeHeight,
                            boolean isSuperNode, double pointX, double pointY) {
        double[][] dockPoints = computeDockPoints(nodeWidth, nodeHeight, isSuperNode);
        int bestIdx = -1;
        double bestDist = Double.MAX_VALUE;

        for (int i = 0; i < dockPoints.length; i++) {
            double dockAbsX = nodeX + dockPoints[i][0];
            double dockAbsY = nodeY + dockPoints[i][1];
            double dist = Math.hypot(dockAbsX - pointX, dockAbsY - pointY);
            if (dist < bestDist) {
                bestDist = dist;
                bestIdx = i;
            }
        }

        return bestDist < 20 ? bestIdx : -1;
    }

    // --- Dock point selection ---

    int[] findBestDockPointPair(
            String sourceNodeId, double srcX, double srcY, int srcWidth, int srcHeight, boolean srcIsSuperNode,
            String targetNodeId, double tgtX, double tgtY, int tgtWidth, int tgtHeight, boolean tgtIsSuperNode) {

        if (sourceNodeId != null && sourceNodeId.equals(targetNodeId)) {
            return findSelfLoopDockPointPair(sourceNodeId, srcWidth, srcHeight, srcIsSuperNode);
        }

        double[][] srcDockPoints = computeDockPoints(srcWidth, srcHeight, srcIsSuperNode);
        double[][] tgtDockPoints = computeDockPoints(tgtWidth, tgtHeight, tgtIsSuperNode);

        Set<Integer> srcOccupied = mOccupiedDockPoints.computeIfAbsent(sourceNodeId, k -> ConcurrentHashMap.newKeySet());
        Set<Integer> tgtOccupied = mOccupiedDockPoints.computeIfAbsent(targetNodeId, k -> ConcurrentHashMap.newKeySet());

        int bestSrcIdx = -1;
        int bestTgtIdx = -1;
        double bestDist = Double.MAX_VALUE;

        double srcCenterX = srcX + srcWidth / 2.0;
        double srcCenterY = srcY + srcHeight / 2.0;
        double tgtCenterX = tgtX + tgtWidth / 2.0;
        double tgtCenterY = tgtY + tgtHeight / 2.0;

        EdgePairSplit split = getPairSplit(sourceNodeId, targetNodeId);
        Integer dirSign = split != null ? split.getDirSign(sourceNodeId, targetNodeId) : null;

        for (int pass = 0; pass < 2; pass++) {
            boolean enforceSplit = pass == 0 && dirSign != null && split != null;
            bestDist = Double.MAX_VALUE;
            bestSrcIdx = -1;
            bestTgtIdx = -1;
            for (int s = 0; s < 24; s++) {
                if (srcOccupied.contains(s)) continue;
                double srcDockAbsX = srcX + srcDockPoints[s][0];
                double srcDockAbsY = srcY + srcDockPoints[s][1];
                if (enforceSplit && !split.matchesSide(srcDockAbsX, srcDockAbsY, srcCenterX, srcCenterY, dirSign)) {
                    continue;
                }
                for (int t = 0; t < 24; t++) {
                    if (tgtOccupied.contains(t)) continue;
                    double tgtDockAbsX = tgtX + tgtDockPoints[t][0];
                    double tgtDockAbsY = tgtY + tgtDockPoints[t][1];
                    if (enforceSplit && !split.matchesSide(tgtDockAbsX, tgtDockAbsY, tgtCenterX, tgtCenterY, dirSign)) {
                        continue;
                    }
                    double dist = Math.hypot(tgtDockAbsX - srcDockAbsX, tgtDockAbsY - srcDockAbsY);
                    if (dist < bestDist) {
                        bestDist = dist;
                        bestSrcIdx = s;
                        bestTgtIdx = t;
                    }
                }
            }
            if (bestSrcIdx >= 0 && bestTgtIdx >= 0) {
                break;
            }
        }

        if (bestSrcIdx < 0) bestSrcIdx = 0;
        if (bestTgtIdx < 0) bestTgtIdx = 0;

        double[] srcDockAbs = new double[] { srcX + srcDockPoints[bestSrcIdx][0], srcY + srcDockPoints[bestSrcIdx][1] };
        double[] tgtDockAbs = new double[] { tgtX + tgtDockPoints[bestTgtIdx][0], tgtY + tgtDockPoints[bestTgtIdx][1] };
        recordDockPairSplit(sourceNodeId, targetNodeId,
                srcCenterX, srcCenterY, tgtCenterX, tgtCenterY, srcDockAbs, tgtDockAbs);
        return new int[] { bestSrcIdx, bestTgtIdx };
    }

    int[] findSelfLoopDockPointPair(String nodeId, int nodeWidth, int nodeHeight, boolean isSuperNode) {
        Set<Integer> occupied = mOccupiedDockPoints.computeIfAbsent(nodeId, k -> ConcurrentHashMap.newKeySet());

        int[][] preferredPairs = {
            {21, 3}, {22, 2}, {20, 4}, {23, 1}, {19, 5}, {3, 21},
        };

        for (int[] pair : preferredPairs) {
            int startIdx = pair[0];
            int endIdx = pair[1];
            if (!occupied.contains(startIdx) && !occupied.contains(endIdx)) {
                return new int[] { startIdx, endIdx };
            }
        }

        for (int s = 19; s <= 23; s++) {
            if (occupied.contains(s)) continue;
            for (int offset = 6; offset >= 4; offset--) {
                int t = (s + offset) % 24;
                if (!occupied.contains(t) && s != t) {
                    return new int[] { s, t };
                }
            }
        }

        return new int[] { 21, 3 };
    }

    // --- Dock point occupation/release ---

    void occupyDockPoint(String nodeId, int dockIndex, boolean isSource) {
        Set<Integer> set = mOccupiedDockPoints.computeIfAbsent(nodeId, k -> ConcurrentHashMap.newKeySet());
        set.add(dockIndex);
    }

    void releaseDockPoint(String nodeId, int dockIndex, boolean isSource) {
        Set<Integer> occupied = mOccupiedDockPoints.get(nodeId);
        if (occupied != null) {
            occupied.remove(dockIndex);
        }
    }

    void occupyStartSignDockPoint(String nodeId) {
        mOccupiedDockPoints.computeIfAbsent(nodeId, k -> ConcurrentHashMap.newKeySet()).add(START_SIGN_DOCK_INDEX);
    }

    void releaseStartSignDockPoint(String nodeId) {
        Set<Integer> occupied = mOccupiedDockPoints.get(nodeId);
        if (occupied != null) {
            occupied.remove(START_SIGN_DOCK_INDEX);
        }
    }

    void occupyStartSignDockPointsRecursive(SuperNode superNode) {
        if (superNode == null) return;
        Map<String, BasicNode> startNodeMap = superNode.getStartNodeMap();
        if (startNodeMap != null) {
            for (BasicNode startNode : startNodeMap.values()) {
                if (startNode != null) {
                    occupyStartSignDockPoint(startNode.getId());
                }
            }
        }
        for (BasicNode node : superNode.getSuperNodeList()) {
            if (node instanceof SuperNode) {
                occupyStartSignDockPointsRecursive((SuperNode) node);
            }
        }
    }

    // --- Edge dock point lifecycle ---

    void initializeEdgeDockPoints(AbstractEdge edge, int nodeWidth, int nodeHeight) {
        if (edge == null) return;
        BasicNode sourceNode = edge.getSourceNode();
        BasicNode targetNode = edge.getTargetNode();
        if (sourceNode == null || targetNode == null) return;

        NodeGraphics srcGraphics = sourceNode.getGraphics();
        NodeGraphics tgtGraphics = targetNode.getGraphics();
        NodePosition srcPos = srcGraphics != null ? srcGraphics.getPosition() : null;
        NodePosition tgtPos = tgtGraphics != null ? tgtGraphics.getPosition() : null;
        double srcX = srcPos != null ? srcPos.getXPos() : 0;
        double srcY = srcPos != null ? srcPos.getYPos() : 0;
        double tgtX = tgtPos != null ? tgtPos.getXPos() : 0;
        double tgtY = tgtPos != null ? tgtPos.getYPos() : 0;
        boolean srcIsSuper = sourceNode instanceof SuperNode;
        boolean tgtIsSuper = targetNode instanceof SuperNode;
        boolean isSelfLoop = sourceNode.getId().equals(targetNode.getId());

        int[] dockPair = isSelfLoop
                ? findSelfLoopDockPointPair(sourceNode.getId(), nodeWidth, nodeHeight, srcIsSuper)
                : findBestDockPointPair(
                        sourceNode.getId(), srcX, srcY, nodeWidth, nodeHeight, srcIsSuper,
                        targetNode.getId(), tgtX, tgtY, nodeWidth, nodeHeight, tgtIsSuper
                );

        int srcDockIdx = dockPair[0];
        int tgtDockIdx = dockPair[1];
        occupyDockPoint(sourceNode.getId(), srcDockIdx, true);
        occupyDockPoint(targetNode.getId(), tgtDockIdx, false);

        double[] srcDock = getDockPointPosition(srcX, srcY, nodeWidth, nodeHeight, srcIsSuper, srcDockIdx);
        double[] tgtDock = getDockPointPosition(tgtX, tgtY, nodeWidth, nodeHeight, tgtIsSuper, tgtDockIdx);

        EdgeGraphics graphics = edge.getGraphics();
        if (graphics == null) {
            graphics = new EdgeGraphics();
            edge.setGraphics(graphics);
        }
        EdgeArrow arrow = graphics.getConnection();
        if (arrow == null) {
            arrow = new EdgeArrow();
            graphics.setConnection(arrow);
        }
        ArrayList<EdgePoint> points = new ArrayList<>();
        points.add(new EdgePoint(
                (int) Math.round(srcDock[0]),
                (int) Math.round(srcDock[0]),
                (int) Math.round(srcDock[1]),
                (int) Math.round(srcDock[1])
        ));
        points.add(new EdgePoint(
                (int) Math.round(tgtDock[0]),
                (int) Math.round(tgtDock[0]),
                (int) Math.round(tgtDock[1]),
                (int) Math.round(tgtDock[1])
        ));
        arrow.setPointList(points);
    }

    void releaseEdgeDockPoints(AbstractEdge edge, int nodeWidth, int nodeHeight) {
        if (edge == null) return;
        BasicNode sourceNode = edge.getSourceNode();
        BasicNode targetNode = edge.getTargetNode();
        if (sourceNode == null || targetNode == null) return;
        EdgeGraphics edgeGraphics = edge.getGraphics();
        if (edgeGraphics == null || edgeGraphics.getConnection() == null) return;
        List<EdgePoint> points = edgeGraphics.getConnection().getPointList();
        if (points == null || points.size() < 2) return;
        EdgePoint startPt = points.get(0);
        EdgePoint endPt = points.get(points.size() - 1);
        NodeGraphics srcGraphics = sourceNode.getGraphics();
        NodeGraphics tgtGraphics = targetNode.getGraphics();
        NodePosition srcPos = srcGraphics != null ? srcGraphics.getPosition() : null;
        NodePosition tgtPos = tgtGraphics != null ? tgtGraphics.getPosition() : null;
        double srcX = srcPos != null ? srcPos.getXPos() : 0;
        double srcY = srcPos != null ? srcPos.getYPos() : 0;
        double tgtX = tgtPos != null ? tgtPos.getXPos() : 0;
        double tgtY = tgtPos != null ? tgtPos.getYPos() : 0;
        int srcDockIdx = findDockPointIndex(srcX, srcY, nodeWidth, nodeHeight,
                sourceNode instanceof SuperNode, startPt.getXPos(), startPt.getYPos());
        if (srcDockIdx >= 0) {
            releaseDockPoint(sourceNode.getId(), srcDockIdx, true);
        }
        int tgtDockIdx = findDockPointIndex(tgtX, tgtY, nodeWidth, nodeHeight,
                targetNode instanceof SuperNode, endPt.getXPos(), endPt.getYPos());
        if (tgtDockIdx >= 0) {
            releaseDockPoint(targetNode.getId(), tgtDockIdx, false);
        }
    }

    // --- Project lifecycle ---

    void initializeDockPointsForProject(RunTimeProject project, int nodeWidth, int nodeHeight) {
        if (project == null) return;
        SceneFlow sceneFlow = project.getSceneFlow();
        if (sceneFlow == null) return;
        initializeDockPointsRecursive(sceneFlow, nodeWidth, nodeHeight);
    }

    void initializeDockPointsRecursive(SuperNode superNode, int nodeWidth, int nodeHeight) {
        List<BasicNode> allNodes = new ArrayList<>();
        allNodes.addAll(superNode.getNodeList());
        allNodes.addAll(superNode.getSuperNodeList());

        Map<String, BasicNode> startNodeMap = superNode.getStartNodeMap();
        if (startNodeMap != null) {
            for (BasicNode startNode : startNodeMap.values()) {
                if (startNode != null) {
                    occupyStartSignDockPoint(startNode.getId());
                }
            }
        }

        for (BasicNode node : allNodes) {
            processNodeEdgesForDocking(node, nodeWidth, nodeHeight);
            if (node instanceof SuperNode) {
                initializeDockPointsRecursive((SuperNode) node, nodeWidth, nodeHeight);
            }
        }
    }

    void processNodeEdgesForDocking(BasicNode node, int nodeWidth, int nodeHeight) {
        NodeGraphics nodeGraphics = node.getGraphics();
        NodePosition nodePos = nodeGraphics != null ? nodeGraphics.getPosition() : null;
        double nodeX = nodePos != null ? nodePos.getXPos() : 0;
        double nodeY = nodePos != null ? nodePos.getYPos() : 0;
        double nodeCenterX = nodeX + nodeWidth / 2.0;
        double nodeCenterY = nodeY + nodeHeight / 2.0;
        boolean isSuperNode = node instanceof SuperNode;

        List<AbstractEdge> edges = new ArrayList<>();
        if (node.getDedge() != null) edges.add(node.getDedge());
        edges.addAll(node.getCEdgeList());
        edges.addAll(node.getIEdgeList());
        edges.addAll(node.getPEdgeList());
        edges.addAll(node.getFEdgeList());

        for (AbstractEdge edge : edges) {
            EdgeGraphics edgeGraphics = edge.getGraphics();
            if (edgeGraphics == null || edgeGraphics.getConnection() == null) continue;

            List<EdgePoint> points = edgeGraphics.getConnection().getPointList();
            if (points == null || points.isEmpty()) continue;

            EdgePoint startPt = points.get(0);
            int srcDockIdx = findDockPointIndex(nodeX, nodeY, nodeWidth, nodeHeight,
                    isSuperNode, startPt.getXPos(), startPt.getYPos());
            if (srcDockIdx >= 0) {
                occupyDockPoint(node.getId(), srcDockIdx, true);
            }

            if (points.size() >= 2) {
                EdgePoint endPt = points.get(points.size() - 1);
                BasicNode targetNode = edge.getTargetNode();
                if (targetNode != null) {
                    NodeGraphics tgtGraphics = targetNode.getGraphics();
                    NodePosition tgtPos = tgtGraphics != null ? tgtGraphics.getPosition() : null;
                    double tgtX = tgtPos != null ? tgtPos.getXPos() : 0;
                    double tgtY = tgtPos != null ? tgtPos.getYPos() : 0;
                    boolean tgtIsSuperNode = targetNode instanceof SuperNode;

                    int tgtDockIdx = findDockPointIndex(tgtX, tgtY, nodeWidth, nodeHeight,
                            tgtIsSuperNode, endPt.getXPos(), endPt.getYPos());
                    if (tgtDockIdx >= 0) {
                        occupyDockPoint(targetNode.getId(), tgtDockIdx, false);
                    }
                    if (srcDockIdx >= 0 && tgtDockIdx >= 0) {
                        double tgtCenterX = tgtX + nodeWidth / 2.0;
                        double tgtCenterY = tgtY + nodeHeight / 2.0;
                        double[] srcDockAbs = new double[] { startPt.getXPos(), startPt.getYPos() };
                        double[] tgtDockAbs = new double[] { endPt.getXPos(), endPt.getYPos() };
                        recordDockPairSplit(edge.getSourceUnid(), edge.getTargetUnid(),
                                nodeCenterX, nodeCenterY, tgtCenterX, tgtCenterY, srcDockAbs, tgtDockAbs);
                    }
                }
            }
        }
    }

    void clearDockPointsForProject(SceneFlow sceneFlow) {
        if (sceneFlow == null) return;
        clearDockPointsRecursive(sceneFlow);
    }

    void clearDockPointsRecursive(SuperNode superNode) {
        mEdgePairSplits.clear();
        mOccupiedDockPoints.remove(superNode.getId());

        for (BasicNode node : superNode.getNodeList()) {
            mOccupiedDockPoints.remove(node.getId());
        }

        for (SuperNode child : superNode.getSuperNodeList()) {
            mOccupiedDockPoints.remove(child.getId());
            clearDockPointsRecursive(child);
        }
    }

    // --- Reassignment/relayout ---

    void reassignDockPointsRecursive(SuperNode superNode, int nodeWidth, int nodeHeight) {
        if (superNode == null) return;
        List<AbstractEdge> edges = new ArrayList<>();
        Set<AbstractEdge> seen = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap<>());
        collectEdgesRecursive(superNode, edges, seen);
        edges.sort((a, b) -> {
            String aSrc = a != null ? String.valueOf(a.getSourceUnid()) : "";
            String bSrc = b != null ? String.valueOf(b.getSourceUnid()) : "";
            int cmp = aSrc.compareTo(bSrc);
            if (cmp != 0) return cmp;
            String aTgt = a != null ? String.valueOf(a.getTargetUnid()) : "";
            String bTgt = b != null ? String.valueOf(b.getTargetUnid()) : "";
            cmp = aTgt.compareTo(bTgt);
            if (cmp != 0) return cmp;
            String aType = a != null ? String.valueOf(getEdgeType(a)) : "";
            String bType = b != null ? String.valueOf(getEdgeType(b)) : "";
            cmp = aType.compareTo(bType);
            if (cmp != 0) return cmp;
            return Integer.compare(System.identityHashCode(a), System.identityHashCode(b));
        });
        for (AbstractEdge edge : edges) {
            reassignDockPointsForEdge(edge, nodeWidth, nodeHeight);
        }
    }

    void collectEdgesRecursive(SuperNode superNode, List<AbstractEdge> out, Set<AbstractEdge> seen) {
        if (superNode == null) return;
        List<BasicNode> allNodes = new ArrayList<>();
        allNodes.addAll(superNode.getNodeList());
        allNodes.addAll(superNode.getSuperNodeList());
        for (BasicNode node : allNodes) {
            for (AbstractEdge edge : node.getEdgeList()) {
                if (edge != null && seen.add(edge)) {
                    out.add(edge);
                }
            }
            if (node instanceof SuperNode) {
                collectEdgesRecursive((SuperNode) node, out, seen);
            }
        }
    }

    void reassignDockPointsForEdge(AbstractEdge edge, int nodeWidth, int nodeHeight) {
        if (edge == null) return;
        releaseEdgeDockPoints(edge, nodeWidth, nodeHeight);
        initializeEdgeDockPoints(edge, nodeWidth, nodeHeight);
    }

    void relayoutEdgesInOrder(List<AbstractEdge> edges, int nodeWidth, int nodeHeight) {
        if (edges == null || edges.isEmpty()) return;
        for (AbstractEdge edge : edges) {
            releaseEdgeDockPoints(edge, nodeWidth, nodeHeight);
        }
        for (AbstractEdge edge : edges) {
            initializeEdgeDockPoints(edge, nodeWidth, nodeHeight);
        }
    }

    void reassignDockPointsForStraighten(SuperNode superNode, int nodeWidth, int nodeHeight) {
        if (superNode == null) return;
        List<AbstractEdge> edges = new ArrayList<>();
        Set<AbstractEdge> seen = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap<>());
        collectEdgesRecursive(superNode, edges, seen);
        Map<String, List<AbstractEdge>> pairs = new java.util.TreeMap<>();
        for (AbstractEdge edge : edges) {
            if (edge == null) continue;
            String src = edge.getSourceUnid();
            String tgt = edge.getTargetUnid();
            if (src == null || tgt == null) continue;
            String key = src.compareTo(tgt) <= 0 ? src + "|" + tgt : tgt + "|" + src;
            pairs.computeIfAbsent(key, k -> new ArrayList<>()).add(edge);
        }
        for (Map.Entry<String, List<AbstractEdge>> entry : pairs.entrySet()) {
            String[] parts = entry.getKey().split("\\|", 2);
            if (parts.length != 2) continue;
            reassignDockPointsForPair(superNode, parts[0], parts[1], nodeWidth, nodeHeight);
        }
    }

    void reassignDockPointsForPair(SuperNode superNode, String sourceId, String targetId, int nodeWidth, int nodeHeight) {
        if (superNode == null || sourceId == null || targetId == null) return;
        List<AbstractEdge> edges = new ArrayList<>();
        Set<AbstractEdge> seen = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap<>());
        collectEdgesRecursive(superNode, edges, seen);
        List<AbstractEdge> group = new ArrayList<>();
        for (AbstractEdge edge : edges) {
            if (edge == null) continue;
            String src = edge.getSourceUnid();
            String tgt = edge.getTargetUnid();
            if (src == null || tgt == null) continue;
            boolean same = (src.equals(sourceId) && tgt.equals(targetId)) || (src.equals(targetId) && tgt.equals(sourceId));
            if (same) {
                group.add(edge);
            }
        }
        if (group.isEmpty()) return;
        for (AbstractEdge edge : group) {
            releaseEdgeDockPoints(edge, nodeWidth, nodeHeight);
        }

        BasicNode nodeA = resolveNodeById(superNode, sourceId);
        BasicNode nodeB = resolveNodeById(superNode, targetId);
        if (nodeA == null || nodeB == null) return;
        boolean sameNode = nodeA.getId().equals(nodeB.getId());
        if (sameNode) {
            for (AbstractEdge edge : group) {
                initializeEdgeDockPoints(edge, nodeWidth, nodeHeight);
            }
            return;
        }

        NodeGraphics aGraphics = nodeA.getGraphics();
        NodeGraphics bGraphics = nodeB.getGraphics();
        NodePosition aPos = aGraphics != null ? aGraphics.getPosition() : null;
        NodePosition bPos = bGraphics != null ? bGraphics.getPosition() : null;
        double aX = aPos != null ? aPos.getXPos() : 0;
        double aY = aPos != null ? aPos.getYPos() : 0;
        double bX = bPos != null ? bPos.getXPos() : 0;
        double bY = bPos != null ? bPos.getYPos() : 0;
        boolean aSuper = nodeA instanceof SuperNode;
        boolean bSuper = nodeB instanceof SuperNode;

        double[][] aDockPoints = computeDockPoints(nodeWidth, nodeHeight, aSuper);
        double[][] bDockPoints = computeDockPoints(nodeWidth, nodeHeight, bSuper);

        int bestASrc = 0;
        int bestBTgt = 0;
        double bestDist = Double.MAX_VALUE;
        for (int s = 0; s < 24; s++) {
            double sx = aX + aDockPoints[s][0];
            double sy = aY + aDockPoints[s][1];
            for (int t = 0; t < 24; t++) {
                double tx = bX + bDockPoints[t][0];
                double ty = bY + bDockPoints[t][1];
                double dist = Math.hypot(tx - sx, ty - sy);
                if (dist < bestDist) {
                    bestDist = dist;
                    bestASrc = s;
                    bestBTgt = t;
                }
            }
        }

        final int anchorA = bestASrc;
        final int anchorB = bestBTgt;

        List<Integer> aCandidates = new ArrayList<>();
        List<Integer> bCandidates = new ArrayList<>();
        for (int i = 0; i < 24; i++) {
            aCandidates.add(i);
            bCandidates.add(i);
        }
        aCandidates.sort((i, j) -> {
            double dxI = aDockPoints[i][0] - aDockPoints[anchorA][0];
            double dyI = aDockPoints[i][1] - aDockPoints[anchorA][1];
            double dxJ = aDockPoints[j][0] - aDockPoints[anchorA][0];
            double dyJ = aDockPoints[j][1] - aDockPoints[anchorA][1];
            return Double.compare(dxI * dxI + dyI * dyI, dxJ * dxJ + dyJ * dyJ);
        });
        bCandidates.sort((i, j) -> {
            double dxI = bDockPoints[i][0] - bDockPoints[anchorB][0];
            double dyI = bDockPoints[i][1] - bDockPoints[anchorB][1];
            double dxJ = bDockPoints[j][0] - bDockPoints[anchorB][0];
            double dyJ = bDockPoints[j][1] - bDockPoints[anchorB][1];
            return Double.compare(dxI * dxI + dyI * dyI, dxJ * dxJ + dyJ * dyJ);
        });

        aCandidates = uniqueDockCandidates(aCandidates, aDockPoints);
        bCandidates = uniqueDockCandidates(bCandidates, bDockPoints);

        Set<Integer> aUsed = new java.util.HashSet<>();
        Set<Integer> bUsed = new java.util.HashSet<>();
        Set<String> aUsedCoords = new java.util.HashSet<>();
        Set<String> bUsedCoords = new java.util.HashSet<>();
        Set<Integer> aOccupied = mOccupiedDockPoints.computeIfAbsent(nodeA.getId(), k -> ConcurrentHashMap.newKeySet());
        Set<Integer> bOccupied = mOccupiedDockPoints.computeIfAbsent(nodeB.getId(), k -> ConcurrentHashMap.newKeySet());

        group.sort((a, b) -> {
            String aType = a != null ? String.valueOf(getEdgeType(a)) : "";
            String bType = b != null ? String.valueOf(getEdgeType(b)) : "";
            int cmp = aType.compareTo(bType);
            if (cmp != 0) return cmp;
            return Integer.compare(System.identityHashCode(a), System.identityHashCode(b));
        });

        for (AbstractEdge edge : group) {
            if (edge == null) continue;
            boolean fromA = nodeA.getId().equals(edge.getSourceUnid());
            List<Integer> srcCandidates = fromA ? aCandidates : bCandidates;
            List<Integer> tgtCandidates = fromA ? bCandidates : aCandidates;
            Set<Integer> srcUsed = fromA ? aUsed : bUsed;
            Set<Integer> tgtUsed = fromA ? bUsed : aUsed;
            Set<Integer> srcOccupied = fromA ? aOccupied : bOccupied;
            Set<Integer> tgtOccupied = fromA ? bOccupied : aOccupied;
            double[][] srcDockPoints = fromA ? aDockPoints : bDockPoints;
            double[][] tgtDockPoints = fromA ? bDockPoints : aDockPoints;
            Set<String> srcUsedCoords = fromA ? aUsedCoords : bUsedCoords;
            Set<String> tgtUsedCoords = fromA ? bUsedCoords : aUsedCoords;

            int srcIdx = nextAvailableDockIndexWithCoords(
                    srcCandidates, srcDockPoints, srcUsed, srcUsedCoords, srcOccupied, 0);
            int tgtIdx = nextAvailableDockIndexWithCoords(
                    tgtCandidates, tgtDockPoints, tgtUsed, tgtUsedCoords, tgtOccupied, 0);

            srcUsed.add(srcIdx);
            tgtUsed.add(tgtIdx);
            srcOccupied.add(srcIdx);
            tgtOccupied.add(tgtIdx);
            srcUsedCoords.add(coordKey(srcDockPoints[srcIdx]));
            tgtUsedCoords.add(coordKey(tgtDockPoints[tgtIdx]));

            double[] srcDock = fromA
                    ? new double[] { aX + aDockPoints[srcIdx][0], aY + aDockPoints[srcIdx][1] }
                    : new double[] { bX + bDockPoints[srcIdx][0], bY + bDockPoints[srcIdx][1] };
            double[] tgtDock = fromA
                    ? new double[] { bX + bDockPoints[tgtIdx][0], bY + bDockPoints[tgtIdx][1] }
                    : new double[] { aX + aDockPoints[tgtIdx][0], aY + aDockPoints[tgtIdx][1] };

            EdgeGraphics graphics = edge.getGraphics();
            if (graphics == null) {
                graphics = new EdgeGraphics();
                edge.setGraphics(graphics);
            }
            EdgeArrow arrow = graphics.getConnection();
            if (arrow == null) {
                arrow = new EdgeArrow();
                graphics.setConnection(arrow);
            }
            ArrayList<EdgePoint> points = new ArrayList<>();
            points.add(new EdgePoint(
                    (int) Math.round(srcDock[0]),
                    (int) Math.round(srcDock[0]),
                    (int) Math.round(srcDock[1]),
                    (int) Math.round(srcDock[1])
            ));
            points.add(new EdgePoint(
                    (int) Math.round(tgtDock[0]),
                    (int) Math.round(tgtDock[0]),
                    (int) Math.round(tgtDock[1]),
                    (int) Math.round(tgtDock[1])
            ));
            arrow.setPointList(points);
        }
    }

    // --- Edge normalization/straightening ---

    void normalizeEdge(AbstractEdge edge, int nodeWidth, int nodeHeight) {
        if (edge == null) return;
        EdgeGraphics graphics = edge.getGraphics();
        if (graphics == null) {
            graphics = new EdgeGraphics();
            edge.setGraphics(graphics);
        }
        EdgeArrow arrow = graphics.getConnection();
        if (arrow == null) {
            arrow = new EdgeArrow();
            graphics.setConnection(arrow);
        }
        List<EdgePoint> points = arrow.getPointList();
        if (points == null || points.size() < 2) return;

        EdgePoint startPt = points.get(0);
        EdgePoint endPt = points.get(points.size() - 1);
        double startX = startPt.getXPos();
        double startY = startPt.getYPos();
        double endX = endPt.getXPos();
        double endY = endPt.getYPos();

        String sourceId = edge.getSourceUnid();
        String targetId = edge.getTargetUnid();
        boolean isSelfLoop = sourceId != null && sourceId.equals(targetId);

        if (isSelfLoop) {
            BasicNode sourceNode = edge.getSourceNode();
            if (sourceNode != null) {
                NodeGraphics nodeGraphics = sourceNode.getGraphics();
                NodePosition nodePos = nodeGraphics != null ? nodeGraphics.getPosition() : null;
                double nodeX = nodePos != null ? nodePos.getXPos() : 0;
                double nodeY = nodePos != null ? nodePos.getYPos() : 0;
                double nodeSize = Math.max(nodeWidth, nodeHeight);

                double startCtrlX = startX + 0.85 * nodeSize;
                double startCtrlY = startY - 0.76 * nodeSize;
                double endCtrlX = endX + 0.16 * nodeSize;
                double endCtrlY = endY - 0.87 * nodeSize;

                startPt.setCtrlXPos((int) Math.round(startCtrlX));
                startPt.setCtrlYPos((int) Math.round(startCtrlY));
                endPt.setCtrlXPos((int) Math.round(endCtrlX));
                endPt.setCtrlYPos((int) Math.round(endCtrlY));
            }
        } else {
            BasicNode sourceNode = edge.getSourceNode();
            BasicNode targetNode = edge.getTargetNode();
            if (sourceNode != null && targetNode != null) {
                NodeGraphics srcGraphics = sourceNode.getGraphics();
                NodeGraphics tgtGraphics = targetNode.getGraphics();
                NodePosition srcPos = srcGraphics != null ? srcGraphics.getPosition() : null;
                NodePosition tgtPos = tgtGraphics != null ? tgtGraphics.getPosition() : null;
                double srcX = srcPos != null ? srcPos.getXPos() : 0;
                double srcY = srcPos != null ? srcPos.getYPos() : 0;
                double tgtX = tgtPos != null ? tgtPos.getXPos() : 0;
                double tgtY = tgtPos != null ? tgtPos.getYPos() : 0;
                double srcCenterX = srcX + nodeWidth / 2.0;
                double srcCenterY = srcY + nodeHeight / 2.0;
                double tgtCenterX = tgtX + nodeWidth / 2.0;
                double tgtCenterY = tgtY + nodeHeight / 2.0;

                double[] ctrl = computeNormalizedControlPoints(
                        startX, startY, srcCenterX, srcCenterY,
                        endX, endY, tgtCenterX, tgtCenterY, nodeHeight);
                startPt.setCtrlXPos((int) Math.round(ctrl[0]));
                startPt.setCtrlYPos((int) Math.round(ctrl[1]));
                endPt.setCtrlXPos((int) Math.round(ctrl[2]));
                endPt.setCtrlYPos((int) Math.round(ctrl[3]));
            }
        }
    }

    double[] computeNormalizedControlPoints(
            double srcDockX, double srcDockY, double srcCenterX, double srcCenterY,
            double tgtDockX, double tgtDockY, double tgtCenterX, double tgtCenterY,
            double nodeHeight) {
        double srcVecX = srcDockX - srcCenterX;
        double srcVecY = srcDockY - srcCenterY;
        double tgtVecX = tgtDockX - tgtCenterX;
        double tgtVecY = tgtDockY - tgtCenterY;

        double distance = Math.hypot(tgtCenterX - srcCenterX, tgtCenterY - srcCenterY);
        double scalingFactor = (distance / nodeHeight) - 0.5;
        if (scalingFactor < 1.25) {
            scalingFactor = 1.25;
        }

        double ctrl1X = Math.max(15, srcCenterX + scalingFactor * srcVecX);
        double ctrl1Y = Math.max(15, srcCenterY + scalingFactor * srcVecY);
        double ctrl2X = Math.max(15, tgtCenterX + scalingFactor * tgtVecX);
        double ctrl2Y = Math.max(15, tgtCenterY + scalingFactor * tgtVecY);

        return new double[] { ctrl1X, ctrl1Y, ctrl2X, ctrl2Y };
    }

    void straightenEdge(AbstractEdge edge) {
        if (edge == null) return;

        String sourceId = edge.getSourceUnid();
        String targetId = edge.getTargetUnid();
        if (sourceId != null && sourceId.equals(targetId)) {
            return;
        }

        BasicNode targetNode = edge.getTargetNode();
        if (targetNode != null) {
            for (AbstractEdge reverseEdge : targetNode.getEdgeList()) {
                String revTargetId = reverseEdge.getTargetUnid();
                if (revTargetId == null && reverseEdge.getTargetNode() != null) {
                    revTargetId = reverseEdge.getTargetNode().getId();
                }
                if (sourceId != null && sourceId.equals(revTargetId)) {
                    return;
                }
            }
        }

        EdgeGraphics graphics = edge.getGraphics();
        if (graphics == null) return;
        EdgeArrow arrow = graphics.getConnection();
        if (arrow == null) return;
        List<EdgePoint> points = arrow.getPointList();
        if (points == null || points.size() < 2) return;

        EdgePoint startPt = points.get(0);
        EdgePoint endPt = points.get(points.size() - 1);
        double startX = startPt.getXPos();
        double startY = startPt.getYPos();
        double endX = endPt.getXPos();
        double endY = endPt.getYPos();

        double dx = endX - startX;
        double dy = endY - startY;
        double dist = Math.hypot(dx, dy);
        double offset = Math.max(30, dist / 3);

        double ux = dist > 0 ? dx / dist : 1;
        double uy = dist > 0 ? dy / dist : 0;

        startPt.setCtrlXPos((int) Math.round(startX + ux * offset));
        startPt.setCtrlYPos((int) Math.round(startY + uy * offset));
        endPt.setCtrlXPos((int) Math.round(endX - ux * offset));
        endPt.setCtrlYPos((int) Math.round(endY - uy * offset));
    }

    double[] computeInitialControlPoint(double startX, double startY, double endX, double endY, boolean isStart) {
        double dx = endX - startX;
        double dy = endY - startY;
        double dist = Math.hypot(dx, dy);
        double offset = Math.max(30, dist / 3);

        if (isStart) {
            double ux = dist > 0 ? dx / dist : 1;
            double uy = dist > 0 ? dy / dist : 0;
            return new double[] { startX + ux * offset, startY + uy * offset };
        } else {
            double ux = dist > 0 ? -dx / dist : -1;
            double uy = dist > 0 ? -dy / dist : 0;
            return new double[] { endX + ux * offset, endY + uy * offset };
        }
    }

    double[] computeSelfLoopControlPoints(
            double startX, double startY, double endX, double endY,
            double nodeCenterX, double nodeCenterY, int nodeWidth, int nodeHeight) {

        double nodeSize = Math.max(nodeWidth, nodeHeight);
        double startCtrlX = startX + 0.85 * nodeSize;
        double startCtrlY = startY - 0.76 * nodeSize;
        double endCtrlX = endX + 0.16 * nodeSize;
        double endCtrlY = endY - 0.87 * nodeSize;

        return new double[] { startCtrlX, startCtrlY, endCtrlX, endCtrlY };
    }

    // --- Pair split management ---

    private void recordDockPairSplit(
            String sourceNodeId, String targetNodeId,
            double srcCenterX, double srcCenterY,
            double tgtCenterX, double tgtCenterY,
            double[] srcDockAbs, double[] tgtDockAbs) {
        if (sourceNodeId == null || targetNodeId == null) return;
        EdgePairSplit split = getOrCreatePairSplit(sourceNodeId, targetNodeId, srcCenterX, srcCenterY, tgtCenterX, tgtCenterY);
        if (split == null) return;
        int sign = split.sideSign(srcDockAbs[0], srcDockAbs[1], srcCenterX, srcCenterY);
        split.setDirSign(sourceNodeId, targetNodeId, sign);
    }

    private EdgePairSplit getPairSplit(String sourceNodeId, String targetNodeId) {
        if (sourceNodeId == null || targetNodeId == null) return null;
        String key = pairKey(sourceNodeId, targetNodeId);
        return mEdgePairSplits.get(key);
    }

    private EdgePairSplit getOrCreatePairSplit(
            String sourceNodeId, String targetNodeId,
            double srcCenterX, double srcCenterY,
            double tgtCenterX, double tgtCenterY) {
        if (sourceNodeId == null || targetNodeId == null) return null;
        String key = pairKey(sourceNodeId, targetNodeId);
        EdgePairSplit existing = mEdgePairSplits.get(key);
        if (existing != null) return existing;
        double vx = tgtCenterX - srcCenterX;
        double vy = tgtCenterY - srcCenterY;
        double len = Math.hypot(vx, vy);
        if (len < 1e-6) return null;
        double nx = -vy / len;
        double ny = vx / len;
        EdgePairSplit created = new EdgePairSplit(nx, ny);
        mEdgePairSplits.put(key, created);
        return created;
    }

    private String pairKey(String a, String b) {
        return a.compareTo(b) <= 0 ? a + "|" + b : b + "|" + a;
    }

    // --- Utility ---

    private int nextAvailableDockIndex(List<Integer> candidates, Set<Integer> used, Set<Integer> occupied, int start) {
        if (candidates == null || candidates.isEmpty()) return 0;
        int size = candidates.size();
        for (int i = 0; i < size; i++) {
            int idx = candidates.get((start + i) % size);
            if (used.contains(idx) || occupied.contains(idx)) continue;
            return idx;
        }
        for (int i = 0; i < size; i++) {
            int idx = candidates.get((start + i) % size);
            if (used.contains(idx)) continue;
            return idx;
        }
        return candidates.get(start % size);
    }

    private int nextAvailableDockIndexWithCoords(
            List<Integer> candidates, double[][] dockPoints,
            Set<Integer> used, Set<String> usedCoords,
            Set<Integer> occupied, int start) {
        if (candidates == null || candidates.isEmpty()) return 0;
        int size = candidates.size();
        for (int i = 0; i < size; i++) {
            int idx = candidates.get((start + i) % size);
            if (used.contains(idx) || occupied.contains(idx)) continue;
            String key = coordKey(dockPoints[idx]);
            if (usedCoords.contains(key)) continue;
            return idx;
        }
        return candidates.get(start % size);
    }

    private String coordKey(double[] dockPoint) {
        if (dockPoint == null || dockPoint.length < 2) return "";
        return Math.round(dockPoint[0]) + "," + Math.round(dockPoint[1]);
    }

    private List<Integer> uniqueDockCandidates(List<Integer> candidates, double[][] dockPoints) {
        if (candidates == null || dockPoints == null) return candidates;
        List<Integer> unique = new ArrayList<>();
        Set<String> seen = new java.util.HashSet<>();
        for (Integer idx : candidates) {
            if (idx == null || idx < 0 || idx >= dockPoints.length) continue;
            String key = coordKey(dockPoints[idx]);
            if (seen.add(key)) {
                unique.add(idx);
            }
        }
        return unique.isEmpty() ? candidates : unique;
    }

    private String getEdgeType(AbstractEdge edge) {
        if (edge instanceof GuargedEdge) return "CEDGE";
        if (edge instanceof RandomEdge) return "PEDGE";
        if (edge instanceof InterruptEdge) return "IEDGE";
        if (edge instanceof ForkingEdge) return "FEDGE";
        if (edge instanceof TimeoutEdge) return "TEDGE";
        if (edge instanceof EpsilonEdge) return "EEDGE";
        return "EEDGE";
    }

    private BasicNode resolveNodeById(SuperNode superNode, String nodeId) {
        if (superNode == null || nodeId == null) return null;
        if (nodeId.equals(superNode.getId())) return superNode;
        for (BasicNode node : superNode.getNodeAndSuperNodeList()) {
            if (nodeId.equals(node.getId())) return node;
        }
        return null;
    }

    // --- EdgePairSplit inner class ---

    static final class EdgePairSplit {
        private final double nx;
        private final double ny;
        private final Map<String, Integer> dirSign = new ConcurrentHashMap<>();

        EdgePairSplit(double nx, double ny) {
            this.nx = nx;
            this.ny = ny;
        }

        int sideSign(double x, double y, double cx, double cy) {
            double dot = (x - cx) * nx + (y - cy) * ny;
            return dot >= 0 ? 1 : -1;
        }

        boolean matchesSide(double x, double y, double cx, double cy, int sign) {
            return sideSign(x, y, cx, cy) == sign;
        }

        Integer getDirSign(String sourceId, String targetId) {
            return dirSign.get(sourceId + "->" + targetId);
        }

        void setDirSign(String sourceId, String targetId, int sign) {
            String key = sourceId + "->" + targetId;
            if (!dirSign.containsKey(key)) {
                dirSign.put(key, sign);
                dirSign.put(targetId + "->" + sourceId, -sign);
            }
        }
    }
}
