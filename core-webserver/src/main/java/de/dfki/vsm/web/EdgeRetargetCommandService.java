package de.dfki.vsm.web;

import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.EpsilonEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.ForkingEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.RandomEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgeGraphics;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgePoint;
import de.dfki.vsm.model.sceneflow.chart.graphics.node.NodeGraphics;
import de.dfki.vsm.model.sceneflow.chart.graphics.node.NodePosition;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.json.JSONObject;

import java.util.List;
import java.util.function.Consumer;

/**
 * Handles SceneFlow.Edge.Retarget command.
 */
public final class EdgeRetargetCommandService {

    public interface Context {
        RunTimeProject runtimeProject(String projectId);

        JSONObject errorResponse(String code, String message);

        SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId);

        AbstractEdge resolveEdgeById(SuperNode root, String edgeId);

        BasicNode resolveNodeById(SuperNode root, String nodeId);

        int getEditorConfigInt(String projectId, String key, int fallback);

        int findDockPointIndex(double nodeX, double nodeY, int nodeWidth, int nodeHeight,
                               boolean isSuperNode, double pointX, double pointY);

        void releaseDockPoint(String nodeId, int dockIndex, boolean isSource);

        int[] findSelfLoopDockPointPair(String nodeId, int nodeWidth, int nodeHeight, boolean isSuperNode);

        int[] findBestDockPointPair(String sourceNodeId, double srcX, double srcY, int srcWidth, int srcHeight, boolean srcIsSuperNode,
                                    String targetNodeId, double tgtX, double tgtY, int tgtWidth, int tgtHeight, boolean tgtIsSuperNode);

        void occupyDockPoint(String nodeId, int dockIndex, boolean isSource);

        double[] getDockPointPosition(double nodeX, double nodeY, int nodeWidth, int nodeHeight,
                                      boolean isSuperNode, int dockIndex);

        double[] computeSelfLoopControlPoints(double startX, double startY, double endX, double endY,
                                              double nodeCenterX, double nodeCenterY, int nodeWidth, int nodeHeight);

        double[] computeInitialControlPoint(double startX, double startY, double endX, double endY, boolean isStart);

        JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SuperNode snapshotTarget, SceneFlow sceneFlow);

        void broadcastSceneFlowSnapshot(Consumer<String> broadcaster, String projectId, JSONObject snapshot);

        void recordHistory(String projectId, String action);

        void recordCommand(String projectId, String action, JSONObject params);
    }

    public JSONObject dispatch(final JSONObject params,
                               final Consumer<String> broadcaster,
                               final Context context) {
        final String pid = params.optString("projectId", "");
        final String superNodeId = params.optString("superNodeId", null);
        final String edgeId = params.optString("edgeId", "");
        final String targetId = params.optString("targetId", "");

        if (pid.isBlank() || edgeId.isBlank() || targetId.isBlank()) {
            return context.errorResponse("BAD_REQUEST", "Missing projectId, edgeId, or targetId");
        }

        final RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }

        final SceneFlow sceneFlow = project.getSceneFlow();
        final SuperNode snapshotTarget = context.resolveSuperNode(sceneFlow, superNodeId);
        final SuperNode activeRoot = snapshotTarget != null ? snapshotTarget : sceneFlow;

        final AbstractEdge dataEdge = context.resolveEdgeById(activeRoot, edgeId);
        if (dataEdge == null) {
            return context.errorResponse("EDGE_NOT_FOUND", "Edge not found: " + edgeId);
        }

        final BasicNode sourceNode = dataEdge.getSourceNode();
        final BasicNode oldTargetNode = dataEdge.getTargetNode();
        final BasicNode newTargetNode = context.resolveNodeById(activeRoot, targetId);

        if (sourceNode == null || newTargetNode == null) {
            return context.errorResponse("NODE_NOT_FOUND", "Source or target node not found");
        }

        final int nodeWidth = context.getEditorConfigInt(pid, "node_width", 90);
        final int nodeHeight = context.getEditorConfigInt(pid, "node_height", nodeWidth);

        releaseOldTargetDock(context, dataEdge, oldTargetNode, nodeWidth, nodeHeight);

        NodeGraphics newTgtGraphics = newTargetNode.getGraphics();
        NodePosition newTgtPos = newTgtGraphics != null ? newTgtGraphics.getPosition() : null;
        double newTgtX = newTgtPos != null ? newTgtPos.getXPos() : 0;
        double newTgtY = newTgtPos != null ? newTgtPos.getYPos() : 0;
        boolean newTgtIsSuperNode = newTargetNode instanceof SuperNode;

        NodeGraphics srcGraphics = sourceNode.getGraphics();
        NodePosition srcPos = srcGraphics != null ? srcGraphics.getPosition() : null;
        double srcX = srcPos != null ? srcPos.getXPos() : 0;
        double srcY = srcPos != null ? srcPos.getYPos() : 0;
        boolean srcIsSuperNode = sourceNode instanceof SuperNode;

        boolean isSelfLoop = sourceNode.getId().equals(newTargetNode.getId());
        int[] dockPair = isSelfLoop
                ? context.findSelfLoopDockPointPair(sourceNode.getId(), nodeWidth, nodeHeight, srcIsSuperNode)
                : context.findBestDockPointPair(
                        sourceNode.getId(), srcX, srcY, nodeWidth, nodeHeight, srcIsSuperNode,
                        newTargetNode.getId(), newTgtX, newTgtY, nodeWidth, nodeHeight, newTgtIsSuperNode
                );
        int newTgtDockIdx = dockPair[1];
        context.occupyDockPoint(newTargetNode.getId(), newTgtDockIdx, false);

        double[] newTgtDock = context.getDockPointPosition(newTgtX, newTgtY, nodeWidth, nodeHeight, newTgtIsSuperNode, newTgtDockIdx);
        updateEdgeEndpointControls(context, dataEdge, newTgtDock, srcX, srcY, nodeWidth, nodeHeight, isSelfLoop);

        detachEdgeFromSource(dataEdge, sourceNode);
        dataEdge.setTargetNode(newTargetNode);
        dataEdge.setTargetUnid(newTargetNode.getId());
        attachEdgeToSource(dataEdge, sourceNode);

        JSONObject response = context.createSceneFlowSnapshot(project, pid, snapshotTarget, sceneFlow);
        response.put("status", "ok");
        response.put("edgeId", edgeId);
        context.broadcastSceneFlowSnapshot(broadcaster, pid, response);
        context.recordHistory(pid, "SceneFlow.Edge.Retarget");
        context.recordCommand(pid, "SceneFlow.Edge.Retarget", params);
        return response;
    }

    private void releaseOldTargetDock(final Context context,
                                      final AbstractEdge dataEdge,
                                      final BasicNode oldTargetNode,
                                      final int nodeWidth,
                                      final int nodeHeight) {
        if (oldTargetNode == null) {
            return;
        }
        EdgeGraphics edgeGraphics = dataEdge.getGraphics();
        if (edgeGraphics == null || edgeGraphics.getConnection() == null) {
            return;
        }
        List<EdgePoint> points = edgeGraphics.getConnection().getPointList();
        if (points == null || points.size() < 2) {
            return;
        }
        EdgePoint endPt = points.get(points.size() - 1);
        NodeGraphics oldTgtGraphics = oldTargetNode.getGraphics();
        NodePosition oldTgtPos = oldTgtGraphics != null ? oldTgtGraphics.getPosition() : null;
        double oldTgtX = oldTgtPos != null ? oldTgtPos.getXPos() : 0;
        double oldTgtY = oldTgtPos != null ? oldTgtPos.getYPos() : 0;
        int oldDockIdx = context.findDockPointIndex(oldTgtX, oldTgtY, nodeWidth, nodeHeight,
                oldTargetNode instanceof SuperNode, endPt.getXPos(), endPt.getYPos());
        if (oldDockIdx >= 0) {
            context.releaseDockPoint(oldTargetNode.getId(), oldDockIdx, false);
        }
    }

    private void updateEdgeEndpointControls(final Context context,
                                            final AbstractEdge dataEdge,
                                            final double[] newTgtDock,
                                            final double srcX,
                                            final double srcY,
                                            final int nodeWidth,
                                            final int nodeHeight,
                                            final boolean isSelfLoop) {
        EdgeGraphics edgeGraphics = dataEdge.getGraphics();
        if (edgeGraphics == null || edgeGraphics.getConnection() == null) {
            return;
        }
        List<EdgePoint> points = edgeGraphics.getConnection().getPointList();
        if (points == null || points.size() < 2) {
            return;
        }
        EdgePoint endPt = points.get(points.size() - 1);
        EdgePoint startPt = points.get(0);
        endPt.setXPos((int) Math.round(newTgtDock[0]));
        endPt.setYPos((int) Math.round(newTgtDock[1]));

        if (isSelfLoop) {
            double nodeCenterX = srcX + nodeWidth / 2.0;
            double nodeCenterY = srcY + nodeHeight / 2.0;
            double[] loopCtrl = context.computeSelfLoopControlPoints(
                    startPt.getXPos(), startPt.getYPos(),
                    newTgtDock[0], newTgtDock[1],
                    nodeCenterX, nodeCenterY, nodeWidth, nodeHeight
            );
            startPt.setCtrlXPos((int) Math.round(loopCtrl[0]));
            startPt.setCtrlYPos((int) Math.round(loopCtrl[1]));
            endPt.setCtrlXPos((int) Math.round(loopCtrl[2]));
            endPt.setCtrlYPos((int) Math.round(loopCtrl[3]));
        } else {
            double[] tgtCtrl = context.computeInitialControlPoint(
                    startPt.getXPos(), startPt.getYPos(),
                    newTgtDock[0], newTgtDock[1], false
            );
            endPt.setCtrlXPos((int) Math.round(tgtCtrl[0]));
            endPt.setCtrlYPos((int) Math.round(tgtCtrl[1]));
        }
    }

    private void detachEdgeFromSource(final AbstractEdge dataEdge, final BasicNode sourceNode) {
        if (dataEdge instanceof GuargedEdge) {
            sourceNode.removeCEdge((GuargedEdge) dataEdge);
        } else if (dataEdge instanceof InterruptEdge) {
            sourceNode.removeIEdge((InterruptEdge) dataEdge);
        } else if (dataEdge instanceof RandomEdge) {
            sourceNode.removePEdge((RandomEdge) dataEdge);
        } else if (dataEdge instanceof ForkingEdge) {
            sourceNode.removeFEdge((ForkingEdge) dataEdge);
        } else if (dataEdge instanceof TimeoutEdge || dataEdge instanceof EpsilonEdge) {
            sourceNode.removeDEdge();
        }
    }

    private void attachEdgeToSource(final AbstractEdge dataEdge, final BasicNode sourceNode) {
        if (dataEdge instanceof GuargedEdge) {
            sourceNode.addCEdge((GuargedEdge) dataEdge);
        } else if (dataEdge instanceof InterruptEdge) {
            sourceNode.addIEdge((InterruptEdge) dataEdge);
        } else if (dataEdge instanceof RandomEdge) {
            sourceNode.addPEdge((RandomEdge) dataEdge);
        } else if (dataEdge instanceof ForkingEdge) {
            sourceNode.addFEdge((ForkingEdge) dataEdge);
        } else if (dataEdge instanceof TimeoutEdge || dataEdge instanceof EpsilonEdge) {
            sourceNode.setDedge(dataEdge);
        }
    }
}
