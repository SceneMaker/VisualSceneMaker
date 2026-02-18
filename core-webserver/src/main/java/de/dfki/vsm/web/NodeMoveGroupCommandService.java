package de.dfki.vsm.web;

import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.graphics.node.NodeGraphics;
import de.dfki.vsm.model.sceneflow.chart.graphics.node.NodePosition;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.function.Consumer;

/**
 * Handles SceneFlow.Node.MoveGroup command.
 */
public final class NodeMoveGroupCommandService {

    public interface Context {
        RunTimeProject runtimeProject(String projectId);

        JSONObject errorResponse(String code, String message);

        SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId);

        int getEditorConfigInt(String projectId, String key, int fallback);

        BasicNode findNodeRecursive(SuperNode root, String nodeId);

        void updateEdgeEndpointsForMovedNode(BasicNode movedNode, SuperNode activeSuperNode, int oldX, int oldY);

        JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SuperNode snapshotTarget, SceneFlow sceneFlow);

        JSONObject buildSceneFlowResponse(JSONObject snapshot);

        void broadcastSceneFlowSnapshot(Consumer<String> broadcaster, String projectId, JSONObject snapshot);

        void recordHistory(String projectId, String action);

        void recordCommand(String projectId, String action, JSONObject params);
    }

    public JSONObject dispatch(final JSONObject params,
                               final Consumer<String> broadcaster,
                               final Context context) {
        final String pid = params.optString("projectId", "");
        final String superNodeId = params.optString("superNodeId", null);
        final JSONArray nodesPayload = params.optJSONArray("nodes");
        final boolean snap = params.optBoolean("snap", false);

        final RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }
        if (nodesPayload == null || nodesPayload.length() == 0) {
            return context.errorResponse("BAD_REQUEST", "Missing nodes");
        }

        final SceneFlow sceneFlow = project.getSceneFlow();
        final SuperNode snapshotTarget = context.resolveSuperNode(sceneFlow, superNodeId);
        final SuperNode activeSuperNode = snapshotTarget != null ? snapshotTarget : sceneFlow;

        final int nodeW = context.getEditorConfigInt(pid, "node_width", 90);
        final int nodeH = context.getEditorConfigInt(pid, "node_height", nodeW);
        final int gridScaleX = context.getEditorConfigInt(pid, "grid_x", 1);
        final int gridScaleY = context.getEditorConfigInt(pid, "grid_y", gridScaleX);
        final int gridX = Math.max(8, nodeW * gridScaleX);
        final int gridY = Math.max(8, nodeH * gridScaleY);
        final double snapOriginX = nodeW / 2.0 + nodeW / 3.0;
        final double snapOriginY = nodeH / 2.0 + nodeH / 3.0;

        IdentityHashMap<BasicNode, int[]> oldPositions = new IdentityHashMap<>();
        List<BasicNode> movedNodes = new ArrayList<>();

        for (int i = 0; i < nodesPayload.length(); i++) {
            JSONObject entry = nodesPayload.optJSONObject(i);
            if (entry == null) {
                return context.errorResponse("BAD_REQUEST", "Invalid nodes entry");
            }
            String moveId = entry.optString("id", "");
            double moveX = entry.has("x") ? entry.optDouble("x", Double.NaN) : Double.NaN;
            double moveY = entry.has("y") ? entry.optDouble("y", Double.NaN) : Double.NaN;
            if (moveId.isBlank() || Double.isNaN(moveX) || Double.isNaN(moveY)) {
                return context.errorResponse("BAD_REQUEST", "Missing node id or coordinates");
            }

            BasicNode dataNode = context.findNodeRecursive(sceneFlow, moveId);
            if (dataNode == null) {
                return context.errorResponse("NODE_NOT_FOUND", "Node not found: " + moveId);
            }

            NodeGraphics oldGraphics = dataNode.getGraphics();
            NodePosition oldPos = oldGraphics != null ? oldGraphics.getPosition() : null;
            int oldX = oldPos != null ? oldPos.getXPos() : 0;
            int oldY = oldPos != null ? oldPos.getYPos() : 0;
            oldPositions.put(dataNode, new int[]{oldX, oldY});

            int targetX = Math.max(1, (int) Math.round(moveX));
            int targetY = Math.max(1, (int) Math.round(moveY));
            if (snap) {
                double centerX = targetX + nodeW / 2.0;
                double centerY = targetY + nodeH / 2.0;
                double snappedCenterX = snapOriginX + Math.round((centerX - snapOriginX) / gridX) * gridX;
                double snappedCenterY = snapOriginY + Math.round((centerY - snapOriginY) / gridY) * gridY;
                targetX = (int) Math.round(snappedCenterX - nodeW / 2.0);
                targetY = (int) Math.round(snappedCenterY - nodeH / 2.0);
            }

            NodeGraphics graphics = dataNode.getGraphics();
            if (graphics == null) {
                graphics = new NodeGraphics(targetX, targetY);
                dataNode.setGraphics(graphics);
            } else {
                graphics.setPosition(targetX, targetY);
            }
            movedNodes.add(dataNode);
        }

        for (BasicNode movedNode : movedNodes) {
            int[] oldPos = oldPositions.get(movedNode);
            context.updateEdgeEndpointsForMovedNode(movedNode, activeSuperNode, oldPos[0], oldPos[1]);
        }

        JSONObject snapshot = context.createSceneFlowSnapshot(project, pid, snapshotTarget, sceneFlow);
        JSONObject response = context.buildSceneFlowResponse(snapshot);
        context.broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
        context.recordHistory(pid, "SceneFlow.Node.MoveGroup");
        context.recordCommand(pid, "SceneFlow.Node.MoveGroup", params);
        return response;
    }
}
