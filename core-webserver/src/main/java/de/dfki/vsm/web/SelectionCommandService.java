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
import de.dfki.vsm.model.sceneflow.chart.graphics.node.NodeGraphics;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.model.sceneflow.glue.command.definition.DataTypeDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;

/**
 * Handles SceneFlow.Selection.Copy/Paste commands.
 */
public final class SelectionCommandService {

    public static final class ClipboardEdgeData {
        final String sourceId;
        final String targetId;
        final String edgeType;
        final String condition;
        final int probability;
        final long timeout;

        public ClipboardEdgeData(String sourceId, String targetId, String edgeType,
                                 String condition, int probability, long timeout) {
            this.sourceId = sourceId;
            this.targetId = targetId;
            this.edgeType = edgeType;
            this.condition = condition;
            this.probability = probability;
            this.timeout = timeout;
        }
    }

    public interface Context {
        RunTimeProject runtimeProject(String projectId);

        JSONObject errorResponse(String code, String message);

        BasicNode findNodeRecursive(SuperNode root, String nodeId);

        SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId);

        int getEditorConfigInt(String projectId, String key, int fallback);

        void collectNodes(SuperNode node, List<BasicNode> out);

        String allocateNodeId(String projectId, boolean superNode, Set<String> used);

        BasicNode resolveNodeById(SuperNode root, String nodeId);

        Expression parseExpressionOrNull(String text);

        void initializeEdgeDockPoints(AbstractEdge edge, int nodeWidth, int nodeHeight);

        void normalizeEdge(AbstractEdge edge, int nodeWidth, int nodeHeight);

        JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SuperNode snapshotTarget, SceneFlow sceneFlow);

        JSONObject buildSceneFlowResponse(JSONObject snapshot);

        void broadcastSceneFlowSnapshot(Consumer<String> broadcaster, String projectId, JSONObject snapshot);

        void recordHistory(String projectId, String action);

        void markDirty(String projectId);

        List<BasicNode> clipboardNodes(String projectId);

        List<ClipboardEdgeData> clipboardEdges(String projectId);
    }

    public JSONObject dispatch(final String method,
                               final JSONObject params,
                               final Consumer<String> broadcaster,
                               final Context context) {
        switch (method) {
            case "SceneFlow.Selection.Copy":
                return copySelectionForProject(params, context);
            case "SceneFlow.Selection.Paste":
                return pasteSelectionForProject(params, broadcaster, context);
            default:
                return context.errorResponse("BAD_REQUEST", "Unsupported selection command: " + method);
        }
    }

    private JSONObject copySelectionForProject(final JSONObject params, final Context context) {
        String pid = params.optString("projectId", "");
        RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }

        JSONArray nodeIdsJson = params.optJSONArray("nodeIds");
        if (nodeIdsJson == null || nodeIdsJson.isEmpty()) {
            return context.errorResponse("BAD_REQUEST", "Missing nodeIds");
        }

        SceneFlow sceneFlow = project.getSceneFlow();
        Set<String> nodeIdSet = new HashSet<>();
        for (int i = 0; i < nodeIdsJson.length(); i++) {
            nodeIdSet.add(nodeIdsJson.getString(i));
        }

        List<BasicNode> clipboardNodes = context.clipboardNodes(pid);
        List<ClipboardEdgeData> clipboardEdges = context.clipboardEdges(pid);
        clipboardNodes.clear();
        clipboardEdges.clear();

        for (String nodeId : nodeIdSet) {
            BasicNode node = context.findNodeRecursive(sceneFlow, nodeId);
            if (node != null) {
                clipboardNodes.add(node.getCopy());
            }
        }

        for (BasicNode node : clipboardNodes) {
            String sourceId = node.getId();
            collectEdgesForClipboard(clipboardEdges, node.getCEdgeList(), sourceId, "CEDGE", nodeIdSet);
            collectEdgesForClipboard(clipboardEdges, node.getPEdgeList(), sourceId, "PEDGE", nodeIdSet);
            collectEdgesForClipboard(clipboardEdges, node.getIEdgeList(), sourceId, "IEDGE", nodeIdSet);
            collectEdgesForClipboard(clipboardEdges, node.getFEdgeList(), sourceId, "FEDGE", nodeIdSet);
            AbstractEdge dEdge = node.getDedge();
            if (dEdge != null && nodeIdSet.contains(dEdge.getTargetUnid())) {
                String edgeType = dEdge instanceof TimeoutEdge ? "TEDGE" : "EEDGE";
                long timeout = dEdge instanceof TimeoutEdge ? ((TimeoutEdge) dEdge).getTimeout() : 0;
                clipboardEdges.add(new ClipboardEdgeData(sourceId, dEdge.getTargetUnid(),
                        edgeType, null, 0, timeout));
            }
        }

        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("copiedCount", clipboardNodes.size());
        return response;
    }

    private void collectEdgesForClipboard(final List<ClipboardEdgeData> clipboardEdges,
                                          final List<? extends AbstractEdge> edges,
                                          final String sourceId,
                                          final String edgeType,
                                          final Set<String> nodeIdSet) {
        if (edges == null) {
            return;
        }
        for (AbstractEdge edge : edges) {
            String targetId = edge.getTargetUnid();
            if (nodeIdSet.contains(targetId)) {
                String condition = null;
                int probability = 0;
                if (edge instanceof GuargedEdge) {
                    Expression cond = ((GuargedEdge) edge).getCondition();
                    condition = cond != null ? cond.getFormattedSyntax() : "true";
                } else if (edge instanceof InterruptEdge) {
                    Expression cond = ((InterruptEdge) edge).getCondition();
                    condition = cond != null ? cond.getFormattedSyntax() : "true";
                } else if (edge instanceof RandomEdge) {
                    probability = ((RandomEdge) edge).getProbability();
                }
                clipboardEdges.add(new ClipboardEdgeData(sourceId, targetId, edgeType, condition, probability, 0));
            }
        }
    }

    private JSONObject pasteSelectionForProject(final JSONObject params,
                                                final Consumer<String> broadcaster,
                                                final Context context) {
        String pid = params.optString("projectId", "");
        RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }

        List<BasicNode> clipboardNodes = context.clipboardNodes(pid);
        List<ClipboardEdgeData> clipboardEdges = context.clipboardEdges(pid);
        if (clipboardNodes.isEmpty()) {
            JSONObject response = new JSONObject();
            response.put("status", "ok");
            response.put("nodeIds", new JSONArray());
            return response;
        }

        int dx = params.optInt("dx", 50);
        int dy = params.optInt("dy", 50);
        String superNodeId = params.optString("superNodeId", null);

        SceneFlow sceneFlow = project.getSceneFlow();
        SuperNode snapshotTarget = context.resolveSuperNode(sceneFlow, superNodeId);
        SuperNode activeSuperNode = snapshotTarget != null ? snapshotTarget : sceneFlow;

        int nodeWidth = context.getEditorConfigInt(pid, "node_width", 90);
        int nodeHeight = context.getEditorConfigInt(pid, "node_height", nodeWidth);
        int gridScaleX = context.getEditorConfigInt(pid, "grid_x", 1);
        int gridScaleY = context.getEditorConfigInt(pid, "grid_y", gridScaleX);
        int gridX = Math.max(8, nodeWidth * gridScaleX);
        int gridY = Math.max(8, nodeHeight * gridScaleY);
        double originX = nodeWidth / 2.0 + nodeWidth / 3.0;
        double originY = nodeHeight / 2.0 + nodeHeight / 3.0;

        Set<String> usedIds = new HashSet<>();
        List<BasicNode> existingNodes = new ArrayList<>();
        context.collectNodes(sceneFlow, existingNodes);
        for (BasicNode existing : existingNodes) {
            if (existing != null && existing.getId() != null) {
                usedIds.add(existing.getId());
            }
        }

        List<int[]> occupiedPositions = new ArrayList<>();
        for (BasicNode node : activeSuperNode.getNodeAndSuperNodeList()) {
            NodeGraphics g = node.getGraphics();
            if (g != null && g.getPosition() != null) {
                int nx = g.getPosition().getXPos();
                int ny = g.getPosition().getYPos();
                if (nx > Integer.MIN_VALUE + 1000 && ny > Integer.MIN_VALUE + 1000) {
                    occupiedPositions.add(new int[]{nx, ny});
                }
            }
        }

        int collisionThreshold = Math.max(nodeWidth, nodeHeight);
        Map<String, String> idMapping = new HashMap<>();
        List<String> newNodeIds = new ArrayList<>();

        for (BasicNode clipboardNode : clipboardNodes) {
            String oldId = clipboardNode.getId();
            boolean isSuperNode = clipboardNode instanceof SuperNode;
            String newId = context.allocateNodeId(pid, isSuperNode, usedIds);
            usedIds.add(newId);
            idMapping.put(oldId, newId);

            BasicNode newNode = isSuperNode ? new SuperNode() : new BasicNode();
            newNode.setId(newId);
            newNode.setName(clipboardNode.getName());
            newNode.setComment(clipboardNode.getComment());
            newNode.setHistoryNodeFlag(clipboardNode.isHistoryNode());

            for (VariableDefinition varDef : clipboardNode.getVarDefList()) {
                newNode.addVarDef(varDef.getCopy());
            }
            for (DataTypeDefinition typeDef : clipboardNode.getTypeDefList()) {
                newNode.addTypeDef(typeDef.getCopy());
            }
            for (Command cmd : clipboardNode.getCmdList()) {
                newNode.addCmd(cmd.getCopy());
            }

            NodeGraphics oldGraphics = clipboardNode.getGraphics();
            int x = (oldGraphics != null && oldGraphics.getPosition() != null ? oldGraphics.getPosition().getXPos() : 0) + dx;
            int y = (oldGraphics != null && oldGraphics.getPosition() != null ? oldGraphics.getPosition().getYPos() : 0) + dy;

            double centerX = x + nodeWidth / 2.0;
            double centerY = y + nodeHeight / 2.0;
            double snappedCenterX = originX + Math.round((centerX - originX) / gridX) * gridX;
            double snappedCenterY = originY + Math.round((centerY - originY) / gridY) * gridY;
            x = Math.max(1, (int) Math.round(snappedCenterX - nodeWidth / 2.0));
            y = Math.max(1, (int) Math.round(snappedCenterY - nodeHeight / 2.0));

            int attempts = 0;
            while (isPositionOccupied(x, y, occupiedPositions, collisionThreshold) && attempts < 100) {
                attempts++;
                x += gridX;
                if (attempts % 5 == 0) {
                    x -= 5 * gridX;
                    y += gridY;
                }
            }
            occupiedPositions.add(new int[]{x, y});

            newNode.setGraphics(new NodeGraphics(x, y));
            newNode.setParentNode(activeSuperNode);
            if (isSuperNode) {
                activeSuperNode.addSuperNode((SuperNode) newNode);
            } else {
                activeSuperNode.addNode(newNode);
            }

            newNodeIds.add(newId);
        }

        for (ClipboardEdgeData ce : clipboardEdges) {
            String newSourceId = idMapping.get(ce.sourceId);
            String newTargetId = idMapping.get(ce.targetId);
            if (newSourceId == null || newTargetId == null) {
                continue;
            }
            BasicNode sourceNode = context.resolveNodeById(activeSuperNode, newSourceId);
            BasicNode targetNode = context.resolveNodeById(activeSuperNode, newTargetId);
            if (sourceNode == null || targetNode == null) {
                continue;
            }
            createEdgeFromClipboard(context, pid, sourceNode, targetNode, ce);
        }

        context.markDirty(pid);
        JSONObject snapshot = context.createSceneFlowSnapshot(project, pid, snapshotTarget, sceneFlow);
        JSONObject response = context.buildSceneFlowResponse(snapshot);
        response.put("nodeIds", new JSONArray(newNodeIds));
        context.broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
        context.recordHistory(pid, "SceneFlow.Selection.Paste");
        return response;
    }

    private void createEdgeFromClipboard(final Context context,
                                         final String projectId,
                                         final BasicNode sourceNode,
                                         final BasicNode targetNode,
                                         final ClipboardEdgeData ce) {
        AbstractEdge edge;
        switch (ce.edgeType) {
            case "CEDGE":
                GuargedEdge cedge = new GuargedEdge();
                cedge.setCondition(context.parseExpressionOrNull(ce.condition != null ? ce.condition : "true"));
                sourceNode.addCEdge(cedge);
                edge = cedge;
                break;
            case "IEDGE":
                InterruptEdge iedge = new InterruptEdge();
                iedge.setCondition(context.parseExpressionOrNull(ce.condition != null ? ce.condition : "true"));
                sourceNode.addIEdge(iedge);
                edge = iedge;
                break;
            case "PEDGE":
                RandomEdge pedge = new RandomEdge();
                pedge.setProbability(ce.probability);
                sourceNode.addPEdge(pedge);
                edge = pedge;
                break;
            case "FEDGE":
                ForkingEdge fedge = new ForkingEdge();
                sourceNode.addFEdge(fedge);
                edge = fedge;
                break;
            case "TEDGE":
                TimeoutEdge tedge = new TimeoutEdge();
                tedge.setTimeout(ce.timeout);
                sourceNode.setDedge(tedge);
                edge = tedge;
                break;
            case "EEDGE":
            default:
                EpsilonEdge eedge = new EpsilonEdge();
                sourceNode.setDedge(eedge);
                edge = eedge;
                break;
        }

        edge.setSourceNode(sourceNode);
        edge.setTargetNode(targetNode);
        edge.setSourceUnid(sourceNode.getId());
        edge.setTargetUnid(targetNode.getId());
        edge.setGraphics(new EdgeGraphics());
        int nodeWidth = context.getEditorConfigInt(projectId, "node_width", 90);
        int nodeHeight = context.getEditorConfigInt(projectId, "node_height", nodeWidth);
        context.initializeEdgeDockPoints(edge, nodeWidth, nodeHeight);
        context.normalizeEdge(edge, nodeWidth, nodeHeight);
    }

    private boolean isPositionOccupied(final int x,
                                       final int y,
                                       final List<int[]> occupiedPositions,
                                       final int threshold) {
        for (int[] pos : occupiedPositions) {
            int dx = Math.abs(x - pos[0]);
            int dy = Math.abs(y - pos[1]);
            if (dx < threshold && dy < threshold) {
                return true;
            }
        }
        return false;
    }
}
