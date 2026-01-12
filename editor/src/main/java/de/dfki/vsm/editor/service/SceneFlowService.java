package de.dfki.vsm.editor.service;

import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.editor.util.IDManager;
import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.graphics.node.NodeGraphics;
import de.dfki.vsm.model.sceneflow.chart.edge.*;
import de.dfki.vsm.model.sceneflow.glue.GlueParser;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.log.LOGDefaultLogger;

import java.awt.Point;
import java.util.*;

/**
 * Headless service for managing SceneFlow editing operations.
 * Extracted from action classes to remove Swing dependencies.
 *
 * @author Phase 2 Refactoring - 2026-01-11
 */
public class SceneFlowService {

    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    /**
     * Node types supported by the editor.
     */
    public enum NodeType {
        BASIC_NODE,
        SUPER_NODE
    }

    /**
     * Edge types supported by the editor.
     */
    public enum EdgeType {
        EPSILON_EDGE,  // EEdge - automatic transition
        TIMEOUT_EDGE,  // TEdge - timeout transition
        CONDITIONAL_EDGE,  // CEdge - guarded/conditional transition
        PROBABILISTIC_EDGE,  // PEdge - random/probabilistic transition
        FORKING_EDGE,  // FEdge - parallel fork transition
        INTERRUPTIVE_EDGE  // IEdge - interrupt transition
    }

    /**
     * Creates a new node in the sceneflow.
     *
     * Extracted from: CreateNodeAction constructor and NodeAction.create() - lines 54-97, 125-155
     *
     * @param project Project containing the sceneflow
     * @param parentNodeId ID of parent SuperNode
     * @param type Type of node to create (BasicNode or SuperNode)
     * @param position Position for the node (used for NodeGraphics)
     * @param name Optional name for the node (null = auto-generate)
     * @return Created node, or null if failed
     */
    public BasicNode createNode(EditorProject project, String parentNodeId, NodeType type,
                                 Point position, String name) {
        if (project == null || parentNodeId == null || type == null) {
            mLogger.failure("Error: Cannot create node with null parameters");
            return null;
        }

        // Find parent node
        SuperNode parentNode = findSuperNodeById(project, parentNodeId);
        if (parentNode == null) {
            mLogger.failure("Error: Parent node not found: " + parentNodeId);
            return null;
        }

        // Create IDManager for this sceneflow
        IDManager idManager = new IDManager(project.getSceneFlow());

        BasicNode dataNode;
        String nodeId;

        if (type == NodeType.BASIC_NODE) {
            // Get next free basic node ID
            nodeId = idManager.getNextFreeNodeID();

            // Generate name if not provided
            String nodeName = (name != null) ? name : "Node " + nodeId;

            // Create NodeGraphics
            NodeGraphics nodeGraphics = new NodeGraphics(position.x, position.y);

            // Create BasicNode using no-arg constructor
            dataNode = new BasicNode();
            dataNode.setId(nodeId);
            dataNode.setName(nodeName);
            dataNode.setGraphics(nodeGraphics);

        } else { // SUPER_NODE
            // Get next free super node ID
            nodeId = idManager.getNextFreeSuperNodeID();

            // Generate name if not provided
            String nodeName = (name != null) ? name : "SuperNode " + nodeId;

            // Create NodeGraphics
            NodeGraphics nodeGraphics = new NodeGraphics(position.x, position.y);

            // Create SuperNode using no-arg constructor
            SuperNode superNode = new SuperNode();
            superNode.setId(nodeId);
            superNode.setName(nodeName);
            superNode.setGraphics(nodeGraphics);

            // Create automatic History child node (from CreateNodeAction lines 77-85)
            String historyId = idManager.getNextFreeNodeID();
            NodeGraphics historyGraphics = new NodeGraphics(
                project.getEditorConfig().sNODEWIDTH,
                project.getEditorConfig().sNODEHEIGHT
            );
            BasicNode historyNode = new BasicNode();
            historyNode.setId(historyId);
            historyNode.setName("History");
            historyNode.setGraphics(historyGraphics);
            historyNode.setHistoryNodeFlag(true);
            historyNode.setParentNode(superNode);
            superNode.addNode(historyNode);
            superNode.getStartNodeMap().put(historyNode.getId(), historyNode);

            dataNode = superNode;
        }

        // Set parent node
        dataNode.setParentNode(parentNode);

        // Check if parent has no start node - make this the start node
        if (parentNode.getStartNodeMap().isEmpty()) {
            parentNode.getStartNodeMap().put(dataNode.getId(), dataNode);
        }

        // Add node to parent's child list
        if (type == NodeType.BASIC_NODE) {
            parentNode.addNode(dataNode);
        } else { // SUPER_NODE
            if (!parentNode.getSuperNodeList().contains(dataNode)) {
                parentNode.addSuperNode((SuperNode) dataNode);
            }
        }

        mLogger.message("Created " + type + " with ID: " + nodeId);

        return dataNode;
    }

    /**
     * Deletes a node from the sceneflow.
     *
     * Extracted from: RemoveNodeAction and NodeAction.delete() - lines 40-123
     *
     * @param project Project containing the sceneflow
     * @param nodeId ID of node to delete
     * @return true if successful
     */
    public boolean deleteNode(EditorProject project, String nodeId) {
        if (project == null || nodeId == null) {
            mLogger.failure("Error: Cannot delete node with null parameters");
            return false;
        }

        // Find the node
        BasicNode dataNode = findNodeById(project, nodeId);
        if (dataNode == null) {
            mLogger.failure("Error: Node not found: " + nodeId);
            return false;
        }

        SuperNode parentNode = (SuperNode) dataNode.getParentNode();
        if (parentNode == null) {
            mLogger.failure("Error: Node has no parent: " + nodeId);
            return false;
        }

        // Delete all connected edges first (from RemoveNodeAction lines 40-45)
        List<String> edgesToDelete = new ArrayList<>();

        // Collect outgoing edges
        if (dataNode.getDedge() != null) {
            edgesToDelete.add(dataNode.getId() + "->" + dataNode.getDedge().getTargetUnid());
        }
        for (GuargedEdge edge : dataNode.getCEdgeList()) {
            edgesToDelete.add(dataNode.getId() + "->" + edge.getTargetUnid());
        }
        for (RandomEdge edge : dataNode.getPEdgeList()) {
            edgesToDelete.add(dataNode.getId() + "->" + edge.getTargetUnid());
        }
        for (ForkingEdge edge : dataNode.getFEdgeList()) {
            edgesToDelete.add(dataNode.getId() + "->" + edge.getTargetUnid());
        }
        for (InterruptEdge edge : dataNode.getIEdgeList()) {
            edgesToDelete.add(dataNode.getId() + "->" + edge.getTargetUnid());
        }

        // Collect incoming edges (need to search all nodes in parent)
        for (BasicNode node : parentNode.getNodeList()) {
            if (pointsToNode(node, nodeId)) {
                edgesToDelete.add(node.getId() + "->" + nodeId);
            }
        }
        for (SuperNode node : parentNode.getSuperNodeList()) {
            if (pointsToNode(node, nodeId)) {
                edgesToDelete.add(node.getId() + "->" + nodeId);
            }
        }

        // Delete all collected edges
        for (String edgeSpec : edgesToDelete) {
            String[] parts = edgeSpec.split("->");
            deleteEdgesFromNodeToTarget(project, parts[0], parts[1]);
        }

        // Remove from parent's child list
        boolean removed = false;
        if (dataNode instanceof SuperNode) {
            parentNode.removeSuperNode((SuperNode) dataNode);
            removed = true;
        } else {
            parentNode.removeNode(dataNode);
            removed = true;
        }

        // Handle start node reassignment (from NodeAction.delete() lines 66-100)
        Map<String, BasicNode> startNodeMap = parentNode.getStartNodeMap();
        if (startNodeMap.containsKey(nodeId)) {
            startNodeMap.remove(nodeId);

            // If no start nodes left, assign a new one
            if (startNodeMap.isEmpty()) {
                if (!parentNode.getNodeList().isEmpty()) {
                    // Find first non-history node
                    for (BasicNode node : parentNode.getNodeList()) {
                        if (!node.isHistoryNode()) {
                            parentNode.getStartNodeMap().put(node.getId(), node);
                            break;
                        }
                    }
                } else if (!parentNode.getSuperNodeList().isEmpty()) {
                    // Use first super node
                    BasicNode newStartNode = parentNode.getSuperNodeAt(0);
                    parentNode.getStartNodeMap().put(newStartNode.getId(), newStartNode);
                }
            }
        }

        if (removed) {
            mLogger.message("Deleted node: " + nodeId);
            return true;
        } else {
            mLogger.failure("Error: Failed to delete node: " + nodeId);
            return false;
        }
    }

    /**
     * Creates a new edge in the sceneflow.
     *
     * Extracted from: CreateEdgeAction and EdgeAction.create() - lines 52-144
     *
     * @param project Project containing the sceneflow
     * @param sourceNodeId ID of source node
     * @param targetNodeId ID of target node
     * @param type Type of edge to create
     * @param edgeData Optional edge data (for edges that need configuration)
     * @return Created edge, or null if failed
     */
    public AbstractEdge createEdge(EditorProject project, String sourceNodeId, String targetNodeId,
                                   EdgeType type, Map<String, Object> edgeData) {
        if (project == null || sourceNodeId == null || targetNodeId == null || type == null) {
            mLogger.failure("Error: Cannot create edge with null parameters");
            return null;
        }

        // Find source and target nodes
        BasicNode sourceNode = findNodeById(project, sourceNodeId);
        BasicNode targetNode = findNodeById(project, targetNodeId);

        if (sourceNode == null || targetNode == null) {
            mLogger.failure("Error: Source or target node not found");
            return null;
        }

        // Create the appropriate edge type (from CreateEdgeAction.showCreationDialog() lines 52-104)
        AbstractEdge dataEdge;

        switch (type) {
            case EPSILON_EDGE:
                dataEdge = new EpsilonEdge();
                break;

            case FORKING_EDGE:
                dataEdge = new ForkingEdge();
                break;

            case TIMEOUT_EDGE:
                // TimeoutEdge requires duration
                long timeout = edgeData != null && edgeData.containsKey("timeout")
                    ? ((Number) edgeData.get("timeout")).longValue() : 1000L;
                TimeoutEdge tedge = new TimeoutEdge();
                try {
                    tedge.setTimeout(timeout);
                } catch (NumberFormatException e) {
                    mLogger.warning("Invalid timeout value, using default: " + e.getMessage());
                }
                dataEdge = tedge;
                break;

            case CONDITIONAL_EDGE:
                // GuargedEdge requires condition (parsed from string)
                String conditionStr = edgeData != null && edgeData.containsKey("condition")
                    ? (String) edgeData.get("condition") : "";
                GuargedEdge cedge = new GuargedEdge();
                if (!conditionStr.isEmpty()) {
                    try {
                        Expression condition = (Expression) GlueParser.run(conditionStr);
                        if (condition != null) {
                            cedge.setCondition(condition);
                        }
                    } catch (Exception e) {
                        mLogger.warning("Failed to parse condition: " + e.getMessage());
                    }
                }
                dataEdge = cedge;
                break;

            case PROBABILISTIC_EDGE:
                // RandomEdge requires probability
                int probability = 100;
                if (!sourceNode.getPEdgeList().isEmpty()) {
                    // If there are existing PEdges, probability must be provided
                    probability = edgeData != null && edgeData.containsKey("probability")
                        ? ((Number) edgeData.get("probability")).intValue() : 50;
                }
                RandomEdge pedge = new RandomEdge();
                pedge.setProbability(probability);
                dataEdge = pedge;
                break;

            case INTERRUPTIVE_EDGE:
                // InterruptEdge requires condition (parsed from string)
                String interruptCondStr = edgeData != null && edgeData.containsKey("condition")
                    ? (String) edgeData.get("condition") : "";
                InterruptEdge iedge = new InterruptEdge();
                if (!interruptCondStr.isEmpty()) {
                    try {
                        Expression interruptCond = (Expression) GlueParser.run(interruptCondStr);
                        if (interruptCond != null) {
                            iedge.setCondition(interruptCond);
                        }
                    } catch (Exception e) {
                        mLogger.warning("Failed to parse interrupt condition: " + e.getMessage());
                    }
                }
                dataEdge = iedge;
                break;

            default:
                mLogger.failure("Error: Unknown edge type: " + type);
                return null;
        }

        // Set source and target (from EdgeAction.create() lines 44-79)
        dataEdge.setSourceNode(sourceNode);
        dataEdge.setTargetNode(targetNode);
        dataEdge.setTargetUnid(targetNode.getId());

        // Add edge to source node's edge list based on type
        switch (type) {
            case EPSILON_EDGE:
            case TIMEOUT_EDGE:
                sourceNode.setDedge(dataEdge);
                break;

            case FORKING_EDGE:
                sourceNode.addFEdge((ForkingEdge) dataEdge);
                break;

            case CONDITIONAL_EDGE:
                sourceNode.addCEdge((GuargedEdge) dataEdge);
                break;

            case PROBABILISTIC_EDGE:
                sourceNode.addPEdge((RandomEdge) dataEdge);
                break;

            case INTERRUPTIVE_EDGE:
                sourceNode.addIEdge((InterruptEdge) dataEdge);
                break;
        }

        mLogger.message("Created " + type + " from " + sourceNodeId + " to " + targetNodeId);

        return dataEdge;
    }

    /**
     * Deletes an edge from the sceneflow.
     *
     * Extracted from: RemoveEdgeAction and EdgeAction.delete() - lines 355-465
     *
     * @param project Project containing the sceneflow
     * @param sourceNodeId ID of source node
     * @param targetNodeId ID of target node
     * @param type Type of edge to delete (null = auto-detect)
     * @return true if successful
     */
    public boolean deleteEdge(EditorProject project, String sourceNodeId, String targetNodeId, EdgeType type) {
        if (project == null || sourceNodeId == null || targetNodeId == null) {
            mLogger.failure("Error: Cannot delete edge with null parameters");
            return false;
        }

        // Find source node
        BasicNode sourceNode = findNodeById(project, sourceNodeId);
        if (sourceNode == null) {
            mLogger.failure("Error: Source node not found: " + sourceNodeId);
            return false;
        }

        // If type not specified, auto-detect
        if (type == null) {
            type = detectEdgeType(sourceNode, targetNodeId);
            if (type == null) {
                mLogger.failure("Error: No edge found from " + sourceNodeId + " to " + targetNodeId);
                return false;
            }
        }

        AbstractEdge removedEdge = null;

        // Remove edge from source node (from EdgeAction.cleanUpData() lines 377-409)
        switch (type) {
            case EPSILON_EDGE:
            case TIMEOUT_EDGE:
                if (sourceNode.getDedge() != null &&
                    sourceNode.getDedge().getTargetUnid().equals(targetNodeId)) {
                    removedEdge = sourceNode.getDedge();
                    sourceNode.removeDEdge();
                }
                break;

            case CONDITIONAL_EDGE:
                for (GuargedEdge edge : sourceNode.getCEdgeList()) {
                    if (edge.getTargetUnid().equals(targetNodeId)) {
                        removedEdge = edge;
                        sourceNode.removeCEdge(edge);
                        break;
                    }
                }
                break;

            case PROBABILISTIC_EDGE:
                for (RandomEdge edge : sourceNode.getPEdgeList()) {
                    if (edge.getTargetUnid().equals(targetNodeId)) {
                        removedEdge = edge;
                        sourceNode.removePEdge(edge);
                        break;
                    }
                }

                // Handle probability reassignment (from EdgeAction.delete() lines 366-374)
                if (sourceNode.getPEdgeList().size() == 1) {
                    // Only one PEdge left, set to 100%
                    sourceNode.getPEdgeList().get(0).setProbability(100);
                }
                break;

            case FORKING_EDGE:
                for (ForkingEdge edge : sourceNode.getFEdgeList()) {
                    if (edge.getTargetUnid().equals(targetNodeId)) {
                        removedEdge = edge;
                        sourceNode.removeFEdge(edge);
                        break;
                    }
                }
                break;

            case INTERRUPTIVE_EDGE:
                for (InterruptEdge edge : sourceNode.getIEdgeList()) {
                    if (edge.getTargetUnid().equals(targetNodeId)) {
                        removedEdge = edge;
                        sourceNode.removeIEdge(edge);
                        break;
                    }
                }
                break;
        }

        if (removedEdge != null) {
            mLogger.message("Deleted " + type + " from " + sourceNodeId + " to " + targetNodeId);
            return true;
        } else {
            mLogger.failure("Error: Edge not found from " + sourceNodeId + " to " + targetNodeId);
            return false;
        }
    }

    // ========== Helper Methods ==========

    /**
     * Finds a BasicNode or SuperNode by ID in the sceneflow.
     */
    public BasicNode findNodeById(EditorProject project, String nodeId) {
        if (project == null || nodeId == null) {
            return null;
        }
        return findNodeInSuperNode(project.getSceneFlow(), nodeId);
    }

    /**
     * Finds a SuperNode by ID in the sceneflow.
     */
    public SuperNode findSuperNodeById(EditorProject project, String nodeId) {
        if (project == null || nodeId == null) {
            return null;
        }

        // Special case: sceneflow root
        if (project.getSceneFlow().getId().equals(nodeId)) {
            return project.getSceneFlow();
        }

        BasicNode node = findNodeInSuperNode(project.getSceneFlow(), nodeId);
        return (node instanceof SuperNode) ? (SuperNode) node : null;
    }

    /**
     * Recursively searches for a node in a SuperNode and its children.
     */
    private BasicNode findNodeInSuperNode(SuperNode parent, String nodeId) {
        if (parent == null || nodeId == null) {
            return null;
        }

        // Check if this is the node we're looking for
        if (parent.getId().equals(nodeId)) {
            return parent;
        }

        // Search in basic nodes
        for (BasicNode node : parent.getNodeList()) {
            if (node.getId().equals(nodeId)) {
                return node;
            }
        }

        // Search in super nodes recursively
        for (SuperNode node : parent.getSuperNodeList()) {
            if (node.getId().equals(nodeId)) {
                return node;
            }
            // Recursively search children
            BasicNode found = findNodeInSuperNode(node, nodeId);
            if (found != null) {
                return found;
            }
        }

        return null;
    }

    /**
     * Checks if a node has any edge pointing to the target node.
     */
    private boolean pointsToNode(BasicNode node, String targetNodeId) {
        if (node.getDedge() != null && node.getDedge().getTargetUnid().equals(targetNodeId)) {
            return true;
        }
        for (GuargedEdge edge : node.getCEdgeList()) {
            if (edge.getTargetUnid().equals(targetNodeId)) return true;
        }
        for (RandomEdge edge : node.getPEdgeList()) {
            if (edge.getTargetUnid().equals(targetNodeId)) return true;
        }
        for (ForkingEdge edge : node.getFEdgeList()) {
            if (edge.getTargetUnid().equals(targetNodeId)) return true;
        }
        for (InterruptEdge edge : node.getIEdgeList()) {
            if (edge.getTargetUnid().equals(targetNodeId)) return true;
        }
        return false;
    }

    /**
     * Deletes all edges from a source node to a target node.
     */
    private void deleteEdgesFromNodeToTarget(EditorProject project, String sourceNodeId, String targetNodeId) {
        BasicNode sourceNode = findNodeById(project, sourceNodeId);
        if (sourceNode == null) return;

        // Check and delete each edge type
        if (sourceNode.getDedge() != null && sourceNode.getDedge().getTargetUnid().equals(targetNodeId)) {
            EdgeType type = (sourceNode.getDedge() instanceof TimeoutEdge)
                ? EdgeType.TIMEOUT_EDGE : EdgeType.EPSILON_EDGE;
            deleteEdge(project, sourceNodeId, targetNodeId, type);
        }

        // Delete CEdges
        for (GuargedEdge edge : new ArrayList<>(sourceNode.getCEdgeList())) {
            if (edge.getTargetUnid().equals(targetNodeId)) {
                deleteEdge(project, sourceNodeId, targetNodeId, EdgeType.CONDITIONAL_EDGE);
            }
        }

        // Delete PEdges
        for (RandomEdge edge : new ArrayList<>(sourceNode.getPEdgeList())) {
            if (edge.getTargetUnid().equals(targetNodeId)) {
                deleteEdge(project, sourceNodeId, targetNodeId, EdgeType.PROBABILISTIC_EDGE);
            }
        }

        // Delete FEdges
        for (ForkingEdge edge : new ArrayList<>(sourceNode.getFEdgeList())) {
            if (edge.getTargetUnid().equals(targetNodeId)) {
                deleteEdge(project, sourceNodeId, targetNodeId, EdgeType.FORKING_EDGE);
            }
        }

        // Delete IEdges
        for (InterruptEdge edge : new ArrayList<>(sourceNode.getIEdgeList())) {
            if (edge.getTargetUnid().equals(targetNodeId)) {
                deleteEdge(project, sourceNodeId, targetNodeId, EdgeType.INTERRUPTIVE_EDGE);
            }
        }
    }

    /**
     * Detects the type of edge from source to target node.
     */
    private EdgeType detectEdgeType(BasicNode sourceNode, String targetNodeId) {
        if (sourceNode.getDedge() != null && sourceNode.getDedge().getTargetUnid().equals(targetNodeId)) {
            return (sourceNode.getDedge() instanceof TimeoutEdge)
                ? EdgeType.TIMEOUT_EDGE : EdgeType.EPSILON_EDGE;
        }

        for (GuargedEdge edge : sourceNode.getCEdgeList()) {
            if (edge.getTargetUnid().equals(targetNodeId)) return EdgeType.CONDITIONAL_EDGE;
        }

        for (RandomEdge edge : sourceNode.getPEdgeList()) {
            if (edge.getTargetUnid().equals(targetNodeId)) return EdgeType.PROBABILISTIC_EDGE;
        }

        for (ForkingEdge edge : sourceNode.getFEdgeList()) {
            if (edge.getTargetUnid().equals(targetNodeId)) return EdgeType.FORKING_EDGE;
        }

        for (InterruptEdge edge : sourceNode.getIEdgeList()) {
            if (edge.getTargetUnid().equals(targetNodeId)) return EdgeType.INTERRUPTIVE_EDGE;
        }

        return null;
    }
}
