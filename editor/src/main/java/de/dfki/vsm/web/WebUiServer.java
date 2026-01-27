package de.dfki.vsm.web;

import de.dfki.vsm.PreferencesDesktop;
import de.dfki.vsm.editor.CmdBadge;
import de.dfki.vsm.editor.Comment;
import de.dfki.vsm.editor.Edge;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.Node;
import de.dfki.vsm.editor.action.CreateCommentAction;
import de.dfki.vsm.editor.action.CreateEdgeAction;
import de.dfki.vsm.editor.action.CreateNodeAction;
import de.dfki.vsm.editor.action.RedoAction;
import de.dfki.vsm.editor.action.RemoveCommentAction;
import de.dfki.vsm.editor.action.RemoveEdgeAction;
import de.dfki.vsm.editor.action.RemoveNodeAction;
import de.dfki.vsm.editor.action.UndoAction;
import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.editor.project.ProjectEditor;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.editor.service.EditorProjectService;
import de.dfki.vsm.editor.service.SceneFlowService;
import de.dfki.vsm.editor.util.SceneFlowManager;
import de.dfki.vsm.model.project.property.ExportableProperties;
import de.dfki.vsm.model.project.property.ProjectProperty;
import de.dfki.vsm.model.acticon.ActiconAction;
import de.dfki.vsm.model.acticon.ActiconConfig;
import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.model.gesticon.GesticonAgent;
import de.dfki.vsm.model.gesticon.GesticonGesture;
import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.model.project.EditorConfig;
import de.dfki.vsm.model.project.PlayerConfig;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.project.ProjectConfig;
import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.badge.CommentBadge;
import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.EpsilonEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.ForkingEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.RandomEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge;
import de.dfki.vsm.model.sceneflow.chart.graphics.comment.CommentBoundary;
import de.dfki.vsm.model.sceneflow.chart.graphics.comment.CommentGraphics;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgeArrow;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgeGraphics;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgePoint;
import de.dfki.vsm.model.sceneflow.chart.graphics.node.NodeGraphics;
import de.dfki.vsm.model.sceneflow.chart.graphics.node.NodePosition;
import de.dfki.vsm.model.sceneflow.glue.GlueParser;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.model.sceneflow.glue.command.definition.ArgumentDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.DataTypeDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.FunctionDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.datatype.ListTypeDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.datatype.MemberDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.datatype.StructTypeDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.BoolLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.FloatLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.IntLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.StringLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.UnaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.record.ArrayExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.record.StructExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.variable.SimpleVariable;
import de.dfki.vsm.model.scenescript.SceneObject;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.model.scenescript.ScriptDiagnostics;
import de.dfki.vsm.model.scenescript.ScriptEntity;
import de.dfki.vsm.model.scenescript.SceneTurn;
import de.dfki.vsm.model.scenescript.SceneUttr;
import de.dfki.vsm.model.scenescript.SceneParam;
import de.dfki.vsm.model.scenescript.UttrElement;
import de.dfki.vsm.model.scenescript.SceneWord;
import de.dfki.vsm.model.scenescript.ActionObject;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.model.scenescript.ActionParam;
import de.dfki.vsm.model.visicon.VisiconAgent;
import de.dfki.vsm.model.visicon.VisiconConfig;
import de.dfki.vsm.model.visicon.VisiconViseme;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.ui.protocol.UiEvent;
import de.dfki.vsm.ui.protocol.UiEventBus;
import de.dfki.vsm.ui.protocol.UiEventListener;
import de.dfki.vsm.ui.protocol.UiChannel;
import de.dfki.vsm.ui.protocol.UiProtocol;
import de.dfki.vsm.util.jpl.JPLEngine;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.tpl.Tuple;
import de.dfki.vsm.util.xml.XMLUtilities;
import de.dfki.vsm.xtesting.NewPropertyManager.exceptions.NotExportableInterface;
import de.dfki.vsm.xtesting.NewPropertyManager.util.ExtensionsFromJar;
import io.javalin.Javalin;
import io.javalin.http.Context;
import io.javalin.http.UnauthorizedResponse;
import io.javalin.http.staticfiles.Location;
import io.javalin.websocket.WsConnectContext;
import io.javalin.websocket.WsContext;
import org.json.JSONArray;
import org.json.JSONObject;
import org.reflections.Reflections;
import org.reflections.scanners.SubTypesScanner;
import org.reflections.util.ConfigurationBuilder;

import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;
import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.UndoManager;
import java.awt.Component;
import java.awt.Point;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.InetAddress;
import java.net.URLConnection;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;
import java.util.jar.Attributes;
import java.util.jar.Manifest;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class WebUiServer implements UiEventListener {

    private static final WebUiServer INSTANCE = new WebUiServer();
    private static final int DEFAULT_PORT = 8090;
    private static final String API_PREFIX = "/api/v1";
    private static final String ROOT_SUPERNODE_ID = "__root__";

    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final SecureRandom mRandom = new SecureRandom();
    private boolean mAllowExternal = false;
    private String mBindHost = "127.0.0.1";
    private final Set<WsContext> mSockets = ConcurrentHashMap.newKeySet();
    private final UiEventBus mUiEventBus = UiProtocol.getEventBus();
    private final ExecutorService mBroadcastExecutor = Executors.newSingleThreadExecutor(r -> {
        Thread thread = new Thread(r, "WebUiServer-Broadcast");
        thread.setDaemon(true);
        return thread;
    });
    private final Map<String, SceneFlowClipboard> mSceneFlowClipboard = new ConcurrentHashMap<>();

    private Javalin mApp;
    private String mToken;
    private int mPort;
    private boolean mStarted;
    private volatile String mLastRuntimeProjectId;
    private volatile List<DeviceEntry> mAvailableDevices;
    private volatile List<String> mExportablePropertyClassNames;

    // Headless services (Phase 6 refactoring)
    private EditorProjectService mEditorProjectService;
    private SceneFlowService mSceneFlowService;
    private String mCurrentProjectId; // Track currently active project

    private static final class DeviceEntry {
        private final String name;
        private final String className;

        private DeviceEntry(String name, String className) {
            this.name = name;
            this.className = className;
        }
    }

    private static final class SceneFlowClipboard {
        private final String projectId;
        private final List<BasicNode> nodes;

        private SceneFlowClipboard(String projectId, List<BasicNode> nodes) {
            this.projectId = projectId;
            this.nodes = nodes;
        }
    }

    private static final class NodeMoveRequest {
        private final String nodeId;
        private final Point target;

        private NodeMoveRequest(String nodeId, Point target) {
            this.nodeId = nodeId;
            this.target = target;
        }
    }

    private static final class VarRenameSnapshot {
        private final Map<BasicNode, List<VariableDefinition>> nodeVarDefsBefore = new LinkedHashMap<>();
        private final Map<BasicNode, List<VariableDefinition>> nodeVarDefsAfter = new LinkedHashMap<>();
        private final Map<BasicNode, List<Command>> nodeCommandsBefore = new LinkedHashMap<>();
        private final Map<BasicNode, List<Command>> nodeCommandsAfter = new LinkedHashMap<>();
        private final Map<AbstractEdge, List<Command>> edgeCommandsBefore = new LinkedHashMap<>();
        private final Map<AbstractEdge, List<Command>> edgeCommandsAfter = new LinkedHashMap<>();
        private final Map<AbstractEdge, Expression> edgeConditionsBefore = new LinkedHashMap<>();
        private final Map<AbstractEdge, Expression> edgeConditionsAfter = new LinkedHashMap<>();
        private final Map<AbstractEdge, Expression> edgeTimeoutsBefore = new LinkedHashMap<>();
        private final Map<AbstractEdge, Expression> edgeTimeoutsAfter = new LinkedHashMap<>();
        private String scriptBefore;
        private String scriptAfter;
        private boolean scriptChanged;

        private boolean hasChanges() {
            return scriptChanged
                    || !nodeVarDefsBefore.isEmpty()
                    || !nodeCommandsBefore.isEmpty()
                    || !edgeCommandsBefore.isEmpty()
                    || !edgeConditionsBefore.isEmpty()
                    || !edgeTimeoutsBefore.isEmpty();
        }
    }

    // Phase 6: Docking point system for edges
    // Track occupied dock points per node - each dock point can only be used by ONE edge endpoint
    // Key format: "nodeId" - single set per node (no in/out separation)
    // This prevents bidirectional edges from overlapping (they must use different dock points)
    private final Map<String, Set<Integer>> mOccupiedDockPoints = new ConcurrentHashMap<>();

    // Dock point index for start sign (left side of node)
    private static final int START_SIGN_DOCK_INDEX = 6;

    // Compute 24 dock points around a node boundary
    // Returns array of {x, y} positions relative to node's top-left corner
    private double[][] computeDockPoints(int nodeWidth, int nodeHeight, boolean isSuperNode) {
        double[][] dockPoints = new double[24][2];
        double halfW = nodeWidth / 2.0;
        double halfH = nodeHeight / 2.0;

        for (int i = 0; i < 24; i++) {
            double angle = i * Math.PI / 12.0 + Math.PI;
            if (isSuperNode) {
                // SuperNode: dock points on rectangular boundary
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
                // BasicNode: dock points on ellipse boundary
                double dockX = Math.round((Math.sin(angle) * 0.5 + 0.5) * nodeWidth);
                double dockY = Math.round((Math.cos(angle) * 0.5 + 0.5) * nodeHeight);
                dockPoints[i][0] = dockX;
                dockPoints[i][1] = dockY;
            }
        }
        return dockPoints;
    }

    // Find best dock point pair for an edge (source dock, target dock) that minimizes edge length
    // Returns int[2] = {sourceDockIndex, targetDockIndex}
    // For self-loops, returns dock points ~90 degrees apart for a nice curve
    private int[] findBestDockPointPair(
            String sourceNodeId, double srcX, double srcY, int srcWidth, int srcHeight, boolean srcIsSuperNode,
            String targetNodeId, double tgtX, double tgtY, int tgtWidth, int tgtHeight, boolean tgtIsSuperNode) {

        // Handle self-loop edges (same source and target node)
        if (sourceNodeId != null && sourceNodeId.equals(targetNodeId)) {
            return findSelfLoopDockPointPair(sourceNodeId, srcWidth, srcHeight, srcIsSuperNode);
        }

        double[][] srcDockPoints = computeDockPoints(srcWidth, srcHeight, srcIsSuperNode);
        double[][] tgtDockPoints = computeDockPoints(tgtWidth, tgtHeight, tgtIsSuperNode);

        // Single set per node - each dock point can only be used by one edge endpoint
        Set<Integer> srcOccupied = mOccupiedDockPoints.computeIfAbsent(sourceNodeId, k -> ConcurrentHashMap.newKeySet());
        Set<Integer> tgtOccupied = mOccupiedDockPoints.computeIfAbsent(targetNodeId, k -> ConcurrentHashMap.newKeySet());

        // DEBUG: Log occupied dock points
        System.out.println(">>> findBestDockPointPair: " + sourceNodeId + " -> " + targetNodeId);
        System.out.println(">>>   srcOccupied[" + sourceNodeId + "] = " + srcOccupied);
        System.out.println(">>>   tgtOccupied[" + targetNodeId + "] = " + tgtOccupied);

        int bestSrcIdx = -1;
        int bestTgtIdx = -1;
        double bestDist = Double.MAX_VALUE;

        // Find the pair of dock points that minimizes edge length
        for (int s = 0; s < 24; s++) {
            if (srcOccupied.contains(s)) continue;

            double srcDockAbsX = srcX + srcDockPoints[s][0];
            double srcDockAbsY = srcY + srcDockPoints[s][1];

            for (int t = 0; t < 24; t++) {
                if (tgtOccupied.contains(t)) continue;

                double tgtDockAbsX = tgtX + tgtDockPoints[t][0];
                double tgtDockAbsY = tgtY + tgtDockPoints[t][1];

                double dist = Math.hypot(tgtDockAbsX - srcDockAbsX, tgtDockAbsY - srcDockAbsY);

                if (dist < bestDist) {
                    bestDist = dist;
                    bestSrcIdx = s;
                    bestTgtIdx = t;
                }
            }
        }

        // Fallback if all dock points are occupied
        if (bestSrcIdx < 0) bestSrcIdx = 0;
        if (bestTgtIdx < 0) bestTgtIdx = 0;

        System.out.println(">>>   SELECTED: srcDock=" + bestSrcIdx + ", tgtDock=" + bestTgtIdx);

        return new int[] { bestSrcIdx, bestTgtIdx };
    }

    // Find dock points for self-loop edges that create a nice upward loop
    // Dock point positions (based on angle = i * π/12 + π):
    //   0 = TOP center, 6 = LEFT center, 12 = BOTTOM center, 18 = RIGHT center
    // Going clockwise from top: 0=top, 3=upper-left, 6=left, 9=lower-left,
    //   12=bottom, 15=lower-right, 18=right, 21=upper-right
    // For a nice upward loop, we want dock points near the TOP (around 0, with 21 on right, 3 on left)
    private int[] findSelfLoopDockPointPair(String nodeId, int nodeWidth, int nodeHeight, boolean isSuperNode) {
        // Single set per node - both self-loop endpoints use dock points from same set
        Set<Integer> occupied = mOccupiedDockPoints.computeIfAbsent(nodeId, k -> ConcurrentHashMap.newKeySet());

        // For an upward loop like in the nice example:
        // - Start point on upper-right (around 1-2 o'clock) = dock 21 or 22
        // - End point on upper-left (around 10-11 o'clock) = dock 3 or 2
        // This creates a loop that curves UP and over the top of the node

        // Preferred pairs for upward loops (start, end):
        // Each pair has start on upper-right, end on upper-left, loop goes UP
        int[][] preferredPairs = {
            {21, 3},   // Upper-right to upper-left (primary - loop goes up)
            {22, 2},   // Slightly closer to top
            {20, 4},   // Slightly more to the sides
            {23, 1},   // Very close to top
            {19, 5},   // More toward right/left sides
            {3, 21},   // Reversed direction
        };

        for (int[] pair : preferredPairs) {
            int startIdx = pair[0];
            int endIdx = pair[1];

            // Check if both positions are available (both in same set now)
            if (!occupied.contains(startIdx) && !occupied.contains(endIdx)) {
                return new int[] { startIdx, endIdx };
            }
        }

        // Fallback: find any pair in upper half with reasonable separation
        for (int s = 19; s <= 23; s++) { // Upper-right quadrant
            if (occupied.contains(s)) continue;

            for (int offset = 6; offset >= 4; offset--) {
                int t = (s + offset) % 24; // Upper-left quadrant
                if (!occupied.contains(t) && s != t) {
                    return new int[] { s, t };
                }
            }
        }

        // Last fallback
        return new int[] { 21, 3 };
    }

    // Mark a dock point as occupied
    // isSource parameter kept for API compatibility but ignored (single set per node)
    private void occupyDockPoint(String nodeId, int dockIndex, boolean isSource) {
        Set<Integer> set = mOccupiedDockPoints.computeIfAbsent(nodeId, k -> ConcurrentHashMap.newKeySet());
        set.add(dockIndex);
        System.out.println(">>> occupyDockPoint: nodeId=" + nodeId + ", dockIndex=" + dockIndex + " -> set now: " + set);
    }

    // Release a dock point
    // isSource parameter kept for API compatibility but ignored (single set per node)
    private void releaseDockPoint(String nodeId, int dockIndex, boolean isSource) {
        Set<Integer> occupied = mOccupiedDockPoints.get(nodeId);
        if (occupied != null) {
            occupied.remove(dockIndex);
        }
    }

    // Get dock point absolute position
    private double[] getDockPointPosition(double nodeX, double nodeY, int nodeWidth, int nodeHeight,
                                           boolean isSuperNode, int dockIndex) {
        double[][] dockPoints = computeDockPoints(nodeWidth, nodeHeight, isSuperNode);
        if (dockIndex >= 0 && dockIndex < dockPoints.length) {
            return new double[] { nodeX + dockPoints[dockIndex][0], nodeY + dockPoints[dockIndex][1] };
        }
        // Fallback to node center
        return new double[] { nodeX + nodeWidth / 2.0, nodeY + nodeHeight / 2.0 };
    }

    // Initialize dock points for all edges in a project
    // Call this when a project is loaded to mark existing dock points as occupied
    private void initializeDockPointsForProject(EditorProject project) {
        if (project == null) return;

        SceneFlow sceneFlow = project.getSceneFlow();
        if (sceneFlow == null) return;

        EditorConfig config = project.getEditorConfig();
        int nodeWidth = config != null ? config.sNODEWIDTH : 90;
        int nodeHeight = config != null ? config.sNODEHEIGHT : 90;

        // Recursively scan all nodes and their edges
        initializeDockPointsRecursive(sceneFlow, nodeWidth, nodeHeight);
    }

    private void initializeDockPointsRecursive(SuperNode superNode, int nodeWidth, int nodeHeight) {
        // Process all nodes in this super node
        List<BasicNode> allNodes = new ArrayList<>();
        allNodes.addAll(superNode.getNodeList());
        allNodes.addAll(superNode.getSuperNodeList());

        // Mark start sign dock points for start nodes
        Map<String, BasicNode> startNodeMap = superNode.getStartNodeMap();
        if (startNodeMap != null) {
            for (BasicNode startNode : startNodeMap.values()) {
                if (startNode != null) {
                    occupyStartSignDockPoint(startNode.getId());
                }
            }
        }

        for (BasicNode node : allNodes) {
            // Process all edges from this node
            processNodeEdgesForDocking(node, nodeWidth, nodeHeight);

            // Recurse into super nodes
            if (node instanceof SuperNode) {
                initializeDockPointsRecursive((SuperNode) node, nodeWidth, nodeHeight);
            }
        }
    }

    private void processNodeEdgesForDocking(BasicNode node, int nodeWidth, int nodeHeight) {
        // Get node position
        NodeGraphics nodeGraphics = node.getGraphics();
        NodePosition nodePos = nodeGraphics != null ? nodeGraphics.getPosition() : null;
        double nodeX = nodePos != null ? nodePos.getXPos() : 0;
        double nodeY = nodePos != null ? nodePos.getYPos() : 0;
        boolean isSuperNode = node instanceof SuperNode;

        // Collect all edges from this node
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

            // Mark source dock point as occupied (isSource=true for outgoing)
            EdgePoint startPt = points.get(0);
            int srcDockIdx = findDockPointIndex(nodeX, nodeY, nodeWidth, nodeHeight,
                    isSuperNode, startPt.getXPos(), startPt.getYPos());
            if (srcDockIdx >= 0) {
                occupyDockPoint(node.getId(), srcDockIdx, true);
            }

            // Mark target dock point as occupied (isSource=false for incoming)
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
                }
            }
        }
    }

    // Clear dock points for a project (call when project is closed)
    private void clearDockPointsForProject(SceneFlow sceneFlow) {
        if (sceneFlow == null) return;
        clearDockPointsRecursive(sceneFlow);
    }

    private void clearDockPointsRecursive(SuperNode superNode) {
        // Remove dock point set for this node (single set per node)
        mOccupiedDockPoints.remove(superNode.getId());

        for (BasicNode node : superNode.getNodeList()) {
            mOccupiedDockPoints.remove(node.getId());
        }

        for (SuperNode child : superNode.getSuperNodeList()) {
            mOccupiedDockPoints.remove(child.getId());
            clearDockPointsRecursive(child);
        }
    }

    // Find the dock point index that matches a given absolute position
    private int findDockPointIndex(double nodeX, double nodeY, int nodeWidth, int nodeHeight,
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

        // Only return if within reasonable tolerance (e.g., 20 pixels)
        return bestDist < 20 ? bestIdx : -1;
    }

    // Mark start sign dock point as occupied for a start node
    private void occupyStartSignDockPoint(String nodeId) {
        mOccupiedDockPoints.computeIfAbsent(nodeId, k -> ConcurrentHashMap.newKeySet()).add(START_SIGN_DOCK_INDEX);
    }

    // Release start sign dock point when node is no longer a start node
    private void releaseStartSignDockPoint(String nodeId) {
        Set<Integer> occupied = mOccupiedDockPoints.get(nodeId);
        if (occupied != null) {
            occupied.remove(START_SIGN_DOCK_INDEX);
        }
    }

    // Find the nearest available (unoccupied) dock point to a given position
    // Returns the dock index, or -1 if all are occupied
    private int findNearestAvailableDockPoint(String nodeId, double nodeX, double nodeY,
            int nodeWidth, int nodeHeight, boolean isSuperNode, double targetX, double targetY) {
        double[][] dockPoints = computeDockPoints(nodeWidth, nodeHeight, isSuperNode);
        Set<Integer> occupied = mOccupiedDockPoints.getOrDefault(nodeId, ConcurrentHashMap.newKeySet());

        int bestIdx = -1;
        double bestDist = Double.MAX_VALUE;

        for (int i = 0; i < dockPoints.length; i++) {
            if (occupied.contains(i)) continue; // Skip occupied dock points

            double dockAbsX = nodeX + dockPoints[i][0];
            double dockAbsY = nodeY + dockPoints[i][1];
            double dist = Math.hypot(dockAbsX - targetX, dockAbsY - targetY);
            if (dist < bestDist) {
                bestDist = dist;
                bestIdx = i;
            }
        }

        return bestIdx;
    }

    // Update edge endpoints when a node is moved
    // Uses delta-based movement to shift edge endpoints by the same amount the node moved
    private void updateEdgeEndpointsForMovedNode(BasicNode movedNode, SuperNode parent, EditorConfig config, int oldX, int oldY) {
        if (movedNode == null || parent == null) return;

        NodeGraphics nodeGraphics = movedNode.getGraphics();
        NodePosition nodePos = nodeGraphics != null ? nodeGraphics.getPosition() : null;
        int newX = nodePos != null ? nodePos.getXPos() : 0;
        int newY = nodePos != null ? nodePos.getYPos() : 0;

        // Calculate movement delta
        int deltaX = newX - oldX;
        int deltaY = newY - oldY;

        // Skip if no actual movement
        if (deltaX == 0 && deltaY == 0) return;

        String nodeId = movedNode.getId();

        // Update outgoing edges (edges FROM this node) - shift start points
        for (AbstractEdge edge : movedNode.getEdgeList()) {
            EdgeGraphics edgeGraphics = edge.getGraphics();
            if (edgeGraphics == null) continue;
            EdgeArrow arrow = edgeGraphics.getConnection();
            if (arrow == null) continue;
            List<EdgePoint> points = arrow.getPointList();
            if (points == null || points.isEmpty()) continue;

            EdgePoint startPt = points.get(0);
            // Shift both position and control point by delta
            startPt.setXPos(startPt.getXPos() + deltaX);
            startPt.setYPos(startPt.getYPos() + deltaY);
            startPt.setCtrlXPos(startPt.getCtrlXPos() + deltaX);
            startPt.setCtrlYPos(startPt.getCtrlYPos() + deltaY);
        }

        // Update incoming edges (edges TO this node) - shift end points
        for (BasicNode otherNode : parent.getNodeAndSuperNodeList()) {
            for (AbstractEdge edge : otherNode.getEdgeList()) {
                String targetId = edge.getTargetUnid();
                if (!nodeId.equals(targetId)) continue;

                EdgeGraphics edgeGraphics = edge.getGraphics();
                if (edgeGraphics == null) continue;
                EdgeArrow arrow = edgeGraphics.getConnection();
                if (arrow == null) continue;
                List<EdgePoint> points = arrow.getPointList();
                if (points == null || points.size() < 2) continue;

                EdgePoint endPt = points.get(points.size() - 1);
                // Shift both position and control point by delta
                endPt.setXPos(endPt.getXPos() + deltaX);
                endPt.setYPos(endPt.getYPos() + deltaY);
                endPt.setCtrlXPos(endPt.getCtrlXPos() + deltaX);
                endPt.setCtrlYPos(endPt.getCtrlYPos() + deltaY);
            }
        }
    }

    // Compute bezier control points using the Swing UI algorithm
    // Control point = node_center + scalingFactor * (dock_point - node_center)
    // This naturally creates curves that bow outward from each node
    private double[] computeInitialControlPoints(
            double srcDockX, double srcDockY, double srcCenterX, double srcCenterY,
            double tgtDockX, double tgtDockY, double tgtCenterX, double tgtCenterY,
            double nodeHeight, boolean isSelfLoop) {

        // Vector from source node center to source dock point
        double srcVecX = srcDockX - srcCenterX;
        double srcVecY = srcDockY - srcCenterY;

        // Vector from target node center to target dock point
        double tgtVecX = tgtDockX - tgtCenterX;
        double tgtVecY = tgtDockY - tgtCenterY;

        // Scale based on distance between node centers (same as Swing UI)
        double distance = Math.hypot(tgtCenterX - srcCenterX, tgtCenterY - srcCenterY);
        double scalingFactor = isSelfLoop ? 3.0 : ((distance / nodeHeight) - 0.5);
        if (scalingFactor < 1.25) {
            scalingFactor = 1.25;
        }

        // Control point 1: source center + scaled vector to source dock
        double ctrl1X = srcCenterX + scalingFactor * srcVecX;
        double ctrl1Y = srcCenterY + scalingFactor * srcVecY;

        // Control point 2: target center + scaled vector to target dock
        double ctrl2X = tgtCenterX + scalingFactor * tgtVecX;
        double ctrl2Y = tgtCenterY + scalingFactor * tgtVecY;

        // Sanitize: ensure control points are not too close to origin
        double minPos = 15;
        ctrl1X = Math.max(ctrl1X, minPos);
        ctrl1Y = Math.max(ctrl1Y, minPos);
        ctrl2X = Math.max(ctrl2X, minPos);
        ctrl2Y = Math.max(ctrl2Y, minPos);

        return new double[] { ctrl1X, ctrl1Y, ctrl2X, ctrl2Y };
    }

    // Legacy method for backward compatibility
    private double[] computeInitialControlPoint(double startX, double startY, double endX, double endY, boolean isStart) {
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

    // Compute control points for self-loop edges that curve outward from the node
    // Creates a nice smooth loop that extends above the node
    // Based on empirically derived good values from manual adjustment
    // Returns double[4] = {startCtrlX, startCtrlY, endCtrlX, endCtrlY}
    private double[] computeSelfLoopControlPoints(
            double startX, double startY, double endX, double endY,
            double nodeCenterX, double nodeCenterY, int nodeWidth, int nodeHeight) {

        // Empirically derived ratios from good manual adjustment:
        // - Start control: goes UP (negative Y) and outward (right for upper-right dock)
        // - End control: goes UP (negative Y) and slightly toward center
        // These ratios create a nice rounded loop above the node

        double nodeSize = Math.max(nodeWidth, nodeHeight);

        // Start control: UP and outward from the start point
        // Offset ratios: X = +0.85 * nodeSize (outward), Y = -0.76 * nodeSize (up)
        double startCtrlX = startX + 0.85 * nodeSize;
        double startCtrlY = startY - 0.76 * nodeSize;

        // End control: UP and slightly toward center
        // Offset ratios: X = +0.16 * nodeSize (toward center), Y = -0.87 * nodeSize (up)
        double endCtrlX = endX + 0.16 * nodeSize;
        double endCtrlY = endY - 0.87 * nodeSize;

        return new double[] { startCtrlX, startCtrlY, endCtrlX, endCtrlY };
    }

    // Check if this is a self-loop edge (same source and target node)
    private boolean isSelfLoopEdge(String sourceNodeId, String targetNodeId) {
        return sourceNodeId != null && sourceNodeId.equals(targetNodeId);
    }

    // Normalize an edge - recalculate control points to create a smooth curve
    // Uses Swing-style algorithm with node centers for proper bowing
    private void normalizeEdge(AbstractEdge edge, EditorConfig config) {
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

        int nodeWidth = config != null ? config.sNODEWIDTH : 90;
        int nodeHeight = config != null ? config.sNODEHEIGHT : 90;

        // Check if it's a self-loop
        String sourceId = edge.getSourceUnid();
        String targetId = edge.getTargetUnid();
        if (isSelfLoopEdge(sourceId, targetId)) {
            // For self-loops, use the special control point calculation
            BasicNode sourceNode = edge.getSourceNode();
            if (sourceNode != null) {
                NodeGraphics nodeGraphics = sourceNode.getGraphics();
                NodePosition nodePos = nodeGraphics != null ? nodeGraphics.getPosition() : null;
                double nodeX = nodePos != null ? nodePos.getXPos() : 0;
                double nodeY = nodePos != null ? nodePos.getYPos() : 0;
                double nodeCenterX = nodeX + nodeWidth / 2.0;
                double nodeCenterY = nodeY + nodeHeight / 2.0;

                double[] loopCtrl = computeSelfLoopControlPoints(startX, startY, endX, endY,
                        nodeCenterX, nodeCenterY, nodeWidth, nodeHeight);
                startPt.setCtrlXPos((int) Math.round(loopCtrl[0]));
                startPt.setCtrlYPos((int) Math.round(loopCtrl[1]));
                endPt.setCtrlXPos((int) Math.round(loopCtrl[2]));
                endPt.setCtrlYPos((int) Math.round(loopCtrl[3]));
            }
        } else {
            // Normal edge - use Swing-style algorithm with node centers
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

                // Use Swing algorithm: control points based on vectors from centers to dock points
                double[] allCtrl = computeInitialControlPoints(
                        startX, startY, srcCenterX, srcCenterY,
                        endX, endY, tgtCenterX, tgtCenterY,
                        nodeHeight, false);
                startPt.setCtrlXPos((int) Math.round(allCtrl[0]));
                startPt.setCtrlYPos((int) Math.round(allCtrl[1]));
                endPt.setCtrlXPos((int) Math.round(allCtrl[2]));
                endPt.setCtrlYPos((int) Math.round(allCtrl[3]));
            }
        }
    }

    // Straighten an edge - make control points lie on the straight line between start and end
    // Note: Self-loops and bidirectional edges cannot be straightened (would overlap)
    private void straightenEdge(AbstractEdge edge) {
        if (edge == null) return;

        // Skip self-loops - straightening them would collapse the curve
        String sourceId = edge.getSourceUnid();
        String targetId = edge.getTargetUnid();
        if (isSelfLoopEdge(sourceId, targetId)) {
            return;
        }

        // Skip if there's a reverse edge (bidirectional pair) - straightening would overlap
        BasicNode targetNode = edge.getTargetNode();
        if (targetNode != null) {
            for (AbstractEdge reverseEdge : targetNode.getEdgeList()) {
                if (sourceId.equals(reverseEdge.getTargetUnid())) {
                    // Found reverse edge - skip straightening
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

        // For a straight line, control points are at 1/3 and 2/3 along the line
        double dx = endX - startX;
        double dy = endY - startY;

        double startCtrlX = startX + dx / 3.0;
        double startCtrlY = startY + dy / 3.0;
        double endCtrlX = startX + 2.0 * dx / 3.0;
        double endCtrlY = startY + 2.0 * dy / 3.0;

        startPt.setCtrlXPos((int) Math.round(startCtrlX));
        startPt.setCtrlYPos((int) Math.round(startCtrlY));
        endPt.setCtrlXPos((int) Math.round(endCtrlX));
        endPt.setCtrlYPos((int) Math.round(endCtrlY));
    }

    public static WebUiServer getInstance() {
        return INSTANCE;
    }

    public synchronized void setAllowExternal(boolean allowExternal) {
        mAllowExternal = allowExternal;
    }

    public synchronized String getLocalUrl() {
        return "http://127.0.0.1:" + mPort + "/";
    }

    public synchronized void start() {
        if (mStarted) {
            return;
        }

        mPort = Integer.parseInt(System.getProperty("vsm.web.port", Integer.toString(DEFAULT_PORT)));
        mToken = resolveToken();
        mBindHost = mAllowExternal ? "0.0.0.0" : "127.0.0.1";

        mApp = Javalin.create(config -> {
            if (WebUiServer.class.getResource("/web-ui") != null) {
                config.addStaticFiles("/web-ui", Location.CLASSPATH);
            } else {
                mLogger.warning("Warning: Web UI static resources not found on classpath (/web-ui).");
            }
        });

        loadAvailableDeviceCache();
        loadExportablePropertyCache();

        // Initialize headless services (Phase 6 refactoring)
        mEditorProjectService = new EditorProjectService();
        mSceneFlowService = new SceneFlowService();

        mApp.before(ctx -> {
            if (requiresAuth(ctx.path())) {
                if (!isAuthorized(ctx)) {
                    throw new UnauthorizedResponse("Missing or invalid token");
                }
            }
        });

        registerRoutes();
        registerWebSocket();

        mApp.start(mBindHost, mPort);
        mStarted = true;
        mUiEventBus.addListener(this);
        updateUiProtocolActive();

        logStartup();

        Runtime.getRuntime().addShutdownHook(new Thread(this::stop, "WebUiServer-Shutdown"));
    }

    public synchronized void stop() {
        if (!mStarted || mApp == null) {
            return;
        }
        mUiEventBus.removeListener(this);
        mUiEventBus.setActive(false);
        mApp.stop();
        mBroadcastExecutor.shutdownNow();
        mStarted = false;
    }

    @Override
    public void onEvent(UiEvent event) {
        if (event == null) {
            return;
        }
        JSONObject message = new JSONObject();
        message.put("type", "event");
        message.put("v", event.getVersion());
        if (event.getId() != null && !event.getId().isBlank()) {
            message.put("id", event.getId());
        }
        if (event.getChannel() != null) {
            message.put("channel", event.getChannel().name().toLowerCase(Locale.ROOT));
        }
        message.put("event", event.getEvent());
        message.put("ts", event.getTimestamp());
        message.put("seq", event.getSequence());
        Object payload = event.getPayload();
        if (event.getChannel() == UiChannel.RUNTIME) {
            payload = attachProjectId(payload, mLastRuntimeProjectId);
        }
        message.put("payload", wrapPayload(payload));
        broadcast(message);
    }

    private Object wrapPayload(Object payload) {
        if (payload == null) {
            return new JSONObject();
        }
        Object wrapped = JSONObject.wrap(payload);
        return wrapped == null ? new JSONObject() : wrapped;
    }

    private Object attachProjectId(Object payload, String projectId) {
        if (projectId == null || projectId.isBlank()) {
            return payload;
        }
        if (payload instanceof JSONObject) {
            JSONObject json = new JSONObject();
            JSONObject existing = (JSONObject) payload;
            for (String key : existing.keySet()) {
                json.put(key, existing.get(key));
            }
            if (!json.has("projectId")) {
                json.put("projectId", projectId);
            }
            return json;
        }
        if (payload instanceof Map) {
            Map<?, ?> raw = (Map<?, ?>) payload;
            Map<String, Object> map = new LinkedHashMap<>();
            for (Map.Entry<?, ?> entry : raw.entrySet()) {
                if (entry.getKey() != null) {
                    map.put(entry.getKey().toString(), entry.getValue());
                }
            }
            map.putIfAbsent("projectId", projectId);
            return map;
        }
        if (payload == null) {
            return new JSONObject().put("projectId", projectId);
        }
        JSONObject json = new JSONObject();
        json.put("projectId", projectId);
        json.put("value", payload);
        return json;
    }

    private void updateUiProtocolActive() {
        mUiEventBus.setActive(!mSockets.isEmpty());
    }

    private void emitUiProjectState(String projectId) {
        if (projectId == null || projectId.isBlank()) {
            return;
        }
        emitUiProjectConfig(projectId);
        emitUiScriptSnapshot(projectId);
        emitUiScriptElements(projectId);
        emitUiSceneFlowSnapshot(projectId, null);
    }

    private void emitUiProjectLoaded(JSONObject projectJson) {
        if (projectJson == null) {
            return;
        }
        JSONObject payload = new JSONObject();
        payload.put("project", projectSummary(projectJson));
        mUiEventBus.emitLazy(() -> UiEvent.create(UiChannel.PROJECT, "project.loaded", payload));
    }

    private void emitUiProjectSaved(JSONObject projectJson) {
        if (projectJson == null) {
            return;
        }
        JSONObject payload = new JSONObject();
        String path = projectPath(projectJson);
        if (path == null) {
            payload.put("path", JSONObject.NULL);
        } else {
            payload.put("path", path);
        }
        payload.put("dirty", projectJson.optBoolean("dirty", false));
        String projectId = projectJson.optString("projectId", "");
        if (!projectId.isBlank()) {
            payload.put("projectId", projectId);
        }
        mUiEventBus.emitLazy(() -> UiEvent.create(UiChannel.PROJECT, "project.saved", payload));
    }

    private void emitUiProjectClosed(String projectId) {
        if (projectId == null || projectId.isBlank()) {
            return;
        }
        JSONObject payload = new JSONObject();
        payload.put("projectId", projectId);
        mUiEventBus.emitLazy(() -> UiEvent.create(UiChannel.PROJECT, "project.closed", payload));
    }

    private void emitUiProjectDirty(String projectId, boolean dirty, List<String> areas) {
        JSONObject payload = new JSONObject();
        payload.put("dirty", dirty);
        if (projectId != null && !projectId.isBlank()) {
            payload.put("projectId", projectId);
        }
        if (areas != null && !areas.isEmpty()) {
            payload.put("areas", areas);
        }
        mUiEventBus.emitLazy(() -> UiEvent.create(UiChannel.PROJECT, "project.dirty", payload));
    }

    private void emitUiPreferences(JSONObject response) {
        if (response == null) {
            return;
        }
        mUiEventBus.emitLazy(() -> UiEvent.create(UiChannel.SYSTEM, "system.preferences", response));
    }

    private JSONObject projectSummary(JSONObject projectJson) {
        JSONObject summary = new JSONObject();
        if (projectJson == null) {
            return summary;
        }
        String projectId = projectJson.optString("projectId", "");
        if (!projectId.isBlank()) {
            summary.put("id", projectId);
        }
        String name = projectJson.optString("name", "");
        if (!name.isBlank()) {
            summary.put("name", name);
        }
        String path = projectPath(projectJson);
        if (path == null) {
            summary.put("path", JSONObject.NULL);
        } else {
            summary.put("path", path);
        }
        summary.put("dirty", projectJson.optBoolean("dirty", false));
        return summary;
    }

    private String projectPath(JSONObject projectJson) {
        if (projectJson == null || projectJson.isNull("path")) {
            return null;
        }
        String path = projectJson.optString("path", null);
        if (path == null || path.isBlank()) {
            return null;
        }
        return path;
    }

    private void emitUiProjectConfig(String projectId) {
        if (projectId == null || projectId.isBlank()) {
            return;
        }
        mUiEventBus.emitLazy(() -> {
            JSONObject payload = callOnEdt(() -> {
                // Phase 6: Use EditorProjectService instead of EditorInstance
                EditorProject project = mEditorProjectService.getProject(projectId);
                if (project == null) {
                    return null;
                }
                JSONObject response = projectConfigToJson(project);
                response.put("projectId", projectId);
                return response;
            });
            if (payload == null) {
                return null;
            }
            return UiEvent.create(UiChannel.PROJECT, "project.config", payload);
        });
    }

    private void emitUiScriptSnapshot(String projectId) {
        if (projectId == null || projectId.isBlank()) {
            return;
        }
        mUiEventBus.emitLazy(() -> {
            JSONObject payload = callOnEdt(() -> {
                // Phase 6: Use EditorProjectService instead of EditorInstance
                EditorProject project = mEditorProjectService.getProject(projectId);
                if (project == null) {
                    return null;
                }
                return scriptSnapshotToJson(project, projectId);
            });
            if (payload == null) {
                return null;
            }
            return UiEvent.create(UiChannel.SCRIPT, "script.snapshot", payload);
        });
    }

    private void emitUiScriptElements(String projectId) {
        if (projectId == null || projectId.isBlank()) {
            return;
        }
        mUiEventBus.emitLazy(() -> {
            JSONObject payload = callOnEdt(() -> {
                // Phase 6: Use EditorProjectService instead of EditorInstance
                EditorProject project = mEditorProjectService.getProject(projectId);
                if (project == null) {
                    return null;
                }
                JSONObject elements = scriptElementsToJson(project);
                JSONObject response = new JSONObject();
                response.put("projectId", projectId);
                response.put("elements", elements);
                return response;
            });
            if (payload == null) {
                return null;
            }
            return UiEvent.create(UiChannel.SCRIPT, "script.elements", payload);
        });
    }

    // Phase 6: Headless version of emitUiSceneFlowSnapshot
    private void emitUiSceneFlowSnapshot(String projectId, String superNodeId) {
        if (projectId == null || projectId.isBlank()) {
            return;
        }
        mUiEventBus.emitLazy(() -> {
            JSONObject payload = callOnEdt(() -> {
                EditorProject project = mEditorProjectService.getProject(projectId);
                if (project == null) {
                    return null;
                }
                SceneFlow sceneFlow = project.getSceneFlow();
                SuperNode superNode = resolveSuperNode(sceneFlow, superNodeId);
                if (superNode == null) {
                    superNode = sceneFlow; // Default to root
                }
                return sceneFlowSnapshot(project, projectId, superNode);
            });
            if (payload == null) {
                return null;
            }
            return UiEvent.create(UiChannel.SCENEFLOW, "sceneflow.snapshot", payload);
        });
    }

    private void emitUiSceneFlowSnapshot(JSONObject snapshot) {
        if (snapshot == null) {
            return;
        }
        mUiEventBus.emitLazy(() -> UiEvent.create(UiChannel.SCENEFLOW, "sceneflow.snapshot", snapshot));
    }

    private void emitUiRuntimeState(String projectId) {
        JSONObject payload = callOnEdt(() -> runtimeStatePayload(projectId));
        if (payload == null) {
            return;
        }
        mUiEventBus.emitLazy(() -> UiEvent.create(UiChannel.RUNTIME, "runtime.state", payload));
    }

    private JSONObject runtimeStatePayload(String projectId) {
        if (projectId == null || projectId.isBlank()) {
            return null;
        }
        // Phase 6: Use EditorProjectService instead of EditorInstance
        EditorProject project = mEditorProjectService.getProject(projectId);
        if (project == null) {
            return null;
        }
        String status = project.isRunning()
                ? (project.isPaused() ? "paused" : "running")
                : "stopped";
        JSONObject payload = new JSONObject();
        payload.put("projectId", projectId);
        payload.put("status", status);
        return payload;
    }

    public String getToken() {
        return mToken;
    }

    private void registerRoutes() {
        mApp.get("/images/*", this::handleImage);
        mApp.get(API_PREFIX + "/token", this::handleToken);
        mApp.get(API_PREFIX + "/info", this::handleInfo);

        mApp.get(API_PREFIX + "/projects", this::handleProjectsList);
        mApp.get(API_PREFIX + "/projects/recent", this::handleRecentProjects);
        mApp.post(API_PREFIX + "/projects/recent/remove", this::handleRecentProjectRemove);
        mApp.get(API_PREFIX + "/projects/samples", ctx -> handleStaticProjectList(ctx, PreferencesDesktop.sSAMPLE_PROJECTS));
        mApp.get(API_PREFIX + "/projects/tutorials", ctx -> handleStaticProjectList(ctx, PreferencesDesktop.sTUTORIALS_PROJECTS));
        mApp.post(API_PREFIX + "/projects/open", this::handleOpenProject);
        mApp.post(API_PREFIX + "/projects", this::handleNewProject);
        mApp.post(API_PREFIX + "/projects/{id}/save", this::handleSaveProject);
        mApp.post(API_PREFIX + "/projects/{id}/save-as", this::handleSaveAsProject);
        mApp.post(API_PREFIX + "/projects/{id}/close", this::handleCloseProject);
        mApp.get(API_PREFIX + "/projects/{id}", this::handleProjectDetail);

        mApp.get(API_PREFIX + "/projects/{id}/sceneflow", this::handleSceneFlow);
        mApp.post(API_PREFIX + "/projects/{id}/sceneflow/navigate", this::handleSceneFlowNavigate);

        mApp.get(API_PREFIX + "/projects/{id}/script", this::handleScript);
        mApp.get(API_PREFIX + "/projects/{id}/script/scenes", this::handleScriptScenes);
        mApp.get(API_PREFIX + "/projects/{id}/script/elements", this::handleScriptElements);
        mApp.post(API_PREFIX + "/projects/{id}/script/diagnostics", this::handleScriptDiagnostics);

        mApp.get(API_PREFIX + "/projects/{id}/functions", this::handleFunctions);
        mApp.get(API_PREFIX + "/projects/{id}/types", this::handleTypes);
        mApp.get(API_PREFIX + "/projects/{id}/config", this::handleConfig);
        mApp.get(API_PREFIX + "/projects/{id}/project-config", this::handleProjectConfig);
        mApp.get(API_PREFIX + "/projects/{id}/project-config/keys", this::handleProjectConfigKeys);
        mApp.get(API_PREFIX + "/devices", this::handleAvailableDevices);
        mApp.get(API_PREFIX + "/preferences", this::handlePreferences);
        mApp.get(API_PREFIX + "/projects/{id}/runtime", this::handleRuntime);

        mApp.get(API_PREFIX + "/fs/roots", this::handleFsRoots);
        mApp.get(API_PREFIX + "/fs/list", this::handleFsList);

        mApp.get("/", ctx -> ctx.redirect("/index.html"));
    }

    private void handleImage(Context ctx) {
        String path = ctx.path();
        if (path == null || !path.startsWith("/images/") || path.contains("..")) {
            ctx.status(404);
            return;
        }
        try (InputStream input = WebUiServer.class.getResourceAsStream(path)) {
            if (input == null) {
                ctx.status(404);
                return;
            }
            byte[] bytes = input.readAllBytes();
            String contentType = URLConnection.guessContentTypeFromName(path);
            if (contentType == null && path.endsWith(".svg")) {
                contentType = "image/svg+xml";
            }
            if (contentType != null) {
                ctx.contentType(contentType);
            }
            ctx.result(new ByteArrayInputStream(bytes));
        } catch (IOException exc) {
            ctx.status(500);
        }
    }

    private void registerWebSocket() {
        mApp.ws("/ws", ws -> {
            ws.onConnect(ctx -> {
                if (!isAuthorized(ctx)) {
                    ctx.session.close(1008, "Unauthorized");
                    return;
                }
                mSockets.add(ctx);
                updateUiProtocolActive();
            });
            ws.onClose(ctx -> {
                mSockets.remove(ctx);
                updateUiProtocolActive();
            });
            ws.onError(ctx -> {
                mSockets.remove(ctx);
                updateUiProtocolActive();
            });
            ws.onMessage(ctx -> handleWsMessage(ctx, ctx.message()));
        });
    }

    private void handleInfo(Context ctx) {
        JSONObject json = new JSONObject();
        json.put("name", "VisualSceneMaker");
        json.put("port", mPort);
        json.put("tokenRequired", mToken != null && !mToken.isEmpty());
        String version = WebUiServer.class.getPackage().getImplementationVersion();
        String build = manifestAttribute("build");
        String revision = manifestAttribute("Build-Revision");
        String buildDate = manifestAttribute("Build-Date");
        json.put("version", version);
        json.put("build", build != null ? build : version);
        if (revision != null) {
            json.put("revision", revision);
            json.put("buildRevision", revision);
        }
        if (buildDate != null) {
            json.put("buildDate", buildDate);
        }
        writeJson(ctx, json);
    }

    private void handleToken(Context ctx) {
        if (!isLocalRequest(ctx)) {
            writeError(ctx, 403, "FORBIDDEN", "Token is only available on localhost");
            return;
        }
        JSONObject json = new JSONObject();
        json.put("token", mToken != null ? mToken : "");
        json.put("tokenRequired", mToken != null && !mToken.isEmpty());
        writeJson(ctx, json);
    }

    private String manifestAttribute(String name) {
        if (name == null || name.isBlank()) {
            return null;
        }
        try (InputStream input = WebUiServer.class.getResourceAsStream("/META-INF/MANIFEST.MF")) {
            if (input == null) {
                return null;
            }
            Manifest manifest = new Manifest(input);
            Attributes attrs = manifest.getMainAttributes();
            String value = attrs.getValue(name);
            return (value == null || value.isBlank()) ? null : value;
        } catch (IOException exc) {
            return null;
        }
    }

    private void handleProjectsList(Context ctx) {
        // Phase 6: Use EditorProjectService instead of EditorInstance
        JSONArray list = callOnEdt(() -> {
            JSONArray projects = new JSONArray();
            Map<String, EditorProject> openProjects = mEditorProjectService.getOpenProjects();
            for (Map.Entry<String, EditorProject> entry : openProjects.entrySet()) {
                String projectId = entry.getKey();
                EditorProject project = entry.getValue();
                projects.put(projectToJson(project, projectId));
            }
            return projects;
        });
        JSONObject response = new JSONObject();
        response.put("projects", list);
        writeJson(ctx, response);
    }

    private void handleRecentProjects(Context ctx) {
        // Phase 6: Use EditorProjectService instead of PreferencesDesktop
        JSONArray recent = new JSONArray();
        List<EditorProjectService.RecentProject> recentProjects = mEditorProjectService.getRecentProjects();
        for (EditorProjectService.RecentProject rp : recentProjects) {
            JSONObject entry = new JSONObject();
            entry.put("name", rp.name);
            entry.put("path", rp.path);
            entry.put("date", String.valueOf(rp.lastOpened));
            recent.put(entry);
        }
        JSONObject response = new JSONObject();
        response.put("projects", recent);
        writeJson(ctx, response);
    }

    private void handleRecentProjectRemove(Context ctx) {
        JSONObject body = readJsonBody(ctx);
        String path = body.optString("path", null);
        if (path == null || path.isBlank()) {
            writeError(ctx, 400, "BAD_REQUEST", "Missing recent project path");
            return;
        }
        boolean removed = removeRecentProject(path);
        JSONObject response = new JSONObject();
        response.put("removed", removed);
        writeJson(ctx, response);
    }

    private void handleStaticProjectList(Context ctx, String basePath) {
        JSONArray list = new JSONArray();
        File base = new File(basePath);
        if (base.exists() && base.isDirectory()) {
            File[] dirs = base.listFiles(File::isDirectory);
            if (dirs != null) {
                Arrays.sort(dirs, Comparator.comparing(File::getName, String.CASE_INSENSITIVE_ORDER));
                for (File dir : dirs) {
                    RunTimeProject project = new RunTimeProject();
                    project.parseForInformation(dir.getPath());
                    JSONObject entry = new JSONObject();
                    String name = project.getProjectName();
                    entry.put("name", (name == null || name.isBlank()) ? dir.getName() : name);
                    entry.put("path", dir.getPath());
                    list.put(entry);
                }
            }
        }
        JSONObject response = new JSONObject();
        response.put("projects", list);
        writeJson(ctx, response);
    }

    private void handleOpenProject(Context ctx) {
        JSONObject body = readJsonBody(ctx);
        String path = body.optString("path", null);
        if (path == null || path.isBlank()) {
            writeError(ctx, 400, "BAD_REQUEST", "Missing project path");
            return;
        }

        // Phase 6: Use EditorProjectService instead of EditorInstance
        JSONObject projectJson = callOnEdt(() -> {
            EditorProject project = mEditorProjectService.openProject(path);
            if (project == null) {
                return null;
            }
            String projectId = mEditorProjectService.getProjectId(project);
            if (projectId == null) {
                return null;
            }
            // Set as current project
            mCurrentProjectId = projectId;

            // Phase 6: Initialize dock points for existing edges
            initializeDockPointsForProject(project);

            return projectToJson(project, projectId);
        });

        if (projectJson == null) {
            writeError(ctx, 500, "OPEN_FAILED", "Failed to open project");
            return;
        }

        writeJson(ctx, projectJson);
        emitUiProjectLoaded(projectJson);
        emitUiProjectState(projectJson.optString("projectId", null));
    }

    private void handleNewProject(Context ctx) {
        JSONObject body = readJsonBody(ctx);
        String name = body.optString("name", null);
        String baseDir = body.optString("baseDir", null);
        if (name == null || name.isBlank()) {
            writeError(ctx, 400, "BAD_REQUEST", "Missing project name");
            return;
        }

        // Phase 6: Use EditorProjectService instead of EditorInstance
        JSONObject projectJson = callOnEdt(() -> {
            EditorProject project = mEditorProjectService.createProject(name);
            if (project == null) {
                return null;
            }
            String projectId = mEditorProjectService.getProjectId(project);
            if (projectId == null) {
                return null;
            }

            // Set as current project
            mCurrentProjectId = projectId;

            // Save to baseDir if provided
            if (baseDir != null && !baseDir.isBlank()) {
                File target = new File(baseDir, name);
                if (project.write(target)) {
                    mEditorProjectService.addRecentProject(project.getProjectPath(), project.getProjectName());
                }
            }

            return projectToJson(project, projectId);
        });

        if (projectJson == null) {
            writeError(ctx, 500, "CREATE_FAILED", "Failed to create project");
            return;
        }

        writeJson(ctx, projectJson);
        emitUiProjectLoaded(projectJson);
        emitUiProjectState(projectJson.optString("projectId", null));
    }

    // Phase 6: Headless version of handleSaveProject
    private void handleSaveProject(Context ctx) {
        String projectId = ctx.pathParam("id");
        JSONObject projectJson = callOnEdt(() -> {
            EditorProject project = mEditorProjectService.getProject(projectId);
            if (project == null) {
                return null;
            }
            if (project.isPending()) {
                return new JSONObject().put("error", "PROJECT_PENDING");
            }
            boolean ok = mEditorProjectService.saveProject(project);
            if (!ok) {
                return new JSONObject().put("error", "SAVE_FAILED");
            }
            return projectToJson(project, projectId);
        });

        if (projectJson == null) {
            writeError(ctx, 404, "PROJECT_NOT_FOUND", "Project not found");
            return;
        }
        if (projectJson.has("error")) {
            String error = projectJson.getString("error");
            if ("PROJECT_PENDING".equals(error)) {
                writeError(ctx, 409, "PROJECT_PENDING", "Project has no path; use save-as");
            } else {
                writeError(ctx, 500, "SAVE_FAILED", "Failed to save project");
            }
            return;
        }
        writeJson(ctx, projectJson);
        emitUiProjectSaved(projectJson);
    }

    // Phase 6: Headless version of handleSaveAsProject
    private void handleSaveAsProject(Context ctx) {
        String projectId = ctx.pathParam("id");
        JSONObject body = readJsonBody(ctx);
        String path = body.optString("path", null);
        if (path == null || path.isBlank()) {
            writeError(ctx, 400, "BAD_REQUEST", "Missing save path");
            return;
        }

        JSONObject projectJson = callOnEdt(() -> {
            EditorProject project = mEditorProjectService.getProject(projectId);
            if (project == null) {
                return null;
            }
            boolean ok = mEditorProjectService.saveProjectAs(project, path);
            if (!ok) {
                return new JSONObject().put("error", "SAVE_FAILED");
            }
            return projectToJson(project, projectId);
        });

        if (projectJson == null) {
            writeError(ctx, 404, "PROJECT_NOT_FOUND", "Project not found");
            return;
        }
        if (projectJson.has("error")) {
            writeError(ctx, 500, "SAVE_FAILED", "Failed to save project");
            return;
        }
        writeJson(ctx, projectJson);
        emitUiProjectSaved(projectJson);
    }

    private void handleCloseProject(Context ctx) {
        String projectId = ctx.pathParam("id");
        // Phase 6: Use EditorProjectService instead of EditorInstance
        boolean closed = callOnEdt(() -> {
            EditorProject project = mEditorProjectService.getProject(projectId);
            if (project != null) {
                // Clear dock points for this project before closing
                clearDockPointsForProject(project.getSceneFlow());
            }
            return mEditorProjectService.closeProject(projectId);
        });
        if (!closed) {
            writeError(ctx, 404, "PROJECT_NOT_FOUND", "Project not found");
            return;
        }
        JSONObject response = new JSONObject();
        response.put("closed", true);
        response.put("projectId", projectId);
        writeJson(ctx, response);
        emitUiProjectClosed(projectId);
    }

    private void handleProjectDetail(Context ctx) {
        String projectId = ctx.pathParam("id");
        // Phase 6: Use EditorProjectService instead of EditorInstance
        JSONObject projectJson = callOnEdt(() -> {
            EditorProject project = mEditorProjectService.getProject(projectId);
            if (project == null) {
                return null;
            }
            return projectToJson(project, projectId);
        });
        if (projectJson == null) {
            writeError(ctx, 404, "PROJECT_NOT_FOUND", "Project not found");
            return;
        }
        writeJson(ctx, projectJson);
    }

    private void handleSceneFlow(Context ctx) {
        String projectId = ctx.pathParam("id");
        String superNodeId = ctx.queryParam("superNodeId");
        // Phase 6: Use EditorProjectService instead of EditorInstance
        JSONObject snapshot = callOnEdt(() -> {
            EditorProject project = mEditorProjectService.getProject(projectId);
            if (project == null) {
                return null;
            }
            SceneFlow sceneFlow = project.getSceneFlow();
            SuperNode target = resolveSuperNode(sceneFlow, superNodeId);
            if (target == null) {
                target = sceneFlow; // Default to root sceneflow
            }
            return sceneFlowSnapshot(project, projectId, target);
        });
        if (snapshot == null) {
            writeError(ctx, 404, "PROJECT_NOT_FOUND", "Project not found");
            return;
        }
        writeJson(ctx, snapshot);
    }

    // Phase 6: Headless version of handleSceneFlowNavigate
    private void handleSceneFlowNavigate(Context ctx) {
        String projectId = ctx.pathParam("id");
        JSONObject body = readJsonBody(ctx);
        String superNodeId = body.optString("superNodeId", null);
        if (superNodeId == null || superNodeId.isBlank()) {
            writeError(ctx, 400, "BAD_REQUEST", "Missing superNodeId");
            return;
        }

        JSONObject snapshot = callOnEdt(() -> {
            EditorProject project = mEditorProjectService.getProject(projectId);
            if (project == null) {
                return null;
            }
            SceneFlow sceneFlow = project.getSceneFlow();

            // Find the target SuperNode
            SuperNode targetSuperNode;
            if (ROOT_SUPERNODE_ID.equals(superNodeId)) {
                targetSuperNode = sceneFlow;
            } else {
                List<SuperNode> path = findPathToSuperNode(sceneFlow, superNodeId);
                if (path == null || path.isEmpty()) {
                    return new JSONObject().put("error", "SUPER_NODE_NOT_FOUND");
                }
                targetSuperNode = path.get(path.size() - 1);
            }

            // Return snapshot of the target SuperNode (headless - no WorkSpacePanel needed)
            return sceneFlowSnapshot(project, projectId, targetSuperNode);
        });

        if (snapshot == null) {
            writeError(ctx, 404, "PROJECT_NOT_FOUND", "Project not found");
            return;
        }
        if (snapshot.has("error")) {
            writeError(ctx, 404, "SUPER_NODE_NOT_FOUND", "Super node not found");
            return;
        }
        writeJson(ctx, snapshot);
        emitUiSceneFlowSnapshot(snapshot);
    }

    private void handleScript(Context ctx) {
        String projectId = ctx.pathParam("id");
        // Phase 6: Use EditorProjectService instead of EditorInstance
        String text = callOnEdt(() -> {
            EditorProject project = mEditorProjectService.getProject(projectId);
            if (project == null) {
                return null;
            }
            SceneScript script = project.getSceneScript();
            return script.getText();
        });
        if (text == null) {
            writeError(ctx, 404, "PROJECT_NOT_FOUND", "Project not found");
            return;
        }
        ScriptDiagnostics.Result diagnostics = ScriptDiagnostics.analyze(text);
        JSONObject response = new JSONObject();
        response.put("text", text);
        response.put("version", text.hashCode());
        response.put("parseOk", diagnostics.isParseOk());
        response.put("parseErrors", diagnosticsToJson(diagnostics.getDiagnostics()));
        writeJson(ctx, response);
    }

    private void handleScriptDiagnostics(Context ctx) {
        String projectId = ctx.pathParam("id");
        JSONObject body = readJsonBody(ctx);
        String bodyText = body.optString("text", null);
        String text = bodyText;
        if (text == null) {
            // Phase 6: Use EditorProjectService instead of EditorInstance
            text = callOnEdt(() -> {
                EditorProject project = mEditorProjectService.getProject(projectId);
                if (project == null) {
                    return null;
                }
                SceneScript script = project.getSceneScript();
                return script.getText();
            });
        }
        if (text == null) {
            writeError(ctx, 404, "PROJECT_NOT_FOUND", "Project not found");
            return;
        }
        ScriptDiagnostics.Result diagnostics = ScriptDiagnostics.analyze(text);
        JSONObject response = new JSONObject();
        response.put("parseOk", diagnostics.isParseOk());
        response.put("parseErrors", diagnosticsToJson(diagnostics.getDiagnostics()));
        writeJson(ctx, response);
    }

    private void handleScriptScenes(Context ctx) {
        String projectId = ctx.pathParam("id");
        // Phase 6: Use EditorProjectService instead of EditorInstance
        JSONObject response = callOnEdt(() -> {
            EditorProject project = mEditorProjectService.getProject(projectId);
            if (project == null) {
                return null;
            }
            SceneScript script = project.getSceneScript();
            return scriptScenesToJson(script);
        });
        if (response == null) {
            writeError(ctx, 404, "PROJECT_NOT_FOUND", "Project not found");
            return;
        }
        writeJson(ctx, response);
    }

    private void handleScriptElements(Context ctx) {
        String projectId = ctx.pathParam("id");
        // Phase 6: Use EditorProjectService instead of EditorInstance
        JSONObject response = callOnEdt(() -> {
            EditorProject project = mEditorProjectService.getProject(projectId);
            if (project == null) {
                return null;
            }
            return scriptElementsToJson(project);
        });
        if (response == null) {
            writeError(ctx, 404, "PROJECT_NOT_FOUND", "Project not found");
            return;
        }
        writeJson(ctx, response);
    }

    private void handleFunctions(Context ctx) {
        String projectId = ctx.pathParam("id");
        // Phase 6: Use EditorProjectService instead of EditorInstance
        JSONObject response = callOnEdt(() -> {
            EditorProject project = mEditorProjectService.getProject(projectId);
            if (project == null) {
                return null;
            }
            return functionsToJson(project);
        });
        if (response == null) {
            writeError(ctx, 404, "PROJECT_NOT_FOUND", "Project not found");
            return;
        }
        writeJson(ctx, response);
    }

    private void handleTypes(Context ctx) {
        String projectId = ctx.pathParam("id");
        // Phase 6: Use EditorProjectService instead of EditorInstance
        JSONObject response = callOnEdt(() -> {
            EditorProject project = mEditorProjectService.getProject(projectId);
            if (project == null) {
                return null;
            }
            return typesToJson(project);
        });
        if (response == null) {
            writeError(ctx, 404, "PROJECT_NOT_FOUND", "Project not found");
            return;
        }
        writeJson(ctx, response);
    }

    private void handleConfig(Context ctx) {
        String projectId = ctx.pathParam("id");
        // Phase 6: Use EditorProjectService instead of EditorInstance
        JSONObject response = callOnEdt(() -> {
            EditorProject project = mEditorProjectService.getProject(projectId);
            if (project == null) {
                return null;
            }
            return configToJson(project);
        });
        if (response == null) {
            writeError(ctx, 404, "PROJECT_NOT_FOUND", "Project not found");
            return;
        }
        writeJson(ctx, response);
    }

    private void handleProjectConfig(Context ctx) {
        String projectId = ctx.pathParam("id");
        // Phase 6: Use EditorProjectService instead of EditorInstance
        JSONObject response = callOnEdt(() -> {
            EditorProject project = mEditorProjectService.getProject(projectId);
            if (project == null) {
                return null;
            }
            return projectConfigToJson(project);
        });
        if (response == null) {
            writeError(ctx, 404, "PROJECT_NOT_FOUND", "Project not found");
            return;
        }
        writeJson(ctx, response);
    }

    private void handleProjectConfigKeys(Context ctx) {
        String projectId = ctx.pathParam("id");
        String deviceName = ctx.queryParam("device");
        String className = ctx.queryParam("className");
        String scope = ctx.queryParam("scope");
        if ((deviceName == null || deviceName.isBlank()) && (className == null || className.isBlank())) {
            writeError(ctx, 400, "BAD_REQUEST", "Missing device or className");
            return;
        }
        boolean agentScope = "agent".equalsIgnoreCase(scope);
        // Phase 6: Use EditorProjectService instead of EditorInstance
        JSONObject response = callOnEdt(() -> {
            EditorProject project = mEditorProjectService.getProject(projectId);
            if (project == null) {
                return null;
            }
            PluginConfig plugin = null;
            if (className != null && !className.isBlank()) {
                String resolvedName = (deviceName == null || deviceName.isBlank()) ? className : deviceName;
                plugin = new PluginConfig("device", resolvedName, className, true, new ArrayList<>());
            } else {
                plugin = project.getProjectConfig().getPluginConfig(deviceName);
                if (plugin == null) {
                    JSONObject error = new JSONObject();
                    error.put("status", 404);
                    error.put("error", "DEVICE_NOT_FOUND");
                    error.put("message", "Device not found");
                    return error;
                }
            }
            return exportableKeysToJson(project, plugin, agentScope);
        });
        if (response == null) {
            writeError(ctx, 404, "PROJECT_NOT_FOUND", "Project not found");
            return;
        }
        if (response.has("status")) {
            int status = response.optInt("status", 400);
            writeError(ctx, status, response.optString("error", "ERROR"), response.optString("message", "Error"));
            return;
        }
        writeJson(ctx, response);
    }

    private void handleAvailableDevices(Context ctx) {
        List<DeviceEntry> cached = mAvailableDevices;
        if (cached == null) {
            cached = loadAvailableDeviceCache();
        }
        JSONArray devices = new JSONArray();
        for (DeviceEntry entry : cached) {
            JSONObject device = new JSONObject();
            device.put("name", entry.name);
            device.put("className", entry.className);
            devices.put(device);
        }
        JSONObject response = new JSONObject();
        response.put("devices", devices);
        writeJson(ctx, response);
    }

    private List<DeviceEntry> loadAvailableDeviceCache() {
        List<DeviceEntry> entries = new ArrayList<>();
        try {
            ExtensionsFromJar extensions = new ExtensionsFromJar("de.dfki.vsm.xtension", false);
            extensions.loadExtensions();
            ArrayList<String> names = extensions.getActivitiesShortNames();
            ArrayList<String> classes = extensions.getActivitiesLongName();
            int count = Math.min(names.size(), classes.size());
            Set<String> seen = new HashSet<>();
            for (int i = 0; i < count; i++) {
                String name = names.get(i);
                String className = classes.get(i);
                if (className == null || className.isBlank() || !seen.add(className)) {
                    continue;
                }
                entries.add(new DeviceEntry(name == null ? "" : name, className));
            }
            entries.sort(Comparator.comparing(entry -> entry.name, String.CASE_INSENSITIVE_ORDER));
        } catch (Exception exc) {
            mLogger.warning("Warning: Failed to scan available devices: " + exc.getMessage());
        }
        mAvailableDevices = entries;
        return entries;
    }

    private void loadExportablePropertyCache() {
        List<String> classes = new ArrayList<>();
        try {
            Reflections reflections = new Reflections(new ConfigurationBuilder()
                    .forPackages("de.dfki.vsm.xtension")
                    .addScanners(new SubTypesScanner(false))
                    .setExpandSuperTypes(false));
            Set<Class<? extends ExportableProperties>> types = reflections.getSubTypesOf(ExportableProperties.class);
            for (Class<? extends ExportableProperties> type : types) {
                if (RunTimePlugin.class.isAssignableFrom(type)) {
                    continue;
                }
                if (!hasNoArgConstructor(type)) {
                    continue;
                }
                classes.add(type.getName());
            }
            classes.sort(String.CASE_INSENSITIVE_ORDER);
        } catch (Exception exc) {
            mLogger.warning("Warning: Failed to scan exportable properties: " + exc.getMessage());
        }
        mExportablePropertyClassNames = classes;
    }

    private boolean hasNoArgConstructor(Class<?> type) {
        try {
            type.getDeclaredConstructor();
            return true;
        } catch (NoSuchMethodException exc) {
            return false;
        }
    }

    private JSONObject exportableKeysToJson(EditorProject project, PluginConfig plugin, boolean agentScope) {
        JSONObject response = new JSONObject();
        response.put("device", plugin.getPluginName());
        response.put("className", plugin.getClassName());
        response.put("scope", agentScope ? "agent" : "plugin");
        JSONArray required = new JSONArray();
        JSONArray optional = new JSONArray();
        try {
            ExportableProperties exportable = resolveExportableProperties(project, plugin);
            Map<ProjectProperty, ?> props = agentScope
                    ? exportable.getExportableAgentProperties()
                    : exportable.getExportableProperties();
            List<JSONObject> requiredList = new ArrayList<>();
            List<JSONObject> optionalList = new ArrayList<>();
            if (props != null) {
                for (ProjectProperty property : props.keySet()) {
                    if (property == null || property.getName() == null) {
                        continue;
                    }
                    JSONObject entry = new JSONObject();
                    entry.put("name", property.getName());
                    String desc = property.getDescription();
                    if (desc != null && !desc.isBlank()) {
                        entry.put("description", desc);
                    }
                    if (property.isRequired()) {
                        requiredList.add(entry);
                    } else {
                        optionalList.add(entry);
                    }
                }
            }
            Comparator<JSONObject> byName = Comparator.comparing(
                    item -> item.optString("name", ""),
                    String.CASE_INSENSITIVE_ORDER);
            requiredList.sort(byName);
            optionalList.sort(byName);
            for (JSONObject entry : requiredList) {
                required.put(entry);
            }
            for (JSONObject entry : optionalList) {
                optional.put(entry);
            }
            response.put("supported", true);
        } catch (NotExportableInterface exc) {
            response.put("supported", false);
        } catch (Exception exc) {
            response.put("supported", false);
            response.put("error", "EXPORTABLE_FAILED");
            response.put("message", exc.getMessage());
        }
        response.put("required", required);
        response.put("optional", optional);
        return response;
    }

    private ExportableProperties resolveExportableProperties(EditorProject project, PluginConfig plugin) throws Exception {
        String className = plugin.getClassName();
        if (className == null || className.isBlank()) {
            throw new NotExportableInterface("Missing plugin class name");
        }
        String pluginPackage = className.contains(".")
                ? className.substring(0, className.lastIndexOf('.'))
                : "";
        String pluginSimple = className.contains(".")
                ? className.substring(className.lastIndexOf('.') + 1)
                : className;
        String baseToken = pluginSimple;
        String[] suffixes = {"CmdExecutor", "Executor", "RunTimePlugin", "Plugin"};
        for (String suffix : suffixes) {
            if (baseToken.endsWith(suffix)) {
                baseToken = baseToken.substring(0, baseToken.length() - suffix.length());
                break;
            }
        }
        String baseTokenLower = baseToken.toLowerCase(Locale.ROOT);
        List<String> candidates = new ArrayList<>();
        List<String> cached = mExportablePropertyClassNames;
        if (cached == null || cached.isEmpty()) {
            loadExportablePropertyCache();
            cached = mExportablePropertyClassNames;
        }
        if (cached != null) {
            for (String entry : cached) {
                if (pluginPackage.isEmpty() || entry.startsWith(pluginPackage)) {
                    candidates.add(entry);
                }
            }
        }
        if (candidates.isEmpty()) {
            throw new NotExportableInterface("No exportable property class found");
        }
        String chosen = null;
        for (String entry : candidates) {
            String simple = entry.substring(entry.lastIndexOf('.') + 1).toLowerCase(Locale.ROOT);
            if (!baseTokenLower.isEmpty() && simple.contains(baseTokenLower)) {
                chosen = entry;
                break;
            }
        }
        if (chosen == null) {
            chosen = candidates.get(0);
        }
        Class<?> propClass = Class.forName(chosen);
        Object instance = propClass.getDeclaredConstructor().newInstance();
        return (ExportableProperties) instance;
    }

    private void handlePreferences(Context ctx) {
        JSONObject response = preferencesToJson();
        writeJson(ctx, response);
    }

    private void handleRuntime(Context ctx) {
        String projectId = ctx.pathParam("id");
        mLastRuntimeProjectId = projectId;
        // Phase 6: Use EditorProjectService instead of EditorInstance
        JSONObject response = callOnEdt(() -> {
            EditorProject project = mEditorProjectService.getProject(projectId);
            if (project == null) {
                return null;
            }
            return runtimeToJson(project);
        });
        if (response == null) {
            writeError(ctx, 404, "PROJECT_NOT_FOUND", "Project not found");
            return;
        }
        writeJson(ctx, response);
    }

    private void handleFsRoots(Context ctx) {
        JSONArray roots = new JSONArray();
        File[] list = File.listRoots();
        if (list != null) {
            for (File root : list) {
                JSONObject entry = new JSONObject();
                entry.put("path", root.getAbsolutePath());
                entry.put("name", root.getPath());
                roots.put(entry);
            }
        }
        JSONObject response = new JSONObject();
        response.put("roots", roots);
        writeJson(ctx, response);
    }

    private void handleFsList(Context ctx) {
        String path = ctx.queryParam("path");
        if (path == null || path.isBlank()) {
            writeError(ctx, 400, "BAD_REQUEST", "Missing path");
            return;
        }
        File dir = new File(path);
        if (!dir.exists()) {
            writeError(ctx, 404, "NOT_FOUND", "Path not found");
            return;
        }
        if (!dir.isDirectory()) {
            writeError(ctx, 400, "BAD_REQUEST", "Path is not a directory");
            return;
        }
        File[] entries = dir.listFiles();
        JSONArray list = new JSONArray();
        if (entries != null) {
            Arrays.sort(entries, Comparator.comparing(File::isFile).thenComparing(File::getName, String.CASE_INSENSITIVE_ORDER));
            for (File entry : entries) {
                JSONObject item = new JSONObject();
                item.put("name", entry.getName());
                item.put("path", entry.getAbsolutePath());
                item.put("directory", entry.isDirectory());
                item.put("size", entry.isFile() ? entry.length() : 0);
                item.put("modified", entry.lastModified());
                list.put(item);
            }
        }
        JSONObject response = new JSONObject();
        response.put("entries", list);
        writeJson(ctx, response);
    }

    private void handleWsMessage(WsContext ctx, String message) {
        String requestId = null;
        String name = "";
        try {
            JSONObject payload = new JSONObject(message);
            String type = payload.optString("type", "cmd");

            // Debug: Log all incoming WebSocket commands
            String cmdName = payload.optString("name", "");
            mLogger.message("*** WS MESSAGE RECEIVED *** type=" + type + ", name=" + cmdName);

            if (!"cmd".equals(type)) {
                return;
            }
            name = payload.optString("name", "");
            requestId = payload.optString("id", null);
            String sourceClientId = payload.optString("sourceClientId", null);
            JSONObject body = payload.optJSONObject("payload");
            if (body == null) {
                body = new JSONObject();
            }

            switch (name) {
                case "Project.Open": {
                    String path = body.optString("path", null);
                    if (path == null || path.isBlank()) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing project path");
                        return;
                    }
                    // Phase 6: Use EditorProjectService instead of EditorInstance
                    JSONObject project = callOnEdt(() -> {
                        EditorProject proj = mEditorProjectService.openProject(path);
                        if (proj == null) {
                            return null;
                        }
                        String projectId = mEditorProjectService.getProjectId(proj);
                        if (projectId == null) {
                            return null;
                        }
                        mCurrentProjectId = projectId;
                        // Phase 6: Initialize dock points for existing edges
                        initializeDockPointsForProject(proj);
                        return projectToJson(proj, projectId);
                    });
                    if (project == null) {
                        sendError(ctx, requestId, "OPEN_FAILED", "Failed to open project");
                        return;
                    }
                    sendResponse(ctx, requestId, name, project);
                    emitUiProjectLoaded(project);
                    emitUiProjectState(project.optString("projectId", null));
                    return;
                }
                case "Project.New": {
                    String projectName = body.optString("name", null);
                    String baseDir = body.optString("baseDir", null);
                    if (projectName == null || projectName.isBlank()) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing project name");
                        return;
                    }
                    // Phase 6: Use EditorProjectService instead of EditorInstance
                    JSONObject project = callOnEdt(() -> {
                        EditorProject proj = mEditorProjectService.createProject(projectName);
                        if (proj == null) {
                            return null;
                        }
                        String projectId = mEditorProjectService.getProjectId(proj);
                        if (projectId == null) {
                            return null;
                        }
                        mCurrentProjectId = projectId;

                        if (baseDir != null && !baseDir.isBlank()) {
                            File target = new File(baseDir, projectName);
                            if (proj.write(target)) {
                                mEditorProjectService.addRecentProject(proj.getProjectPath(), proj.getProjectName());
                            }
                        }
                        return projectToJson(proj, projectId);
                    });
                    if (project == null) {
                        sendError(ctx, requestId, "CREATE_FAILED", "Failed to create project");
                        return;
                    }
                    sendResponse(ctx, requestId, name, project);
                    emitUiProjectLoaded(project);
                    emitUiProjectState(project.optString("projectId", null));
                    return;
                }
                case "Project.Save": {
                    String projectId = body.optString("projectId", null);
                    if (projectId == null || projectId.isBlank()) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId");
                        return;
                    }
                    // Phase 6: Use EditorProjectService instead of EditorInstance
                    JSONObject project = callOnEdt(() -> {
                        EditorProject proj = mEditorProjectService.getProject(projectId);
                        if (proj == null) {
                            return null;
                        }
                        if (proj.isPending()) {
                            return new JSONObject().put("error", "PROJECT_PENDING");
                        }
                        if (!mEditorProjectService.saveProject(proj)) {
                            return new JSONObject().put("error", "SAVE_FAILED");
                        }
                        return projectToJson(proj, projectId);
                    });
                    if (project == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (project.has("error")) {
                        String err = project.getString("error");
                        if ("PROJECT_PENDING".equals(err)) {
                            sendError(ctx, requestId, "PROJECT_PENDING", "Project has no path; use save-as");
                        } else {
                            sendError(ctx, requestId, "SAVE_FAILED", "Failed to save project");
                        }
                        return;
                    }
                    sendResponse(ctx, requestId, name, project);
                    emitUiProjectSaved(project);
                    return;
                }
                // Phase 6: Headless version of Project.SaveAs
                case "Project.SaveAs": {
                    String projectId = body.optString("projectId", null);
                    String path = body.optString("path", null);
                    if (projectId == null || projectId.isBlank() || path == null || path.isBlank()) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId or path");
                        return;
                    }
                    JSONObject project = callOnEdt(() -> {
                        EditorProject proj = mEditorProjectService.getProject(projectId);
                        if (proj == null) {
                            return null;
                        }
                        // Extract project name from path
                        File targetFile = new File(path);
                        String projectName = targetFile.getName();
                        proj.setProjectName(projectName);
                        if (proj.write(targetFile)) {
                            mEditorProjectService.addRecentProject(proj.getProjectPath(), proj.getProjectName());
                            // write() already updates the initial hash
                            return projectToJson(proj, projectId);
                        }
                        return new JSONObject().put("error", "SAVE_FAILED");
                    });
                    if (project == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (project.has("error")) {
                        sendError(ctx, requestId, "SAVE_FAILED", "Failed to save project");
                        return;
                    }
                    sendResponse(ctx, requestId, name, project);
                    emitUiProjectSaved(project);
                    return;
                }
                // Phase 6: Headless version of Project.Close
                case "Project.Close": {
                    String projectId = body.optString("projectId", null);
                    if (projectId == null || projectId.isBlank()) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId");
                        return;
                    }
                    boolean closed = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return false;
                        }
                        // Clear dock points for this project
                        clearDockPointsForProject(project.getSceneFlow());
                        // Close the project
                        return mEditorProjectService.closeProject(projectId);
                    });
                    if (!closed) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    sendResponse(ctx, requestId, name, new JSONObject().put("projectId", projectId));
                    emitUiProjectClosed(projectId);
                    return;
                }
                // Phase 6: Headless version of Project.Activate
                case "Project.Activate": {
                    String projectId = body.optString("projectId", null);
                    if (projectId == null || projectId.isBlank()) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId");
                        return;
                    }
                    JSONObject project = callOnEdt(() -> {
                        EditorProject proj = mEditorProjectService.getProject(projectId);
                        if (proj == null) {
                            return null;
                        }
                        // Set as current active project
                        mCurrentProjectId = projectId;
                        return projectToJson(proj, projectId);
                    });
                    if (project == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    sendResponse(ctx, requestId, name, project);
                    emitUiProjectLoaded(project);
                    emitUiProjectState(project.optString("projectId", null));
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Navigate
                case "SceneFlow.Navigate": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    if (projectId == null || projectId.isBlank() || superNodeId == null || superNodeId.isBlank()) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId or superNodeId");
                        return;
                    }
                    JSONObject snapshot = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        // Resolve target SuperNode
                        SuperNode targetSuperNode;
                        if (ROOT_SUPERNODE_ID.equals(superNodeId)) {
                            targetSuperNode = sceneFlow;
                        } else {
                            BasicNode found = findNodeRecursive(sceneFlow, superNodeId);
                            if (found instanceof SuperNode) {
                                targetSuperNode = (SuperNode) found;
                            } else {
                                return new JSONObject().put("error", "SUPER_NODE_NOT_FOUND");
                            }
                        }
                        // Return snapshot of the target SuperNode
                        return sceneFlowSnapshot(project, projectId, targetSuperNode);
                    });
                    if (snapshot == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (snapshot.has("error")) {
                        sendError(ctx, requestId, "SUPER_NODE_NOT_FOUND", "Super node not found");
                        return;
                    }
                    sendResponse(ctx, requestId, name, snapshot);
                    return;
                }
                // Phase 6/8: Headless version of SceneFlow.Node.Create using SceneFlowService
                case "SceneFlow.Node.Create": {
                    String projectId = body.optString("projectId", null);
                    String parentId = body.optString("parentId", body.optString("superNodeId", null));
                    String nodeType = body.optString("nodeType", "Basic");
                    String nodeName = body.optString("name", null);
                    double x = body.has("x") ? body.optDouble("x", Double.NaN) : Double.NaN;
                    double y = body.has("y") ? body.optDouble("y", Double.NaN) : Double.NaN;
                    if (projectId == null || projectId.isBlank() || !isFinite(x) || !isFinite(y)) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId or coordinates");
                        return;
                    }
                    final String resolvedParentId = parentId;
                    final String finalNodeName = nodeName;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }

                        // Resolve parent node ID (use sceneflow root if not specified)
                        String effectiveParentId = resolvedParentId;
                        if (effectiveParentId == null || effectiveParentId.isBlank() || ROOT_SUPERNODE_ID.equals(effectiveParentId)) {
                            effectiveParentId = project.getSceneFlow().getId();
                        }

                        // Calculate grid-snapped position
                        EditorConfig config = project.getEditorConfig();
                        int gridX = config != null ? config.sGRID_XSPACE : 1;
                        int gridY = config != null ? config.sGRID_YSPACE : 1;
                        int nodeW = config != null ? config.sNODEWIDTH : 90;
                        int nodeH = config != null ? config.sNODEHEIGHT : 90;
                        int clampedX = clampPositive((int) Math.round(x));
                        int clampedY = clampPositive((int) Math.round(y));
                        double snapOriginX = nodeW / 2.0 + nodeW / 3.0;
                        double snapOriginY = nodeH / 2.0 + nodeH / 3.0;
                        double centerX = clampedX + nodeW / 2.0;
                        double centerY = clampedY + nodeH / 2.0;
                        double snappedCenterX = snapOriginX + Math.round((centerX - snapOriginX) / gridX) * gridX;
                        double snappedCenterY = snapOriginY + Math.round((centerY - snapOriginY) / gridY) * gridY;
                        int snappedX = (int) Math.round(snappedCenterX - nodeW / 2.0);
                        int snappedY = (int) Math.round(snappedCenterY - nodeH / 2.0);

                        // Determine node type
                        boolean isSuper = "super".equalsIgnoreCase(nodeType) || "supernode".equalsIgnoreCase(nodeType);
                        SceneFlowService.NodeType serviceNodeType = isSuper
                            ? SceneFlowService.NodeType.SUPER_NODE
                            : SceneFlowService.NodeType.BASIC_NODE;

                        // Delegate to SceneFlowService
                        BasicNode createdNode = mSceneFlowService.createNode(
                            project, effectiveParentId, serviceNodeType, new Point(snappedX, snappedY), finalNodeName);

                        if (createdNode == null) {
                            return null;
                        }

                        // Handle start node dock point (UI concern)
                        SuperNode parentNode = (SuperNode) createdNode.getParentNode();
                        if (parentNode != null && parentNode.getStartNodeMap().containsKey(createdNode.getId())) {
                            occupyStartSignDockPoint(createdNode.getId());
                        }

                        JSONObject payloadResp = new JSONObject();
                        payloadResp.put("nodeId", createdNode.getId());
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, parentNode));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found or node creation failed");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    if (response.has("dirty")) {
                        emitUiProjectDirty(projectId, response.getBoolean("dirty"), List.of("sceneflow"));
                    }
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Node.Move
                case "SceneFlow.Node.Move": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String nodeId = body.optString("nodeId", null);
                    double x = body.has("x") ? body.optDouble("x", Double.NaN) : Double.NaN;
                    double y = body.has("y") ? body.optDouble("y", Double.NaN) : Double.NaN;
                    boolean snap = body.optBoolean("snap", false);
                    if (projectId == null || projectId.isBlank() || nodeId == null || nodeId.isBlank()
                            || !isFinite(x) || !isFinite(y)) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId, nodeId, or coordinates");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        BasicNode dataNode = findNodeRecursive(sceneFlow, nodeId);
                        if (dataNode == null) {
                            return new JSONObject().put("error", "NODE_NOT_FOUND");
                        }
                        // Capture old position before updating
                        NodeGraphics oldGraphics = dataNode.getGraphics();
                        NodePosition oldPos = oldGraphics != null ? oldGraphics.getPosition() : null;
                        int oldX = oldPos != null ? oldPos.getXPos() : 0;
                        int oldY = oldPos != null ? oldPos.getYPos() : 0;

                        int targetX = (int) Math.round(x);
                        int targetY = (int) Math.round(y);
                        if (snap) {
                            EditorConfig config = project.getEditorConfig();
                            int gridX = config != null ? config.sGRID_XSPACE : 1;
                            int gridY = config != null ? config.sGRID_YSPACE : 1;
                            int nodeW = config != null ? config.sNODEWIDTH : 90;
                            int nodeH = config != null ? config.sNODEHEIGHT : 90;
                            // Grid origin offset by -gridX/2 to match visual grid dots
                            double snapOriginX = nodeW / 2.0 + nodeW / 3.0;
                            double snapOriginY = nodeH / 2.0 + nodeH / 3.0;
                            // Snap node CENTER to grid, then convert back to top-left
                            double centerX = targetX + nodeW / 2.0;
                            double centerY = targetY + nodeH / 2.0;
                            double snappedCenterX = snapOriginX + Math.round((centerX - snapOriginX) / gridX) * gridX;
                            double snappedCenterY = snapOriginY + Math.round((centerY - snapOriginY) / gridY) * gridY;
                            targetX = (int) Math.round(snappedCenterX - nodeW / 2.0);
                            targetY = (int) Math.round(snappedCenterY - nodeH / 2.0);
                        }
                        dataNode.setGraphics(new NodeGraphics(targetX, targetY));

                        // Update edge endpoints by applying the movement delta
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        updateEdgeEndpointsForMovedNode(dataNode, snapshotTarget, project.getEditorConfig(), oldX, oldY);

                        JSONObject payloadResp = new JSONObject();
                        payloadResp.put("nodeId", nodeId);
                        payloadResp.put("x", targetX);
                        payloadResp.put("y", targetY);
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), "Node not found");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    if (response.has("dirty")) {
                        emitUiProjectDirty(projectId, response.getBoolean("dirty"), List.of("sceneflow"));
                    }
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Node.MoveGroup
                case "SceneFlow.Node.MoveGroup": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    JSONArray nodesPayload = body.optJSONArray("nodes");
                    boolean snap = body.optBoolean("snap", false);
                    if (projectId == null || projectId.isBlank() || nodesPayload == null || nodesPayload.length() == 0) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId or nodes");
                        return;
                    }
                    List<NodeMoveRequest> moves = new ArrayList<>();
                    for (int i = 0; i < nodesPayload.length(); i++) {
                        JSONObject entry = nodesPayload.optJSONObject(i);
                        if (entry == null) {
                            sendError(ctx, requestId, "BAD_REQUEST", "Invalid nodes entry");
                            return;
                        }
                        String moveId = entry.optString("id", null);
                        double moveX = entry.has("x") ? entry.optDouble("x", Double.NaN) : Double.NaN;
                        double moveY = entry.has("y") ? entry.optDouble("y", Double.NaN) : Double.NaN;
                        if (moveId == null || moveId.isBlank() || !isFinite(moveX) || !isFinite(moveY)) {
                            sendError(ctx, requestId, "BAD_REQUEST", "Missing node id or coordinates");
                            return;
                        }
                        moves.add(new NodeMoveRequest(moveId, new Point((int) Math.round(moveX), (int) Math.round(moveY))));
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        // Phase 6: Use EditorProjectService instead of EditorInstance
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        SuperNode activeSuperNode = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        if (activeSuperNode == null) {
                            activeSuperNode = sceneFlow;
                        }
                        // Phase 6: Move nodes by directly updating their graphics (headless - no WorkSpacePanel)
                        // Store old positions for delta-based edge updates
                        EditorConfig config = project.getEditorConfig();
                        int gridX = config != null ? config.sGRID_XSPACE : 1;
                        int gridY = config != null ? config.sGRID_YSPACE : 1;
                        int nodeW = config != null ? config.sNODEWIDTH : 90;
                        int nodeH = config != null ? config.sNODEHEIGHT : 90;
                        // Grid origin offset by -gridX/2 to match visual grid dots
                        double snapOriginX = nodeW / 2.0 + nodeW / 3.0;
                        double snapOriginY = nodeH / 2.0 + nodeH / 3.0;
                        Map<BasicNode, int[]> oldPositions = new IdentityHashMap<>();
                        List<BasicNode> movedNodes = new ArrayList<>();
                        for (NodeMoveRequest move : moves) {
                            BasicNode dataNode = resolveNodeById(activeSuperNode, move.nodeId);
                            if (dataNode == null) {
                                return new JSONObject().put("error", "NODE_NOT_FOUND").put("nodeId", move.nodeId);
                            }
                            // Capture old position before updating
                            NodeGraphics oldGraphics = dataNode.getGraphics();
                            NodePosition oldPos = oldGraphics != null ? oldGraphics.getPosition() : null;
                            int oldX = oldPos != null ? oldPos.getXPos() : 0;
                            int oldY = oldPos != null ? oldPos.getYPos() : 0;
                            oldPositions.put(dataNode, new int[] { oldX, oldY });

                            // Clamp position to positive values and apply snap
                            int targetX = Math.max(1, move.target.x);
                            int targetY = Math.max(1, move.target.y);
                            if (snap) {
                                // Snap node CENTER to grid, then convert back to top-left
                                double centerX = targetX + nodeW / 2.0;
                                double centerY = targetY + nodeH / 2.0;
                                double snappedCenterX = snapOriginX + Math.round((centerX - snapOriginX) / gridX) * gridX;
                                double snappedCenterY = snapOriginY + Math.round((centerY - snapOriginY) / gridY) * gridY;
                                targetX = (int) Math.round(snappedCenterX - nodeW / 2.0);
                                targetY = (int) Math.round(snappedCenterY - nodeH / 2.0);
                            }
                            // Update node graphics
                            NodeGraphics graphics = dataNode.getGraphics();
                            if (graphics == null) {
                                graphics = new NodeGraphics(targetX, targetY);
                                dataNode.setGraphics(graphics);
                            } else {
                                graphics.setPosition(targetX, targetY);
                            }
                            movedNodes.add(dataNode);
                        }

                        // Update edge endpoints to follow the moved nodes
                        for (BasicNode movedNode : movedNodes) {
                            int[] oldPos = oldPositions.get(movedNode);
                            updateEdgeEndpointsForMovedNode(movedNode, activeSuperNode, config, oldPos[0], oldPos[1]);
                        }

                        JSONObject payloadResp = new JSONObject();
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, activeSuperNode));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        String nodeId = response.optString("nodeId", "");
                        sendError(ctx, requestId, response.getString("error"), "Node not found" + (nodeId.isBlank() ? "" : ": " + nodeId));
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    broadcastDirtyIfPresent(response, projectId);
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Node.Update
                case "SceneFlow.Node.Update": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String nodeId = body.optString("nodeId", "");
                    JSONObject fields = body.optJSONObject("fields");
                    if (projectId == null || projectId.isBlank()) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId");
                        return;
                    }
                    final JSONObject sourcePayload = fields == null ? body : fields;
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        BasicNode dataNode = nodeId.isBlank() ? sceneFlow : findNodeRecursive(sceneFlow, nodeId);
                        if (dataNode == null) {
                            return new JSONObject().put("error", "NODE_NOT_FOUND");
                        }
                        if (sourcePayload.has("name") && !dataNode.isHistoryNode()) {
                            String nodeName = sourcePayload.optString("name", "").trim();
                            if (nodeName.isBlank()) {
                                nodeName = dataNode.getId();
                            }
                            dataNode.setName(nodeName);
                        }
                        if (sourcePayload.has("comment")) {
                            dataNode.setComment(sourcePayload.optString("comment", ""));
                        }
                        if (sourcePayload.has("isStart")) {
                            boolean isStart = sourcePayload.optBoolean("isStart", false);
                            updateStartFlag(dataNode, null, isStart);
                        }
                        JSONObject payloadResp = new JSONObject();
                        payloadResp.put("nodeId", nodeId);
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), "Node not found");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    if (response.has("dirty")) {
                        emitUiProjectDirty(projectId, response.getBoolean("dirty"), List.of("sceneflow"));
                    }
                    return;
                }
                // Phase 6/8: Headless version of SceneFlow.Node.Delete using SceneFlowService
                case "SceneFlow.Node.Delete": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String nodeId = body.optString("nodeId", null);

                    if (projectId == null || projectId.isBlank() || nodeId == null || nodeId.isBlank()) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId or nodeId");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();

                        // Don't allow deleting the root sceneflow itself
                        if (nodeId.equals(sceneFlow.getId())) {
                            return new JSONObject().put("error", "CANNOT_DELETE_ROOT")
                                    .put("message", "Cannot delete the root SceneFlow node");
                        }

                        // Clear UI-specific dock point tracking before deletion
                        mOccupiedDockPoints.remove(nodeId);

                        // Delegate to SceneFlowService for domain logic
                        boolean deleted = mSceneFlowService.deleteNode(project, nodeId);

                        if (!deleted) {
                            return new JSONObject().put("error", "NODE_NOT_FOUND")
                                    .put("message", "Node not found or could not be deleted");
                        }

                        JSONObject payloadResp = new JSONObject();
                        payloadResp.put("nodeId", nodeId);
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        String errorCode = response.getString("error");
                        String errorMsg = response.optString("message", "Node not found");
                        sendError(ctx, requestId, errorCode, errorMsg);
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    if (response.has("dirty")) {
                        emitUiProjectDirty(projectId, response.getBoolean("dirty"), List.of("sceneflow"));
                    }
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Selection.Copy
                case "SceneFlow.Selection.Copy": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    JSONArray nodeIds = body.optJSONArray("nodeIds");
                    if (projectId == null || projectId.isBlank() || nodeIds == null) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId or nodeIds");
                        return;
                    }
                    if (sourceClientId == null || sourceClientId.isBlank()) {
                        sendError(ctx, requestId, "CLIENT_ID_REQUIRED", "Missing sourceClientId");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        // Phase 6: Use EditorProjectService instead of EditorInstance
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        // Resolve the active SuperNode from superNodeId or default to root
                        SuperNode active = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        if (active == null) {
                            active = sceneFlow;
                        }
                        List<BasicNode> copies = new ArrayList<>();
                        for (int i = 0; i < nodeIds.length(); i++) {
                            String id = nodeIds.optString(i, "").trim();
                            if (id.isEmpty()) {
                                continue;
                            }
                            BasicNode node = resolveNodeById(active, id);
                            if (node == null || node.isHistoryNode()) {
                                continue;
                            }
                            BasicNode copy = node.getCopy();
                            if (copy != null) {
                                copies.add(copy);
                            }
                        }
                        if (copies.isEmpty()) {
                            return new JSONObject().put("error", "NODE_NOT_FOUND");
                        }
                        mSceneFlowClipboard.put(sourceClientId, new SceneFlowClipboard(projectId, copies));
                        return new JSONObject().put("count", copies.size());
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), "No nodes to copy");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Selection.Paste
                case "SceneFlow.Selection.Paste": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    double dxRaw = body.has("dx") ? body.optDouble("dx", Double.NaN) : Double.NaN;
                    double dyRaw = body.has("dy") ? body.optDouble("dy", Double.NaN) : Double.NaN;
                    if (projectId == null || projectId.isBlank() || !isFinite(dxRaw) || !isFinite(dyRaw)) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId or offset");
                        return;
                    }
                    if (sourceClientId == null || sourceClientId.isBlank()) {
                        sendError(ctx, requestId, "CLIENT_ID_REQUIRED", "Missing sourceClientId");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        // Phase 6: Use EditorProjectService instead of EditorInstance
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlowClipboard clipboard = mSceneFlowClipboard.get(sourceClientId);
                        if (clipboard == null || clipboard.nodes == null || clipboard.nodes.isEmpty()) {
                            return new JSONObject().put("error", "CLIPBOARD_EMPTY");
                        }
                        List<BasicNode> nodes = new ArrayList<>();
                        for (BasicNode stored : clipboard.nodes) {
                            if (stored == null) {
                                continue;
                            }
                            BasicNode copy = stored.getCopy();
                            if (copy != null) {
                                nodes.add(copy);
                            }
                        }
                        if (nodes.isEmpty()) {
                            return new JSONObject().put("error", "CLIPBOARD_EMPTY");
                        }
                        Point min = minNodePosition(nodes);
                        int dx = (int) Math.round(dxRaw);
                        int dy = (int) Math.round(dyRaw);
                        if (min != null) {
                            if (min.x + dx < 1) {
                                dx += 1 - (min.x + dx);
                            }
                            if (min.y + dy < 1) {
                                dy += 1 - (min.y + dy);
                            }
                        }
                        Map<BasicNode, String> oldIds = new IdentityHashMap<>();
                        for (BasicNode node : nodes) {
                            collectNodeIdsRecursive(node, oldIds);
                        }
                        // Phase 6: Reassign IDs headlessly using generateNextNodeId
                        SceneFlow sceneFlow = project.getSceneFlow();
                        reassignAllIDsHeadless(sceneFlow, nodes);
                        Map<String, String> idMap = new HashMap<>();
                        for (Map.Entry<BasicNode, String> entry : oldIds.entrySet()) {
                            BasicNode node = entry.getKey();
                            String oldId = entry.getValue();
                            if (node != null && oldId != null && !oldId.isBlank()) {
                                idMap.put(oldId, node.getId());
                            }
                        }
                        // Phase 6: Resolve active SuperNode headlessly
                        SuperNode activeSuperNode = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        if (activeSuperNode == null) {
                            activeSuperNode = sceneFlow;
                        }
                        Set<String> allowedTargetIds = new HashSet<>(idMap.values());
                        HashMap<String, BasicNode> startBefore = new HashMap<>(activeSuperNode.getStartNodeMap());
                        Set<String> startIds = new HashSet<>();
                        for (Map.Entry<BasicNode, String> entry : oldIds.entrySet()) {
                            String oldId = entry.getValue();
                            if (oldId == null || oldId.isBlank()) {
                                continue;
                            }
                            if (startBefore.containsKey(oldId)) {
                                String newId = idMap.get(oldId);
                                if (newId != null && !newId.isBlank()) {
                                    startIds.add(newId);
                                }
                            }
                        }
                        for (BasicNode node : nodes) {
                            normalizeEdgeIdsRecursive(node, allowedTargetIds);
                        }
                        for (BasicNode node : nodes) {
                            remapAltStartMapsRecursive(node, idMap);
                        }
                        for (BasicNode node : nodes) {
                            normalizeSuperNodesRecursive(node);
                        }
                        for (BasicNode node : nodes) {
                            offsetNodeGraphics(node, dx, dy);
                        }
                        // Phase 6: Extract and store edges before adding nodes (headless - no WorkSpacePanel)
                        Map<BasicNode, ArrayList<GuargedEdge>> cEdges = new HashMap<>();
                        Map<BasicNode, ArrayList<RandomEdge>> pEdges = new HashMap<>();
                        Map<BasicNode, ArrayList<ForkingEdge>> fEdges = new HashMap<>();
                        Map<BasicNode, ArrayList<InterruptEdge>> iEdges = new HashMap<>();
                        Map<BasicNode, AbstractEdge> defaultEdges = new HashMap<>();
                        for (BasicNode node : nodes) {
                            if (node.hasEdge()) {
                                switch (node.getFlavour()) {
                                    case CNODE: {
                                        ArrayList<GuargedEdge> edges = filterEdgesForCopy(node.getCEdgeList(), node.getId(), allowedTargetIds);
                                        cEdges.put(node, edges);
                                        node.removeAllCEdges();
                                        if (node.hasDEdge()) {
                                            AbstractEdge edge = node.getDedge();
                                            if (normalizeEdgeForCopy(edge, node.getId(), allowedTargetIds)) {
                                                defaultEdges.put(node, edge);
                                            }
                                            node.removeDEdge();
                                        }
                                        break;
                                    }
                                    case PNODE: {
                                        ArrayList<RandomEdge> edges = filterEdgesForCopy(node.getPEdgeList(), node.getId(), allowedTargetIds);
                                        pEdges.put(node, edges);
                                        node.removeAllPEdges();
                                        break;
                                    }
                                    case FNODE: {
                                        ArrayList<ForkingEdge> edges = filterEdgesForCopy(node.getFEdgeList(), node.getId(), allowedTargetIds);
                                        fEdges.put(node, edges);
                                        node.removeAllFEdges();
                                        break;
                                    }
                                    case INODE: {
                                        ArrayList<InterruptEdge> edges = filterEdgesForCopy(node.getIEdgeList(), node.getId(), allowedTargetIds);
                                        iEdges.put(node, edges);
                                        node.removeAllIEdges();
                                        if (node.hasDEdge()) {
                                            AbstractEdge edge = node.getDedge();
                                            if (normalizeEdgeForCopy(edge, node.getId(), allowedTargetIds)) {
                                                defaultEdges.put(node, edge);
                                            }
                                            node.removeDEdge();
                                        }
                                        break;
                                    }
                                    case TNODE:
                                    case ENODE:
                                    case NONE: {
                                        if (node.hasDEdge()) {
                                            AbstractEdge edge = node.getDedge();
                                            if (normalizeEdgeForCopy(edge, node.getId(), allowedTargetIds)) {
                                                defaultEdges.put(node, edge);
                                            }
                                            node.removeDEdge();
                                        }
                                        break;
                                    }
                                    default:
                                        break;
                                }
                            }
                        }
                        // Offset edge graphics
                        final int finalDx = dx;
                        final int finalDy = dy;
                        for (Map.Entry<BasicNode, ArrayList<GuargedEdge>> entry : cEdges.entrySet()) {
                            for (GuargedEdge edge : entry.getValue()) {
                                offsetEdgeGraphics(edge, finalDx, finalDy);
                            }
                        }
                        for (Map.Entry<BasicNode, ArrayList<RandomEdge>> entry : pEdges.entrySet()) {
                            for (RandomEdge edge : entry.getValue()) {
                                offsetEdgeGraphics(edge, finalDx, finalDy);
                            }
                        }
                        for (Map.Entry<BasicNode, ArrayList<ForkingEdge>> entry : fEdges.entrySet()) {
                            for (ForkingEdge edge : entry.getValue()) {
                                offsetEdgeGraphics(edge, finalDx, finalDy);
                            }
                        }
                        for (Map.Entry<BasicNode, ArrayList<InterruptEdge>> entry : iEdges.entrySet()) {
                            for (InterruptEdge edge : entry.getValue()) {
                                offsetEdgeGraphics(edge, finalDx, finalDy);
                            }
                        }
                        for (Map.Entry<BasicNode, AbstractEdge> entry : defaultEdges.entrySet()) {
                            offsetEdgeGraphics(entry.getValue(), finalDx, finalDy);
                        }
                        // Phase 6: Add nodes directly to the model (headless - no WorkSpacePanel)
                        JSONArray newNodeIds = new JSONArray();
                        Map<String, BasicNode> createdNodes = new HashMap<>();
                        final SuperNode targetSuperNode = activeSuperNode;
                        for (BasicNode node : nodes) {
                            // Set the parent node reference
                            node.setParentNode(targetSuperNode);
                            // Add to parent based on type
                            if (node instanceof SuperNode) {
                                targetSuperNode.addSuperNode((SuperNode) node);
                            } else {
                                targetSuperNode.addNode(node);
                            }
                            newNodeIds.put(node.getId());
                            createdNodes.put(node.getId(), node);
                        }
                        // Handle start flag for pasted nodes
                        if (!startIds.isEmpty()) {
                            for (String startId : startIds) {
                                BasicNode dataNode = createdNodes.get(startId);
                                if (dataNode == null) {
                                    continue;
                                }
                                // Phase 6: Update start flag headlessly
                                targetSuperNode.addStartNode(dataNode);
                                occupyStartSignDockPoint(dataNode.getId());
                            }
                        }
                        // Phase 6: Add edges directly to the model (headless - no CreateEdgeAction)
                        for (BasicNode node : nodes) {
                            if (cEdges.containsKey(node)) {
                                for (GuargedEdge edge : cEdges.get(node)) {
                                    // Verify target exists in created nodes
                                    String targetId = edge.getTargetUnid();
                                    if (targetId != null && !targetId.isBlank() && createdNodes.containsKey(targetId)) {
                                        node.addCEdge(edge);
                                    }
                                }
                            }
                            if (pEdges.containsKey(node)) {
                                for (RandomEdge edge : pEdges.get(node)) {
                                    String targetId = edge.getTargetUnid();
                                    if (targetId != null && !targetId.isBlank() && createdNodes.containsKey(targetId)) {
                                        node.addPEdge(edge);
                                    }
                                }
                            }
                            if (fEdges.containsKey(node)) {
                                for (ForkingEdge edge : fEdges.get(node)) {
                                    String targetId = edge.getTargetUnid();
                                    if (targetId != null && !targetId.isBlank() && createdNodes.containsKey(targetId)) {
                                        node.addFEdge(edge);
                                    }
                                }
                            }
                            if (iEdges.containsKey(node)) {
                                for (InterruptEdge edge : iEdges.get(node)) {
                                    String targetId = edge.getTargetUnid();
                                    if (targetId != null && !targetId.isBlank() && createdNodes.containsKey(targetId)) {
                                        node.addIEdge(edge);
                                    }
                                }
                            }
                            if (defaultEdges.containsKey(node)) {
                                AbstractEdge edge = defaultEdges.get(node);
                                if (edge != null) {
                                    String targetId = edge.getTargetUnid();
                                    if (targetId != null && !targetId.isBlank() && createdNodes.containsKey(targetId)) {
                                        node.setDedge(edge);
                                    }
                                }
                            }
                        }
                        // Phase 6: Initialize dock points for pasted nodes' edges
                        EditorConfig config = project.getEditorConfig();
                        int nodeWidth = config != null ? config.sNODEWIDTH : 90;
                        int nodeHeight = config != null ? config.sNODEHEIGHT : 90;
                        for (BasicNode node : nodes) {
                            processNodeEdgesForDocking(node, nodeWidth, nodeHeight);
                        }
                        JSONObject payloadResp = new JSONObject();
                        payloadResp.put("nodeIds", newNodeIds);
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, targetSuperNode));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), "Clipboard empty");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    broadcastDirtyIfPresent(response, projectId);
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Node.TypeDef.Add
                case "SceneFlow.Node.TypeDef.Add": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String nodeId = body.optString("nodeId", "");
                    JSONObject typeDefJson = body.optJSONObject("typeDef");
                    int index = body.has("index") ? body.optInt("index", -1) : -1;
                    if (projectId == null || projectId.isBlank() || typeDefJson == null) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId, nodeId, or typeDef");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        BasicNode dataNode = nodeId.isBlank() ? sceneFlow : findNodeRecursive(sceneFlow, nodeId);
                        if (dataNode == null) {
                            return new JSONObject().put("error", "NODE_NOT_FOUND");
                        }
                        StringBuilder error = new StringBuilder();
                        DataTypeDefinition typeDef = parseTypeDef(typeDefJson, error);
                        if (typeDef == null) {
                            return new JSONObject().put("error", error.length() > 0 ? error.toString() : "TYPEDEF_INVALID");
                        }
                        List<DataTypeDefinition> list = dataNode.getTypeDefList();
                        int insertIndex = index < 0 || index > list.size() ? list.size() : index;
                        list.add(insertIndex, typeDef);
                        JSONObject payloadResp = new JSONObject();
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), "Type definition update failed");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    if (response.has("dirty")) {
                        emitUiProjectDirty(projectId, response.getBoolean("dirty"), List.of("sceneflow"));
                    }
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Node.TypeDef.Update
                case "SceneFlow.Node.TypeDef.Update": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String nodeId = body.optString("nodeId", "");
                    JSONObject typeDefJson = body.optJSONObject("typeDef");
                    int index = body.optInt("index", -1);
                    if (projectId == null || projectId.isBlank()
                            || typeDefJson == null || index < 0) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId, nodeId, typeDef, or index");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        BasicNode dataNode = resolveNodeForDefinitions(sceneFlow, nodeId);
                        if (dataNode == null) {
                            return new JSONObject().put("error", "NODE_NOT_FOUND");
                        }
                        List<DataTypeDefinition> list = dataNode.getTypeDefList();
                        if (index >= list.size()) {
                            return new JSONObject().put("error", "TYPEDEF_NOT_FOUND");
                        }
                        StringBuilder error = new StringBuilder();
                        DataTypeDefinition typeDef = parseTypeDef(typeDefJson, error);
                        if (typeDef == null) {
                            return new JSONObject().put("error", error.length() > 0 ? error.toString() : "TYPEDEF_INVALID");
                        }

                        // Update the type definition in the list
                        list.set(index, typeDef);

                        JSONObject payloadResp = new JSONObject();
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), "Type definition update failed");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    broadcastDirtyIfPresent(response, projectId);
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Node.TypeDef.Delete
                case "SceneFlow.Node.TypeDef.Delete": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String nodeId = body.optString("nodeId", "");
                    int index = body.optInt("index", -1);
                    if (projectId == null || projectId.isBlank() || index < 0) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId, nodeId, or index");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        BasicNode dataNode = resolveNodeForDefinitions(sceneFlow, nodeId);
                        if (dataNode == null) {
                            return new JSONObject().put("error", "NODE_NOT_FOUND");
                        }
                        List<DataTypeDefinition> list = dataNode.getTypeDefList();
                        if (index >= list.size()) {
                            return new JSONObject().put("error", "TYPEDEF_NOT_FOUND");
                        }

                        // Remove the type definition from the list
                        list.remove(index);

                        JSONObject payloadResp = new JSONObject();
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), "Type definition delete failed");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    broadcastDirtyIfPresent(response, projectId);
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Node.TypeDef.Move
                case "SceneFlow.Node.TypeDef.Move": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String nodeId = body.optString("nodeId", "");
                    int from = body.optInt("from", -1);
                    int to = body.optInt("to", -1);
                    if (projectId == null || projectId.isBlank()
                            || from < 0 || to < 0) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId, nodeId, or indices");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        BasicNode dataNode = resolveNodeForDefinitions(sceneFlow, nodeId);
                        if (dataNode == null) {
                            return new JSONObject().put("error", "NODE_NOT_FOUND");
                        }
                        List<DataTypeDefinition> list = dataNode.getTypeDefList();
                        if (from >= list.size() || to >= list.size()) {
                            return new JSONObject().put("error", "TYPEDEF_NOT_FOUND");
                        }
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        if (from == to) {
                            JSONObject payloadResp = new JSONObject();
                            payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                            payloadResp.put("dirty", project.hasChanged());
                            return payloadResp;
                        }

                        // Move the entry from 'from' index to 'to' index
                        DataTypeDefinition entry = list.remove(from);
                        list.add(to, entry);

                        JSONObject payloadResp = new JSONObject();
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), "Type definition move failed");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    broadcastDirtyIfPresent(response, projectId);
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Node.VarDef.Add
                case "SceneFlow.Node.VarDef.Add": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String nodeId = body.optString("nodeId", "");
                    JSONObject varDefJson = body.optJSONObject("varDef");
                    int index = body.has("index") ? body.optInt("index", -1) : -1;
                    if (projectId == null || projectId.isBlank() || varDefJson == null) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId, nodeId, or varDef");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        BasicNode dataNode = nodeId.isBlank() ? sceneFlow : findNodeRecursive(sceneFlow, nodeId);
                        if (dataNode == null) {
                            return new JSONObject().put("error", "NODE_NOT_FOUND");
                        }
                        StringBuilder error = new StringBuilder();
                        VariableDefinition varDef = parseVarDef(varDefJson, dataNode, error);
                        if (varDef == null) {
                            return new JSONObject().put("error", error.length() > 0 ? error.toString() : "VARDEF_INVALID");
                        }
                        List<VariableDefinition> list = dataNode.getVarDefList();
                        int insertIndex = index < 0 || index > list.size() ? list.size() : index;
                        list.add(insertIndex, varDef);
                        JSONObject payloadResp = new JSONObject();
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), "Variable definition update failed");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    if (response.has("dirty")) {
                        emitUiProjectDirty(projectId, response.getBoolean("dirty"), List.of("sceneflow"));
                    }
                    if (response.optBoolean("scriptChanged")) {
                        emitUiScriptSnapshot(projectId);
                        emitUiScriptElements(projectId);
                    }
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Node.VarDef.Update
                case "SceneFlow.Node.VarDef.Update": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String nodeId = body.optString("nodeId", "");
                    JSONObject varDefJson = body.optJSONObject("varDef");
                    int index = body.optInt("index", -1);
                    if (projectId == null || projectId.isBlank()
                            || varDefJson == null || index < 0) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId, nodeId, varDef, or index");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        BasicNode dataNode = nodeId.isBlank() ? sceneFlow : findNodeRecursive(sceneFlow, nodeId);
                        if (dataNode == null) {
                            return new JSONObject().put("error", "NODE_NOT_FOUND");
                        }
                        List<VariableDefinition> list = dataNode.getVarDefList();
                        if (index >= list.size()) {
                            return new JSONObject().put("error", "VARDEF_NOT_FOUND");
                        }
                        String oldName = null;
                        VariableDefinition existing = list.get(index);
                        if (existing != null) {
                            oldName = existing.getName();
                        }
                        StringBuilder error = new StringBuilder();
                        VariableDefinition varDef = parseVarDef(varDefJson, dataNode, error);
                        if (varDef == null) {
                            return new JSONObject().put("error", error.length() > 0 ? error.toString() : "VARDEF_INVALID");
                        }
                        List<VariableDefinition> before = copyVarDefList(list);
                        list.set(index, varDef);
                        String newName = varDef.getName();
                        VarRenameSnapshot renameSnapshot = null;
                        if (oldName != null && newName != null && !oldName.equals(newName)) {
                            StringBuilder renameError = new StringBuilder();
                            renameSnapshot = renameVariableReferences(project, oldName, newName, renameError);
                            if (renameSnapshot == null) {
                                applyVarDefList(dataNode, before);
                                String renameMessage = renameError.length() > 0 ? renameError.toString() : "Variable rename failed.";
                                return new JSONObject().put("error", "VARDEF_RENAME_FAILED").put("message", renameMessage);
                            }
                            renameSnapshot.nodeVarDefsBefore.putIfAbsent(dataNode, before);
                            renameSnapshot.nodeVarDefsAfter.putIfAbsent(dataNode, copyVarDefList(dataNode.getVarDefList()));
                        }
                        // Note: Undo/redo support removed for headless operation
                        JSONObject payloadResp = new JSONObject();
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        if (renameSnapshot != null && renameSnapshot.scriptChanged) {
                            payloadResp.put("scriptChanged", true);
                        }
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), "Variable definition update failed");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    if (response.has("dirty")) {
                        emitUiProjectDirty(projectId, response.getBoolean("dirty"), List.of("sceneflow"));
                    }
                    if (response.optBoolean("scriptChanged", false)) {
                        emitUiScriptSnapshot(projectId);
                        emitUiScriptElements(projectId);
                    }
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Node.VarDef.Delete
                case "SceneFlow.Node.VarDef.Delete": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String nodeId = body.optString("nodeId", "");
                    int index = body.optInt("index", -1);
                    if (projectId == null || projectId.isBlank() || index < 0) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId, nodeId, or index");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        BasicNode dataNode = nodeId.isBlank() ? sceneFlow : findNodeRecursive(sceneFlow, nodeId);
                        if (dataNode == null) {
                            return new JSONObject().put("error", "NODE_NOT_FOUND");
                        }
                        List<VariableDefinition> list = dataNode.getVarDefList();
                        if (index >= list.size()) {
                            return new JSONObject().put("error", "VARDEF_NOT_FOUND");
                        }
                        list.remove(index);
                        JSONObject payloadResp = new JSONObject();
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), "Variable definition update failed");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    if (response.has("dirty")) {
                        emitUiProjectDirty(projectId, response.getBoolean("dirty"), List.of("sceneflow"));
                    }
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Node.VarDef.Move
                case "SceneFlow.Node.VarDef.Move": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String nodeId = body.optString("nodeId", "");
                    int from = body.optInt("from", -1);
                    int to = body.optInt("to", -1);
                    if (projectId == null || projectId.isBlank()
                            || from < 0 || to < 0) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId, nodeId, or indices");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        BasicNode dataNode = resolveNodeForDefinitions(sceneFlow, nodeId);
                        if (dataNode == null) {
                            return new JSONObject().put("error", "NODE_NOT_FOUND");
                        }
                        List<VariableDefinition> list = dataNode.getVarDefList();
                        if (from >= list.size() || to >= list.size()) {
                            return new JSONObject().put("error", "VARDEF_NOT_FOUND");
                        }
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        if (from == to) {
                            JSONObject payloadResp = new JSONObject();
                            payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                            payloadResp.put("dirty", project.hasChanged());
                            return payloadResp;
                        }

                        // Move the entry from 'from' index to 'to' index
                        VariableDefinition entry = list.remove(from);
                        list.add(to, entry);

                        JSONObject payloadResp = new JSONObject();
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), "Variable definition move failed");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    broadcastDirtyIfPresent(response, projectId);
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Node.Cmd.Add
                case "SceneFlow.Node.Cmd.Add": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String nodeId = body.optString("nodeId", "");
                    JSONObject commandJson = body.optJSONObject("command");
                    int index = body.has("index") ? body.optInt("index", -1) : -1;
                    if (projectId == null || projectId.isBlank() || commandJson == null) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId, nodeId, or command");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        BasicNode dataNode = nodeId.isBlank() ? sceneFlow : findNodeRecursive(sceneFlow, nodeId);
                        if (dataNode == null) {
                            return new JSONObject().put("error", "NODE_NOT_FOUND");
                        }
                        StringBuilder error = new StringBuilder();
                        Command command = parseCommandText(commandJson.optString("text", ""), error);
                        if (command == null) {
                            return new JSONObject().put("error", error.length() > 0 ? error.toString() : "COMMAND_INVALID");
                        }
                        List<Command> list = dataNode.getCmdList();
                        int insertIndex = index < 0 || index > list.size() ? list.size() : index;
                        list.add(insertIndex, command);
                        JSONObject payloadResp = new JSONObject();
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), "Command update failed");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    if (response.has("dirty")) {
                        emitUiProjectDirty(projectId, response.getBoolean("dirty"), List.of("sceneflow"));
                    }
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Node.Cmd.Update
                case "SceneFlow.Node.Cmd.Update": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String nodeId = body.optString("nodeId", "");
                    JSONObject commandJson = body.optJSONObject("command");
                    int index = body.optInt("index", -1);
                    if (projectId == null || projectId.isBlank()
                            || commandJson == null || index < 0) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId, nodeId, command, or index");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        BasicNode dataNode = resolveNodeForDefinitions(sceneFlow, nodeId);
                        if (dataNode == null) {
                            return new JSONObject().put("error", "NODE_NOT_FOUND");
                        }
                        List<Command> list = dataNode.getCmdList();
                        if (index >= list.size()) {
                            return new JSONObject().put("error", "COMMAND_NOT_FOUND");
                        }
                        StringBuilder error = new StringBuilder();
                        Command command = parseCommandText(commandJson.optString("text", ""), error);
                        if (command == null) {
                            return new JSONObject().put("error", error.length() > 0 ? error.toString() : "COMMAND_INVALID");
                        }

                        // Update the command in the list
                        list.set(index, command);

                        JSONObject payloadResp = new JSONObject();
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), "Command update failed");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    broadcastDirtyIfPresent(response, projectId);
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Node.Cmd.Delete
                case "SceneFlow.Node.Cmd.Delete": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String nodeId = body.optString("nodeId", "");
                    int index = body.optInt("index", -1);
                    if (projectId == null || projectId.isBlank() || index < 0) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId, nodeId, or index");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        BasicNode dataNode = resolveNodeForDefinitions(sceneFlow, nodeId);
                        if (dataNode == null) {
                            return new JSONObject().put("error", "NODE_NOT_FOUND");
                        }
                        List<Command> list = dataNode.getCmdList();
                        if (index >= list.size()) {
                            return new JSONObject().put("error", "COMMAND_NOT_FOUND");
                        }

                        // Remove the command from the list
                        list.remove(index);

                        JSONObject payloadResp = new JSONObject();
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), "Command delete failed");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    broadcastDirtyIfPresent(response, projectId);
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Node.Cmd.Move
                case "SceneFlow.Node.Cmd.Move": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String nodeId = body.optString("nodeId", "");
                    int from = body.optInt("from", -1);
                    int to = body.optInt("to", -1);
                    if (projectId == null || projectId.isBlank()
                            || from < 0 || to < 0) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId, nodeId, or indices");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        BasicNode dataNode = resolveNodeForDefinitions(sceneFlow, nodeId);
                        if (dataNode == null) {
                            return new JSONObject().put("error", "NODE_NOT_FOUND");
                        }
                        List<Command> list = dataNode.getCmdList();
                        if (from >= list.size() || to >= list.size()) {
                            return new JSONObject().put("error", "COMMAND_NOT_FOUND");
                        }
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        if (from == to) {
                            JSONObject payloadResp = new JSONObject();
                            payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                            payloadResp.put("dirty", project.hasChanged());
                            return payloadResp;
                        }

                        // Move the entry from 'from' index to 'to' index
                        Command entry = list.remove(from);
                        list.add(to, entry);

                        JSONObject payloadResp = new JSONObject();
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), "Command move failed");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    broadcastDirtyIfPresent(response, projectId);
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Comment.Create
                case "SceneFlow.Comment.Create": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    double x = body.has("x") ? body.optDouble("x", Double.NaN) : Double.NaN;
                    double y = body.has("y") ? body.optDouble("y", Double.NaN) : Double.NaN;
                    int width = body.has("width") ? body.optInt("width", 120) : 120;
                    int height = body.has("height") ? body.optInt("height", 90) : 90;
                    String text = body.optString("text", "");
                    final boolean hasText = text != null && !text.isBlank();
                    final String finalText = text;
                    if (projectId == null || projectId.isBlank() || !isFinite(x) || !isFinite(y)) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId or coordinates");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        // Resolve target SuperNode
                        SuperNode targetSuperNode = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);

                        // Create comment badge directly in the model
                        CommentBadge badge = new CommentBadge();
                        int clampedX = clampPositive((int) Math.round(x));
                        int clampedY = clampPositive((int) Math.round(y));

                        // Set graphics
                        CommentGraphics graphics = new CommentGraphics();
                        graphics.setRectangle(new CommentBoundary(clampedX, clampedY, width, height));
                        badge.setGraphics(graphics);

                        // Set text
                        if (hasText) {
                            badge.setHTMLText(finalText);
                        }

                        // Add to SuperNode
                        targetSuperNode.addComment(badge);
                        // Project dirty state is tracked automatically via hash comparison

                        List<CommentBadge> list = targetSuperNode.getCommentList();
                        JSONObject payloadResp = new JSONObject();
                        payloadResp.put("commentId", "C" + (list.size() - 1));
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, targetSuperNode));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), "Failed to create comment");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    broadcastDirtyIfPresent(response, projectId);
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Comment.Update
                case "SceneFlow.Comment.Update": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String commentId = body.optString("commentId", null);
                    double x = body.has("x") ? body.optDouble("x", Double.NaN) : Double.NaN;
                    double y = body.has("y") ? body.optDouble("y", Double.NaN) : Double.NaN;
                    int width = body.has("width") ? body.optInt("width", -1) : -1;
                    int height = body.has("height") ? body.optInt("height", -1) : -1;
                    String text = body.has("text") ? body.optString("text", null) : null;
                    final boolean hasText = text != null;
                    final String finalText = text;
                    if (projectId == null || projectId.isBlank() || commentId == null || commentId.isBlank()
                            || !isFinite(x) || !isFinite(y)) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId, commentId, or coordinates");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        SuperNode targetSuperNode = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);

                        CommentBadge badge = resolveCommentById(targetSuperNode, commentId);
                        if (badge == null) {
                            return new JSONObject().put("error", "COMMENT_NOT_FOUND");
                        }

                        CommentGraphics graphics = badge.getGraphics();
                        if (graphics == null) {
                            graphics = new CommentGraphics();
                            badge.setGraphics(graphics);
                        }

                        CommentBoundary rect = graphics.getRectangle();
                        int nextWidth = width > 0 ? width : (rect != null ? rect.getWidth() : 100);
                        int nextHeight = height > 0 ? height : (rect != null ? rect.getHeight() : 100);
                        int clampedX = clampPositive((int) Math.round(x));
                        int clampedY = clampPositive((int) Math.round(y));
                        graphics.setRectangle(new CommentBoundary(clampedX, clampedY, nextWidth, nextHeight));

                        if (hasText) {
                            badge.setHTMLText(finalText);
                        }

                        // Project dirty state is tracked automatically via hash comparison

                        JSONObject payloadResp = new JSONObject();
                        payloadResp.put("commentId", commentId);
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, targetSuperNode));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), "Comment not found");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    broadcastDirtyIfPresent(response, projectId);
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Comment.Delete
                case "SceneFlow.Comment.Delete": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String commentId = body.optString("commentId", null);
                    if (projectId == null || projectId.isBlank() || commentId == null || commentId.isBlank()) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId or commentId");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        SuperNode targetSuperNode = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);

                        CommentBadge badge = resolveCommentById(targetSuperNode, commentId);
                        if (badge == null) {
                            return new JSONObject().put("error", "COMMENT_NOT_FOUND");
                        }

                        // Remove comment from SuperNode
                        targetSuperNode.removeComment(badge);
                        // Project dirty state is tracked automatically via hash comparison

                        JSONObject payloadResp = new JSONObject();
                        payloadResp.put("commentId", commentId);
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, targetSuperNode));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), "Comment not found");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    broadcastDirtyIfPresent(response, projectId);
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Edge.Create
                case "SceneFlow.Edge.Create": {
                    System.out.println(">>> SceneFlow.Edge.Create handler entered");
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String sourceId = body.optString("sourceId", null);
                    String targetId = body.optString("targetId", null);
                    String edgeType = body.optString("edgeType", body.optString("type", "EEDGE"));
                    System.out.println(">>> Edge.Create: projectId=" + projectId + ", sourceId=" + sourceId + ", targetId=" + targetId + ", edgeType=" + edgeType);
                    if (projectId == null || projectId.isBlank()
                            || sourceId == null || sourceId.isBlank()
                            || targetId == null || targetId.isBlank()) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId, sourceId, or targetId");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    Edge.TYPE resolvedType = parseEdgeCreateType(edgeType);
                    if (resolvedType == null) {
                        sendError(ctx, requestId, "EDGE_NOT_ALLOWED", "Unsupported edge type");
                        return;
                    }
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        BasicNode sourceNode = findNodeRecursive(sceneFlow, sourceId);
                        BasicNode targetNode = findNodeRecursive(sceneFlow, targetId);
                        if (sourceNode == null || targetNode == null) {
                            return new JSONObject().put("error", "NODE_NOT_FOUND");
                        }
                        AbstractEdge dataEdge = null;
                        switch (resolvedType) {
                            case EEDGE:
                                dataEdge = new EpsilonEdge();
                                break;
                            case FEDGE:
                                dataEdge = new ForkingEdge();
                                break;
                            case TEDGE: {
                                TimeoutEdge tedge = new TimeoutEdge();
                                try {
                                    tedge.setTimeout(1000);
                                } catch (NumberFormatException ignored) {
                                }
                                dataEdge = tedge;
                                break;
                            }
                            case CEDGE: {
                                GuargedEdge cedge = new GuargedEdge();
                                cedge.setCondition(new BoolLiteral(true));
                                dataEdge = cedge;
                                break;
                            }
                            case IEDGE: {
                                InterruptEdge iedge = new InterruptEdge();
                                iedge.setCondition(new BoolLiteral(true));
                                dataEdge = iedge;
                                break;
                            }
                            case PEDGE: {
                                RandomEdge pedge = new RandomEdge();
                                int sum = sumProbabilities(sourceNode, null);
                                int remaining = Math.max(0, 100 - sum);
                                int defaultProb = sourceNode.getPEdgeList().isEmpty() ? 100 : 50;
                                int desired = remaining > 0 ? Math.min(defaultProb, remaining) : 0;
                                pedge.setProbability(desired);
                                dataEdge = pedge;
                                break;
                            }
                            default:
                                break;
                        }
                        if (dataEdge == null) {
                            return new JSONObject().put("error", "EDGE_NOT_ALLOWED");
                        }
                        // Set source and target nodes (both references and string IDs)
                        dataEdge.setSourceNode(sourceNode);
                        dataEdge.setTargetNode(targetNode);
                        dataEdge.setSourceUnid(sourceNode.getId());
                        dataEdge.setTargetUnid(targetNode.getId());

                        // Phase 6: Initialize edge graphics with dock points and bezier control points
                        EdgeGraphics edgeGraphics = new EdgeGraphics();
                        EdgeArrow arrow = new EdgeArrow();
                        ArrayList<EdgePoint> points = new ArrayList<>();

                        // Get node positions and sizes
                        NodeGraphics sourceGraphics = sourceNode.getGraphics();
                        NodeGraphics targetGraphics = targetNode.getGraphics();
                        NodePosition srcPos = sourceGraphics != null ? sourceGraphics.getPosition() : null;
                        NodePosition tgtPos = targetGraphics != null ? targetGraphics.getPosition() : null;
                        double srcX = srcPos != null ? srcPos.getXPos() : 0;
                        double srcY = srcPos != null ? srcPos.getYPos() : 0;
                        double tgtX = tgtPos != null ? tgtPos.getXPos() : 100;
                        double tgtY = tgtPos != null ? tgtPos.getYPos() : 0;

                        // Get node sizes from EditorConfig
                        EditorConfig config = project.getEditorConfig();
                        int nodeWidth = config != null ? config.sNODEWIDTH : 90;
                        int nodeHeight = config != null ? config.sNODEHEIGHT : 90;
                        boolean srcIsSuperNode = sourceNode instanceof SuperNode;
                        boolean tgtIsSuperNode = targetNode instanceof SuperNode;

                        // Find best dock point pair - use special selection for self-loops
                        int[] dockPair;
                        boolean isSelfLoop = isSelfLoopEdge(sourceNode.getId(), targetNode.getId());
                        System.out.println(">>> Edge.Create isSelfLoop=" + isSelfLoop + " (source=" + sourceNode.getId() + ", target=" + targetNode.getId() + ")");

                        if (isSelfLoop) {
                            // Self-loop: use upper-half dock points for a nice upward loop
                            dockPair = findSelfLoopDockPointPair(sourceNode.getId(), nodeWidth, nodeHeight, srcIsSuperNode);
                            System.out.println(">>> Self-loop dock pair: [" + dockPair[0] + ", " + dockPair[1] + "]");
                        } else {
                            // Normal edge: minimize edge length
                            dockPair = findBestDockPointPair(
                                sourceNode.getId(), srcX, srcY, nodeWidth, nodeHeight, srcIsSuperNode,
                                targetNode.getId(), tgtX, tgtY, nodeWidth, nodeHeight, tgtIsSuperNode
                            );
                        }
                        int srcDockIdx = dockPair[0];
                        int tgtDockIdx = dockPair[1];

                        // Mark dock points as occupied (source=outgoing, target=incoming)
                        occupyDockPoint(sourceNode.getId(), srcDockIdx, true);
                        occupyDockPoint(targetNode.getId(), tgtDockIdx, false);

                        // Get absolute dock positions
                        double[] srcDock = getDockPointPosition(srcX, srcY, nodeWidth, nodeHeight, srcIsSuperNode, srcDockIdx);
                        double[] tgtDock = getDockPointPosition(tgtX, tgtY, nodeWidth, nodeHeight, tgtIsSuperNode, tgtDockIdx);

                        // Compute bezier control points (special handling for self-loops)
                        double[] srcCtrl;
                        double[] tgtCtrl;
                        if (isSelfLoopEdge(sourceNode.getId(), targetNode.getId())) {
                            // Self-loop: use special control points that curve outward
                            double nodeCenterX = srcX + nodeWidth / 2.0;
                            double nodeCenterY = srcY + nodeHeight / 2.0;
                            double[] loopCtrl = computeSelfLoopControlPoints(
                                srcDock[0], srcDock[1], tgtDock[0], tgtDock[1],
                                nodeCenterX, nodeCenterY, nodeWidth, nodeHeight
                            );
                            srcCtrl = new double[] { loopCtrl[0], loopCtrl[1] };
                            tgtCtrl = new double[] { loopCtrl[2], loopCtrl[3] };
                        } else {
                            // Normal edge: use Swing-style control points based on dock vectors
                            double srcCenterX = srcX + nodeWidth / 2.0;
                            double srcCenterY = srcY + nodeHeight / 2.0;
                            double tgtCenterX = tgtX + nodeWidth / 2.0;
                            double tgtCenterY = tgtY + nodeHeight / 2.0;
                            double[] allCtrl = computeInitialControlPoints(
                                srcDock[0], srcDock[1], srcCenterX, srcCenterY,
                                tgtDock[0], tgtDock[1], tgtCenterX, tgtCenterY,
                                nodeHeight, false
                            );
                            srcCtrl = new double[] { allCtrl[0], allCtrl[1] };
                            tgtCtrl = new double[] { allCtrl[2], allCtrl[3] };
                        }

                        // Create start point with bezier control
                        // EdgePoint constructor: (xPos, ctrlXPos, yPos, ctrlYPos)
                        EdgePoint startPoint = new EdgePoint(
                            (int) Math.round(srcDock[0]), (int) Math.round(srcCtrl[0]),
                            (int) Math.round(srcDock[1]), (int) Math.round(srcCtrl[1])
                        );
                        points.add(startPoint);

                        // Create end point with bezier control
                        // EdgePoint constructor: (xPos, ctrlXPos, yPos, ctrlYPos)
                        EdgePoint endPoint = new EdgePoint(
                            (int) Math.round(tgtDock[0]), (int) Math.round(tgtCtrl[0]),
                            (int) Math.round(tgtDock[1]), (int) Math.round(tgtCtrl[1])
                        );
                        points.add(endPoint);

                        if (isSelfLoop) {
                            double nodeCenterX = srcX + nodeWidth / 2.0;
                            double nodeCenterY = srcY + nodeHeight / 2.0;
                            System.out.println(">>> Self-loop node: pos=(" + (int)srcX + "," + (int)srcY + ") size=" + nodeWidth + "x" + nodeHeight + " center=(" + (int)nodeCenterX + "," + (int)nodeCenterY + ")");
                            System.out.println(">>> Self-loop points: start=(" + startPoint.getXPos() + "," + startPoint.getYPos() +
                                ") startCtrl=(" + startPoint.getCtrlXPos() + "," + startPoint.getCtrlYPos() +
                                ") end=(" + endPoint.getXPos() + "," + endPoint.getYPos() +
                                ") endCtrl=(" + endPoint.getCtrlXPos() + "," + endPoint.getCtrlYPos() + ")");
                        }

                        arrow.setPointList(points);
                        edgeGraphics.setConnection(arrow);
                        dataEdge.setGraphics(edgeGraphics);

                        // Add edge to source node based on type
                        switch (resolvedType) {
                            case CEDGE:
                                sourceNode.addCEdge((GuargedEdge) dataEdge);
                                break;
                            case EEDGE:
                            case TEDGE:
                                // TimeoutEdge and EpsilonEdge use the D-edge (default edge)
                                sourceNode.setDedge(dataEdge);
                                break;
                            case FEDGE:
                                sourceNode.addFEdge((ForkingEdge) dataEdge);
                                break;
                            case IEDGE:
                                sourceNode.addIEdge((InterruptEdge) dataEdge);
                                break;
                            case PEDGE:
                                sourceNode.addPEdge((RandomEdge) dataEdge);
                                break;
                        }

                        // Compute edge ID by finding its position in the snapshot
                        String newEdgeId = null;
                        int edgeIdx = 0;
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        for (BasicNode node : snapshotTarget.getNodeAndSuperNodeList()) {
                            for (AbstractEdge edge : node.getEdgeList()) {
                                if (edge == dataEdge) {
                                    newEdgeId = "E" + edgeIdx;
                                    break;
                                }
                                edgeIdx++;
                            }
                            if (newEdgeId != null) break;
                        }

                        JSONObject payloadResp = new JSONObject();
                        if (newEdgeId != null) {
                            payloadResp.put("edgeId", newEdgeId);
                        }
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        String code = response.getString("error");
                        sendError(ctx, requestId, code, "Failed to create edge");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    if (response.has("dirty")) {
                        emitUiProjectDirty(projectId, response.getBoolean("dirty"), List.of("sceneflow"));
                    }
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Edge.Update
                case "SceneFlow.Edge.Update": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String edgeId = body.optString("edgeId", null);
                    String sourceId = body.optString("sourceId", null);
                    String targetId = body.optString("targetId", null);
                    JSONObject fields = body.optJSONObject("fields");
                    boolean hasEdgeId = edgeId != null && !edgeId.isBlank();
                    if (projectId == null || projectId.isBlank()
                            || (!hasEdgeId && (sourceId == null || sourceId.isBlank() || targetId == null || targetId.isBlank()))) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId or edge identifiers");
                        return;
                    }
                    final JSONObject sourcePayload = fields == null ? body : fields;
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();

                        // Find the edge
                        AbstractEdge dataEdge = hasEdgeId
                                ? resolveEdgeById(sceneFlow, edgeId)
                                : resolveEdgeByNodes(sceneFlow, sourceId, targetId);
                        if (dataEdge == null) {
                            return new JSONObject().put("error", "EDGE_NOT_FOUND");
                        }

                        // Apply edge updates (headless - no WorkSpacePanel)
                        String error = applyEdgeUpdatesHeadless(dataEdge, sceneFlow, sourcePayload, project.getEditorConfig());
                        if (error != null) {
                            return new JSONObject().put("error", "EDGE_UPDATE_FAILED").put("message", error);
                        }

                        JSONObject payloadResp = new JSONObject();
                        payloadResp.put("edgeId", edgeId == null ? "" : edgeId);
                        // Phase 6: Use resolveSnapshotTarget to maintain view context
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), response.optString("message", "Failed to update edge"));
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    return;
                }
                // Phase 6: Headless version - Edge path operations handled by Web UI
                case "SceneFlow.Edge.Normalize":
                case "SceneFlow.Edge.Straighten": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String edgeId = body.optString("edgeId", null);
                    if (projectId == null || projectId.isBlank()
                            || edgeId == null || edgeId.isBlank()) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId or edgeId");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    final boolean isNormalize = "SceneFlow.Edge.Normalize".equals(name);
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        AbstractEdge dataEdge = resolveEdgeById(sceneFlow, edgeId);
                        if (dataEdge == null) {
                            return new JSONObject().put("error", "EDGE_NOT_FOUND");
                        }
                        // Apply normalize or straighten
                        if (isNormalize) {
                            normalizeEdge(dataEdge, project.getEditorConfig());
                        } else {
                            straightenEdge(dataEdge);
                        }
                        JSONObject payloadResp = new JSONObject();
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), "Failed to update edge");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    broadcastDirtyIfPresent(response, projectId);
                    return;
                }
                // Phase 6: Headless version - Edge path operations for multiple edges
                case "SceneFlow.Edge.NormalizeGroup":
                case "SceneFlow.Edge.StraightenGroup": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    JSONArray edgeIds = body.optJSONArray("edgeIds");
                    if (projectId == null || projectId.isBlank() || edgeIds == null || edgeIds.length() == 0) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId or edgeIds");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    final boolean isNormalize = "SceneFlow.Edge.NormalizeGroup".equals(name);
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        EditorConfig config = project.getEditorConfig();
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);

                        // Process all edges
                        for (int i = 0; i < edgeIds.length(); i++) {
                            String edgeId = edgeIds.optString(i, "").trim();
                            if (!edgeId.isEmpty()) {
                                AbstractEdge dataEdge = resolveEdgeById(sceneFlow, edgeId);
                                if (dataEdge == null) {
                                    return new JSONObject().put("error", "EDGE_NOT_FOUND");
                                }
                                // Apply normalize or straighten
                                if (isNormalize) {
                                    normalizeEdge(dataEdge, config);
                                } else {
                                    straightenEdge(dataEdge);
                                }
                            }
                        }

                        JSONObject payloadResp = new JSONObject();
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), "Failed to update edges");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    broadcastDirtyIfPresent(response, projectId);
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Edge.Retarget
                case "SceneFlow.Edge.Retarget": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String edgeId = body.optString("edgeId", null);
                    String targetId = body.optString("targetId", null);
                    if (projectId == null || projectId.isBlank()
                            || edgeId == null || edgeId.isBlank()
                            || targetId == null || targetId.isBlank()) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId, edgeId, or targetId");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();

                        // Find the edge
                        AbstractEdge dataEdge = resolveEdgeById(sceneFlow, edgeId);
                        if (dataEdge == null) {
                            return new JSONObject().put("error", "EDGE_NOT_FOUND");
                        }

                        // Find source, old target, and new target nodes
                        BasicNode sourceNode = dataEdge.getSourceNode();
                        BasicNode oldTargetNode = dataEdge.getTargetNode();
                        BasicNode newTargetNode = resolveNodeById(sceneFlow, targetId);

                        if (sourceNode == null || newTargetNode == null) {
                            return new JSONObject().put("error", "NODE_NOT_FOUND");
                        }

                        // Phase 6: Get node dimensions for dock point handling
                        EditorConfig config = project.getEditorConfig();
                        int nodeWidth = config != null ? config.sNODEWIDTH : 90;
                        int nodeHeight = config != null ? config.sNODEHEIGHT : 90;

                        // Phase 6: Release old target's dock point
                        if (oldTargetNode != null) {
                            EdgeGraphics edgeGraphics = dataEdge.getGraphics();
                            if (edgeGraphics != null && edgeGraphics.getConnection() != null) {
                                List<EdgePoint> points = edgeGraphics.getConnection().getPointList();
                                if (points != null && points.size() >= 2) {
                                    EdgePoint endPt = points.get(points.size() - 1);
                                    NodeGraphics oldTgtGraphics = oldTargetNode.getGraphics();
                                    NodePosition oldTgtPos = oldTgtGraphics != null ? oldTgtGraphics.getPosition() : null;
                                    double oldTgtX = oldTgtPos != null ? oldTgtPos.getXPos() : 0;
                                    double oldTgtY = oldTgtPos != null ? oldTgtPos.getYPos() : 0;
                                    int oldDockIdx = findDockPointIndex(oldTgtX, oldTgtY, nodeWidth, nodeHeight,
                                            oldTargetNode instanceof SuperNode, endPt.getXPos(), endPt.getYPos());
                                    if (oldDockIdx >= 0) {
                                        releaseDockPoint(oldTargetNode.getId(), oldDockIdx, false);
                                    }
                                }
                            }
                        }

                        // Phase 6: Find and occupy new dock point on new target, update edge endpoint
                        NodeGraphics newTgtGraphics = newTargetNode.getGraphics();
                        NodePosition newTgtPos = newTgtGraphics != null ? newTgtGraphics.getPosition() : null;
                        double newTgtX = newTgtPos != null ? newTgtPos.getXPos() : 0;
                        double newTgtY = newTgtPos != null ? newTgtPos.getYPos() : 0;
                        boolean newTgtIsSuperNode = newTargetNode instanceof SuperNode;

                        // Get source position for dock point selection
                        NodeGraphics srcGraphics = sourceNode.getGraphics();
                        NodePosition srcPos = srcGraphics != null ? srcGraphics.getPosition() : null;
                        double srcX = srcPos != null ? srcPos.getXPos() : 0;
                        double srcY = srcPos != null ? srcPos.getYPos() : 0;
                        boolean srcIsSuperNode = sourceNode instanceof SuperNode;

                        // Find best dock point pair for new configuration
                        int[] dockPair;
                        boolean isSelfLoop = sourceNode.getId().equals(newTargetNode.getId());
                        if (isSelfLoop) {
                            dockPair = findSelfLoopDockPointPair(sourceNode.getId(), nodeWidth, nodeHeight, srcIsSuperNode);
                        } else {
                            dockPair = findBestDockPointPair(
                                sourceNode.getId(), srcX, srcY, nodeWidth, nodeHeight, srcIsSuperNode,
                                newTargetNode.getId(), newTgtX, newTgtY, nodeWidth, nodeHeight, newTgtIsSuperNode
                            );
                        }
                        int newTgtDockIdx = dockPair[1];
                        occupyDockPoint(newTargetNode.getId(), newTgtDockIdx, false);

                        // Update edge endpoint to new dock position
                        double[] newTgtDock = getDockPointPosition(newTgtX, newTgtY, nodeWidth, nodeHeight, newTgtIsSuperNode, newTgtDockIdx);
                        EdgeGraphics edgeGraphics = dataEdge.getGraphics();
                        if (edgeGraphics != null && edgeGraphics.getConnection() != null) {
                            List<EdgePoint> points = edgeGraphics.getConnection().getPointList();
                            if (points != null && points.size() >= 2) {
                                EdgePoint endPt = points.get(points.size() - 1);
                                EdgePoint startPt = points.get(0);
                                // Update end position
                                endPt.setXPos((int) Math.round(newTgtDock[0]));
                                endPt.setYPos((int) Math.round(newTgtDock[1]));
                                // Update control point
                                double[] tgtCtrl = computeInitialControlPoint(
                                    startPt.getXPos(), startPt.getYPos(),
                                    newTgtDock[0], newTgtDock[1], false
                                );
                                endPt.setCtrlXPos((int) Math.round(tgtCtrl[0]));
                                endPt.setCtrlYPos((int) Math.round(tgtCtrl[1]));
                            }
                        }

                        // Retarget the edge (headless - no WorkSpacePanel)
                        // Step 1: Remove edge from source node's edge list
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

                        // Step 2: Update target references
                        dataEdge.setTargetNode(newTargetNode);
                        dataEdge.setTargetUnid(newTargetNode.getId());

                        // Step 3: Add edge back to source node's edge list with new target
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

                        JSONObject payloadResp = new JSONObject();
                        payloadResp.put("edgeId", edgeId);
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), "Failed to retarget edge");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    broadcastDirtyIfPresent(response, projectId);
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Edge.PEdge.UpdateGroup
                case "SceneFlow.Edge.PEdge.UpdateGroup": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String sourceId = body.optString("sourceId", null);
                    JSONArray updates = body.optJSONArray("updates");
                    if (projectId == null || projectId.isBlank()
                            || sourceId == null || sourceId.isBlank()
                            || updates == null) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId, sourceId, or updates");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        BasicNode sourceNode = resolveNodeById(sceneFlow, sourceId);
                        if (sourceNode == null) {
                            return new JSONObject().put("error", "NODE_NOT_FOUND");
                        }
                        List<RandomEdge> edges = sourceNode.getPEdgeList();
                        if (edges.isEmpty()) {
                            return new JSONObject().put("error", "EDGE_NOT_FOUND");
                        }
                        Map<RandomEdge, Integer> updateMap = new LinkedHashMap<>();
                        for (int i = 0; i < updates.length(); i++) {
                            JSONObject entry = updates.optJSONObject(i);
                            if (entry == null) {
                                return new JSONObject().put("error", "INVALID_PAYLOAD").put("message", "Invalid edge update.");
                            }
                            String edgeId = entry.optString("edgeId", "");
                            String targetId = entry.optString("targetId", "");
                            RandomEdge edge = resolvePEdgeForSource(sceneFlow, sourceNode, edgeId, targetId);
                            if (edge == null) {
                                return new JSONObject().put("error", "EDGE_NOT_FOUND");
                            }
                            if (updateMap.containsKey(edge)) {
                                return new JSONObject().put("error", "DUPLICATE_EDGE").put("message", "Duplicate edge entry.");
                            }
                            Object raw = entry.opt("probability");
                            int probability;
                            try {
                                probability = Integer.parseInt(String.valueOf(raw));
                            } catch (NumberFormatException ex) {
                                return new JSONObject().put("error", "INVALID_PROBABILITY").put("message", "Probability must be a number.");
                            }
                            if (probability < 0 || probability > 100) {
                                return new JSONObject().put("error", "INVALID_PROBABILITY").put("message", "Probability must be between 0 and 100.");
                            }
                            updateMap.put(edge, probability);
                        }
                        if (updateMap.size() != edges.size()) {
                            return new JSONObject().put("error", "EDGE_COUNT_MISMATCH").put("message", "Provide probabilities for all P-edges.");
                        }
                        int sum = 0;
                        for (int probability : updateMap.values()) {
                            sum += probability;
                        }
                        if (sum != 100) {
                            return new JSONObject().put("error", "PROBABILITY_SUM_INVALID").put("message", "Total probability must be 100%.");
                        }

                        // Apply probabilities (headless - no WorkSpacePanel)
                        for (Map.Entry<RandomEdge, Integer> entry : updateMap.entrySet()) {
                            RandomEdge edge = entry.getKey();
                            if (edge != null) {
                                edge.setProbability(entry.getValue());
                            }
                        }

                        JSONObject payloadResp = new JSONObject();
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), response.optString("message", "Probability update failed"));
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    broadcastDirtyIfPresent(response, projectId);
                    return;
                }
                // Phase 6: Headless version of SceneFlow.Edge.Delete
                case "SceneFlow.Edge.Delete": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    String edgeId = body.optString("edgeId", null);
                    String sourceId = body.optString("sourceId", null);
                    String targetId = body.optString("targetId", null);
                    boolean hasEdgeId = edgeId != null && !edgeId.isBlank();
                    if (projectId == null || projectId.isBlank()
                            || (!hasEdgeId && (sourceId == null || sourceId.isBlank() || targetId == null || targetId.isBlank()))) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId or edge identifiers");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();

                        // Find the edge to delete
                        AbstractEdge dataEdge = hasEdgeId
                                ? resolveEdgeById(sceneFlow, edgeId)
                                : resolveEdgeByNodes(sceneFlow, sourceId, targetId);
                        if (dataEdge == null) {
                            return new JSONObject().put("error", "EDGE_NOT_FOUND");
                        }

                        // Get source node to remove edge from
                        BasicNode sourceNode = dataEdge.getSourceNode();
                        if (sourceNode == null) {
                            return new JSONObject().put("error", "EDGE_NOT_FOUND");
                        }

                        // Phase 6: Release dock points before removing edge
                        BasicNode targetNode = dataEdge.getTargetNode();
                        EdgeGraphics edgeGraphics = dataEdge.getGraphics();
                        if (edgeGraphics != null && edgeGraphics.getConnection() != null) {
                            List<EdgePoint> edgePoints = edgeGraphics.getConnection().getPointList();
                            if (edgePoints != null && edgePoints.size() >= 2) {
                                EdgePoint startPt = edgePoints.get(0);
                                EdgePoint endPt = edgePoints.get(edgePoints.size() - 1);

                                EditorConfig config = project.getEditorConfig();
                                int nodeWidth = config != null ? config.sNODEWIDTH : 90;
                                int nodeHeight = config != null ? config.sNODEHEIGHT : 90;

                                // Find and release source dock point (isSource=true for outgoing)
                                if (sourceNode != null) {
                                    NodeGraphics srcGraphics = sourceNode.getGraphics();
                                    NodePosition srcPos = srcGraphics != null ? srcGraphics.getPosition() : null;
                                    double srcX = srcPos != null ? srcPos.getXPos() : 0;
                                    double srcY = srcPos != null ? srcPos.getYPos() : 0;
                                    int srcDockIdx = findDockPointIndex(srcX, srcY, nodeWidth, nodeHeight,
                                            sourceNode instanceof SuperNode, startPt.getXPos(), startPt.getYPos());
                                    if (srcDockIdx >= 0) {
                                        releaseDockPoint(sourceNode.getId(), srcDockIdx, true);
                                    }
                                }

                                // Find and release target dock point (isSource=false for incoming)
                                if (targetNode != null) {
                                    NodeGraphics tgtGraphics = targetNode.getGraphics();
                                    NodePosition tgtPos = tgtGraphics != null ? tgtGraphics.getPosition() : null;
                                    double tgtX = tgtPos != null ? tgtPos.getXPos() : 0;
                                    double tgtY = tgtPos != null ? tgtPos.getYPos() : 0;
                                    int tgtDockIdx = findDockPointIndex(tgtX, tgtY, nodeWidth, nodeHeight,
                                            targetNode instanceof SuperNode, endPt.getXPos(), endPt.getYPos());
                                    if (tgtDockIdx >= 0) {
                                        releaseDockPoint(targetNode.getId(), tgtDockIdx, false);
                                    }
                                }
                            }
                        }

                        // Remove edge based on type (these methods return void)
                        if (dataEdge instanceof GuargedEdge) {
                            sourceNode.removeCEdge((GuargedEdge) dataEdge);
                        } else if (dataEdge instanceof InterruptEdge) {
                            sourceNode.removeIEdge((InterruptEdge) dataEdge);
                        } else if (dataEdge instanceof RandomEdge) {
                            sourceNode.removePEdge((RandomEdge) dataEdge);
                        } else if (dataEdge instanceof ForkingEdge) {
                            sourceNode.removeFEdge((ForkingEdge) dataEdge);
                        } else if (dataEdge instanceof TimeoutEdge || dataEdge instanceof EpsilonEdge) {
                            // TimeoutEdge and EpsilonEdge use the D-edge
                            sourceNode.removeDEdge();
                        }

                        JSONObject payloadResp = new JSONObject();
                        payloadResp.put("edgeId", edgeId == null ? "" : edgeId);
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        String code = response.getString("error");
                        sendError(ctx, requestId, code, "Failed to delete edge");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    return;
                }
                // Phase 6: Headless version - Edge path operations for all edges in current view
                case "SceneFlow.Edge.NormalizeAll":
                case "SceneFlow.Edge.StraightenAll": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    if (projectId == null || projectId.isBlank()) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    final boolean isNormalize = "SceneFlow.Edge.NormalizeAll".equals(name);
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        EditorConfig config = project.getEditorConfig();
                        SuperNode snapshotTarget = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);

                        // Process all edges in the current view (snapshotTarget)
                        for (BasicNode node : snapshotTarget.getNodeAndSuperNodeList()) {
                            for (AbstractEdge edge : node.getEdgeList()) {
                                if (isNormalize) {
                                    normalizeEdge(edge, config);
                                } else {
                                    straightenEdge(edge);
                                }
                            }
                        }

                        JSONObject payloadResp = new JSONObject();
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, snapshotTarget));
                        payloadResp.put("dirty", project.hasChanged());
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    broadcastDirtyIfPresent(response, projectId);
                    return;
                }
                case "SceneFlow.Undo":
                // Phase 6: Headless version of SceneFlow.Redo (no-op - undo system not available in headless mode)
                case "SceneFlow.Redo": {
                    String projectId = body.optString("projectId", null);
                    String superNodeId = body.optString("superNodeId", null);
                    if (projectId == null || projectId.isBlank()) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId");
                        return;
                    }
                    final String resolvedSuperNodeId = superNodeId;
                    JSONObject response = callOnEdt(() -> {
                        EditorProject project = mEditorProjectService.getProject(projectId);
                        if (project == null) {
                            return null;
                        }
                        SceneFlow sceneFlow = project.getSceneFlow();
                        SuperNode targetSuperNode = resolveSnapshotTarget(sceneFlow, resolvedSuperNodeId);
                        // Undo/Redo not implemented in headless mode - return current snapshot
                        JSONObject payloadResp = new JSONObject();
                        payloadResp.put("snapshot", sceneFlowSnapshot(project, projectId, targetSuperNode));
                        payloadResp.put("message", "Undo/Redo not available in headless mode");
                        return payloadResp;
                    });
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    return;
                }
                case "Runtime.Play":
                case "Runtime.Pause":
                case "Runtime.Stop": {
                    String projectId = body.optString("projectId", null);
                    if (projectId == null || projectId.isBlank()) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId");
                        return;
                    }
                    final String runtimeCommand = name;
                    mLastRuntimeProjectId = projectId;
                    boolean ok = callOnEdt(() -> runRuntimeCommand(projectId, runtimeCommand));
                    if (!ok) {
                        sendError(ctx, requestId, "RUNTIME_FAILED", "Failed to change runtime state");
                        return;
                    }
                    JSONObject payloadResp = new JSONObject().put("projectId", projectId);
                    sendResponse(ctx, requestId, name, payloadResp);
                    emitUiRuntimeState(projectId);
                    return;
                }
                case "Runtime.Variable.Set": {
                    String projectId = body.optString("projectId", null);
                    String varName = body.optString("name", null);
                    String valueExpr = body.optString("value", null);
                    if (valueExpr == null || valueExpr.isBlank()) {
                        valueExpr = body.optString("valueExpr", null);
                    }
                    final String valueExprResolved = valueExpr;
                    if (projectId == null || projectId.isBlank()
                            || varName == null || varName.isBlank()
                            || valueExprResolved == null || valueExprResolved.isBlank()) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId, name, or value");
                        return;
                    }
                    JSONObject response = callOnEdt(() -> applyRuntimeVariableUpdate(projectId, varName, valueExprResolved));
                    if (response == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"),
                                response.optString("message", "Failed to update variable"));
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    return;
                }
                // Phase 6: Headless version of Runtime.Query
                case "Runtime.Query": {
                    String projectId = body.optString("projectId", null);
                    String query = body.optString("query", null);
                    if (projectId == null || projectId.isBlank() || query == null || query.isBlank()) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId or query");
                        return;
                    }
                    // Phase 6: Use EditorProjectService to verify project exists
                    EditorProject project = mEditorProjectService.getProject(projectId);
                    if (project == null) {
                        sendError(ctx, requestId, "PROJECT_NOT_FOUND", "Project not found");
                        return;
                    }
                    JSONObject response = applyRuntimeQuery(query);
                    sendResponse(ctx, requestId, name, response);
                    return;
                }
                case "Preferences.Update": {
                    JSONObject values = body.optJSONObject("values");
                    if (values == null) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing values");
                        return;
                    }
                    JSONObject response = callOnEdt(() -> applyPreferencesUpdate(values));
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), response.optString("message", "Failed to update preferences"));
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    emitUiPreferences(response);
                    return;
                }
                case "Config.Update": {
                    String projectId = body.optString("projectId", null);
                    JSONObject values = body.optJSONObject("values");
                    if (projectId == null || projectId.isBlank() || values == null) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId or values");
                        return;
                    }
                    JSONObject response = callOnEdt(() -> applyConfigUpdate(projectId, values));
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), response.optString("message", "Failed to update config"));
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    emitUiProjectConfig(projectId);
                    return;
                }
                case "ProjectConfig.Update": {
                    String projectId = body.optString("projectId", null);
                    JSONObject configPayload = body.optJSONObject("config");
                    if (projectId == null || projectId.isBlank() || configPayload == null) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId or config");
                        return;
                    }
                    JSONObject response = callOnEdt(() -> applyProjectConfigUpdate(projectId, configPayload));
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), response.optString("message", "Failed to update project config"));
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    emitUiProjectConfig(projectId);
                    return;
                }
                case "Script.Update": {
                    String projectId = body.optString("projectId", null);
                    String text = body.has("text") ? body.optString("text", null) : null;
                    Integer version = body.has("version") ? body.optInt("version") : null;
                    boolean force = body.optBoolean("force", false);
                    if (projectId == null || projectId.isBlank() || text == null) {
                        sendError(ctx, requestId, "BAD_REQUEST", "Missing projectId or text");
                        return;
                    }
                    JSONObject response = callOnEdt(() -> applyScriptUpdate(projectId, text, version, force));
                    if (response.has("error")) {
                        sendError(ctx, requestId, response.getString("error"), response.optString("message", "Failed to update script"));
                        return;
                    }
                    sendResponse(ctx, requestId, name, response);
                    if (response.optBoolean("applied")) {
                        emitUiScriptSnapshot(projectId);
                        if (response.has("dirty")) {
                            boolean dirty = response.getBoolean("dirty");
                            emitUiProjectDirty(projectId, dirty, List.of("script"));
                        }
                    }
                    return;
                }
                default:
                    sendError(ctx, requestId, "NOT_IMPLEMENTED", "Command not implemented");
            }
        } catch (Exception e) {
            String errorName = "SERVER_ERROR";
            String errorMessage = "WebSocket command failed.";
            if (name != null && !name.isBlank()) {
                errorMessage = "WebSocket command failed: " + name;
            }
            mLogger.failure("WebSocket message error: " + e.getMessage());
            sendError(ctx, requestId, errorName, errorMessage);
        }
    }

    // Phase 6: Headless version of runRuntimeCommand
    private boolean runRuntimeCommand(String projectId, String command) {
        EditorProject project = mEditorProjectService.getProject(projectId);
        if (project == null) {
            return false;
        }
        switch (command) {
            case "Runtime.Play":
                if (project.isRunning()) {
                    if (project.isPaused()) {
                        return project.proceed();
                    }
                    return true;
                }
                if (project.launch()) {
                    return project.start();
                }
                return false;
            case "Runtime.Pause":
                if (project.isRunning() && !project.isPaused()) {
                    return project.pause();
                }
                return true;
            case "Runtime.Stop":
                if (project.isRunning()) {
                    if (project.abort()) {
                        project.unload();
                        return true;
                    }
                    return false;
                }
                return true;
            default:
                return false;
        }
    }

    private JSONObject projectToJson(ProjectEditor editor, JTabbedPane tabs, int index) {
        EditorProject project = editor.getEditorProject();
        String name = project.getProjectName();
        if (name == null || name.isBlank()) {
            if (index >= 0) {
                name = tabs.getTitleAt(index);
            }
        }
        if (name == null) {
            name = "";
        }
        String path = project.getProjectPath();
        if (path != null && path.isBlank()) {
            path = null;
        }
        boolean systemPath = isSystemDirectory(path);
        boolean sampleProject = isUnderDirectory(path, PreferencesDesktop.sSAMPLE_PROJECTS);
        boolean tutorialProject = isUnderDirectory(path, PreferencesDesktop.sTUTORIALS_PROJECTS);
        boolean jarPath = isJarPath(path);
        boolean saveAsOnly = project.isPending() || !systemPath || jarPath || sampleProject || tutorialProject;
        String runtimeState = project.isRunning()
                ? (project.isPaused() ? "paused" : "running")
                : "stopped";
        String activeSuperNodeId = editor.getSceneFlowEditor().getSceneFlowManager().getCurrentActiveSuperNode().getId();

        JSONObject config = new JSONObject();
        EditorConfig editorConfig = project.getEditorConfig();
        config.put("node_width", editorConfig.getProperty("node_width"));
        config.put("node_height", editorConfig.getProperty("node_height"));

        JSONObject json = new JSONObject();
        json.put("projectId", projectIdFor(editor));
        json.put("name", name);
        json.put("path", path);
        json.put("dirty", project.hasChanged());
        json.put("runtimeState", runtimeState);
        json.put("activeSuperNodeId", activeSuperNodeId);
        json.put("pending", project.isPending());
        json.put("saveAsOnly", saveAsOnly);
        json.put("config", config);
        return json;
    }

    // Phase 6: Headless version of projectToJson
    private JSONObject projectToJson(EditorProject project, String projectId) {
        String name = project.getProjectName();
        if (name == null || name.isBlank()) {
            name = "";
        }
        String path = project.getProjectPath();
        if (path != null && path.isBlank()) {
            path = null;
        }
        boolean systemPath = isSystemDirectory(path);
        boolean sampleProject = isUnderDirectory(path, PreferencesDesktop.sSAMPLE_PROJECTS);
        boolean tutorialProject = isUnderDirectory(path, PreferencesDesktop.sTUTORIALS_PROJECTS);
        boolean jarPath = isJarPath(path);
        boolean saveAsOnly = project.isPending() || !systemPath || jarPath || sampleProject || tutorialProject;
        String runtimeState = project.isRunning()
                ? (project.isPaused() ? "paused" : "running")
                : "stopped";

        // Default to root sceneflow ID for active supernode
        String activeSuperNodeId = project.getSceneFlow().getId();
        if (activeSuperNodeId == null || activeSuperNodeId.isBlank()) {
            activeSuperNodeId = ROOT_SUPERNODE_ID;
        }

        JSONObject config = new JSONObject();
        EditorConfig editorConfig = project.getEditorConfig();
        config.put("node_width", editorConfig.getProperty("node_width"));
        config.put("node_height", editorConfig.getProperty("node_height"));

        JSONObject json = new JSONObject();
        json.put("projectId", projectId);
        json.put("name", name);
        json.put("path", path);
        json.put("dirty", project.hasChanged());
        json.put("runtimeState", runtimeState);
        json.put("activeSuperNodeId", activeSuperNodeId);
        json.put("pending", project.isPending());
        json.put("saveAsOnly", saveAsOnly);
        json.put("config", config);
        return json;
    }

    private boolean removeRecentProject(String targetPath) {
        if (targetPath == null || targetPath.isBlank()) {
            return false;
        }
        List<String> paths = new ArrayList<>();
        List<String> names = new ArrayList<>();
        List<String> dates = new ArrayList<>();
        boolean removed = false;
        for (int i = 0; i <= PreferencesDesktop.sMAX_RECENT_PROJECTS; i++) {
            String path = PreferencesDesktop.getProperty("recentproject." + i + ".path");
            String name = PreferencesDesktop.getProperty("recentproject." + i + ".name");
            String date = PreferencesDesktop.getProperty("recentproject." + i + ".date");
            if (path == null || name == null) {
                continue;
            }
            if (path.equals(targetPath)) {
                removed = true;
                continue;
            }
            paths.add(path);
            names.add(name);
            dates.add(date);
        }
        for (int i = 0; i <= PreferencesDesktop.sMAX_RECENT_PROJECTS; i++) {
            PreferencesDesktop.removeProperty("recentproject." + i + ".path");
            PreferencesDesktop.removeProperty("recentproject." + i + ".name");
            PreferencesDesktop.removeProperty("recentproject." + i + ".date");
        }
        int count = Math.min(paths.size(), PreferencesDesktop.sMAX_RECENT_PROJECTS + 1);
        for (int i = 0; i < count; i++) {
            PreferencesDesktop.setProperty("recentproject." + i + ".path", paths.get(i));
            PreferencesDesktop.setProperty("recentproject." + i + ".name", names.get(i));
            String date = dates.get(i);
            if (date != null) {
                PreferencesDesktop.setProperty("recentproject." + i + ".date", date);
            }
        }
        PreferencesDesktop.save();
        // Phase 6: Removed EditorInstance.getInstance().clearRecentProjects() - no Swing UI to refresh
        return removed;
    }

    private boolean isSystemDirectory(String path) {
        if (path == null || path.isBlank() || isJarPath(path)) {
            return false;
        }
        File dir = new File(path);
        return dir.exists() && dir.isDirectory();
    }

    private boolean isJarPath(String path) {
        if (path == null || path.isBlank()) {
            return false;
        }
        String lower = path.toLowerCase(Locale.ROOT);
        return lower.startsWith("jar:")
                || lower.contains(".jar!")
                || lower.contains(".jar/")
                || lower.contains(".jar\\")
                || lower.endsWith(".jar");
    }

    private boolean isUnderDirectory(String path, String basePath) {
        if (path == null || path.isBlank() || basePath == null || basePath.isBlank()) {
            return false;
        }
        File base = new File(basePath).getAbsoluteFile();
        File target = new File(path).getAbsoluteFile();
        String basePathAbs = base.getAbsolutePath();
        String targetPathAbs = target.getAbsolutePath();
        if (targetPathAbs.equals(basePathAbs)) {
            return true;
        }
        return targetPathAbs.startsWith(basePathAbs + File.separator);
    }

    private JSONObject sceneFlowSnapshot(ProjectEditor editor, SuperNode superNode) {
        EditorProject project = editor.getEditorProject();
        SceneFlowManager manager = editor.getSceneFlowEditor().getSceneFlowManager();
        EditorConfig config = project.getEditorConfig();

        JSONObject snapshot = new JSONObject();
        snapshot.put("projectId", projectIdFor(editor));
        snapshot.put("superNodeId", superNode.getId());
        snapshot.put("revision", superNode.getHashCode());
        JSONObject superNodeJson = new JSONObject();
        superNodeJson.put("id", superNode.getId());
        superNodeJson.put("name", superNode.getName());
        superNodeJson.put("flavour", superNode.getFlavour().name());
        snapshot.put("superNode", superNodeJson);

        JSONArray path = new JSONArray();
        JSONArray pathNodesJson = new JSONArray();
        List<SuperNode> pathNodes = findPathToSuperNode(manager.getSceneFlow(), superNode.getId());
        if (superNode.getParentNode() == null && (pathNodes == null || pathNodes.isEmpty())) {
            pathNodes = new ArrayList<>();
            pathNodes.add(superNode);
        }
        if (pathNodes != null) {
            for (SuperNode node : pathNodes) {
                String nodeName = node.getName();
                if (nodeName == null || nodeName.isBlank()) {
                    nodeName = "SceneFlow";
                }
                String nodeId = node.getId();
                if (nodeId == null || nodeId.isBlank()) {
                    nodeId = ROOT_SUPERNODE_ID;
                }
                path.put(nodeName);
                JSONObject pathEntry = new JSONObject();
                pathEntry.put("id", nodeId);
                pathEntry.put("name", nodeName);
                pathEntry.put("isRoot", node.getParentNode() == null);
                pathNodesJson.put(pathEntry);
            }
        }
        snapshot.put("path", path);
        snapshot.put("pathNodes", pathNodesJson);

        Set<String> altStartIds = collectAltStartIds(manager, superNode);
        JSONObject superNodeData = nodeToJson(superNode, superNode, altStartIds, config);
        SuperNode parent = superNode.getParentNode();
        boolean isRoot = parent == null;
        boolean isStart = isRoot || parent.getStartNodeMap().containsKey(superNode.getId());
        superNodeData.put("isStart", isStart);
        superNodeData.put("isRoot", isRoot);
        snapshot.put("superNodeData", superNodeData);
        JSONArray nodes = new JSONArray();
        for (BasicNode node : superNode.getNodeAndSuperNodeList()) {
            nodes.put(nodeToJson(node, superNode, altStartIds, config));
        }
        snapshot.put("nodes", nodes);

        JSONArray edges = new JSONArray();
        int edgeIndex = 0;
        for (BasicNode node : superNode.getNodeAndSuperNodeList()) {
            for (AbstractEdge edge : node.getEdgeList()) {
                edges.put(edgeToJson(edge, edgeIndex++));
            }
        }
        snapshot.put("edges", edges);

        JSONArray comments = new JSONArray();
        int commentIndex = 0;
        for (CommentBadge comment : superNode.getCommentList()) {
            comments.put(commentToJson(comment, commentIndex++));
        }
        snapshot.put("comments", comments);
        return snapshot;
    }

    // Phase 6: Headless version of sceneFlowSnapshot
    private JSONObject sceneFlowSnapshot(EditorProject project, String projectId, SuperNode superNode) {
        EditorConfig config = project.getEditorConfig();
        SceneFlow sceneFlow = project.getSceneFlow();

        JSONObject snapshot = new JSONObject();
        snapshot.put("projectId", projectId);
        snapshot.put("superNodeId", superNode.getId());
        snapshot.put("revision", superNode.getHashCode());
        JSONObject superNodeJson = new JSONObject();
        superNodeJson.put("id", superNode.getId());
        superNodeJson.put("name", superNode.getName());
        superNodeJson.put("flavour", superNode.getFlavour().name());
        snapshot.put("superNode", superNodeJson);

        JSONArray path = new JSONArray();
        JSONArray pathNodesJson = new JSONArray();
        List<SuperNode> pathNodes = findPathToSuperNode(sceneFlow, superNode.getId());
        if (superNode.getParentNode() == null && (pathNodes == null || pathNodes.isEmpty())) {
            pathNodes = new ArrayList<>();
            pathNodes.add(superNode);
        }
        if (pathNodes != null) {
            for (SuperNode node : pathNodes) {
                String nodeName = node.getName();
                if (nodeName == null || nodeName.isBlank()) {
                    nodeName = "SceneFlow";
                }
                String nodeId = node.getId();
                if (nodeId == null || nodeId.isBlank()) {
                    nodeId = ROOT_SUPERNODE_ID;
                }
                path.put(nodeName);
                JSONObject pathEntry = new JSONObject();
                pathEntry.put("id", nodeId);
                pathEntry.put("name", nodeName);
                pathEntry.put("isRoot", node.getParentNode() == null);
                pathNodesJson.put(pathEntry);
            }
        }
        snapshot.put("path", path);
        snapshot.put("pathNodes", pathNodesJson);

        Set<String> altStartIds = collectAltStartIds(superNode);
        JSONObject superNodeData = nodeToJson(superNode, superNode, altStartIds, config);
        SuperNode parent = superNode.getParentNode();
        boolean isRoot = parent == null;
        boolean isStart = isRoot || parent.getStartNodeMap().containsKey(superNode.getId());
        superNodeData.put("isStart", isStart);
        superNodeData.put("isRoot", isRoot);
        snapshot.put("superNodeData", superNodeData);
        JSONArray nodes = new JSONArray();
        for (BasicNode node : superNode.getNodeAndSuperNodeList()) {
            nodes.put(nodeToJson(node, superNode, altStartIds, config));
        }
        snapshot.put("nodes", nodes);

        JSONArray edges = new JSONArray();
        int edgeIndex = 0;
        for (BasicNode node : superNode.getNodeAndSuperNodeList()) {
            for (AbstractEdge edge : node.getEdgeList()) {
                edges.put(edgeToJson(edge, edgeIndex++));
            }
        }
        snapshot.put("edges", edges);

        JSONArray comments = new JSONArray();
        int commentIndex = 0;
        for (CommentBadge comment : superNode.getCommentList()) {
            comments.put(commentToJson(comment, commentIndex++));
        }
        snapshot.put("comments", comments);
        return snapshot;
    }

    // Phase 6: Headless version of collectAltStartIds
    private Set<String> collectAltStartIds(SuperNode target) {
        Set<String> altStartIds = new LinkedHashSet<>();
        SuperNode parent = target.getParentNode();
        if (parent == null) {
            return altStartIds;
        }
        for (BasicNode node : parent.getNodeAndSuperNodeList()) {
            for (AbstractEdge edge : node.getEdgeList()) {
                if (!target.getId().equals(edge.getTargetUnid())) {
                    continue;
                }
                Map<Tuple<String, BasicNode>, Tuple<String, BasicNode>> altMap = edge.getAltMap();
                if (altMap == null) {
                    continue;
                }
                for (Tuple<String, BasicNode> alt : altMap.values()) {
                    if (alt != null && alt.getFirst() != null && !alt.getFirst().isEmpty()) {
                        altStartIds.add(alt.getFirst());
                    }
                }
            }
        }
        return altStartIds;
    }

    private JSONObject nodeToJson(BasicNode node, SuperNode superNode, Set<String> altStartIds, EditorConfig config) {
        JSONObject json = new JSONObject();
        json.put("id", node.getId());
        json.put("type", (node instanceof SuperNode) ? "Super" : "Basic");
        json.put("name", node.getName());
        json.put("comment", node.getComment() == null ? "" : node.getComment());
        json.put("flavour", node.getFlavour().name());
        json.put("isStart", superNode.getStartNodeMap().containsKey(node.getId()));
        json.put("isAltStart", altStartIds.contains(node.getId()));
        json.put("isHistory", node.isHistoryNode());
        int childCount = 0;
        if (node instanceof SuperNode) {
            childCount = ((SuperNode) node).getNodeAndSuperNodeList().size();
        }
        json.put("childCount", childCount);

        JSONObject graphics = new JSONObject();
        int x = 0;
        int y = 0;
        NodeGraphics nodeGraphics = node.getGraphics();
        if (nodeGraphics != null) {
            NodePosition position = nodeGraphics.getPosition();
            if (position != null) {
                x = position.getXPos();
                y = position.getYPos();
            }
        }
        graphics.put("x", x);
        graphics.put("y", y);
        json.put("graphics", graphics);

        JSONObject size = new JSONObject();
        if (node instanceof SuperNode) {
            size.put("w", config.sSUPERNODEWIDTH);
            size.put("h", config.sSUPERNODEHEIGHT);
        } else {
            size.put("w", config.sNODEWIDTH);
            size.put("h", config.sNODEHEIGHT);
        }
        json.put("size", size);

        json.put("typeDefs", typeDefsToJson(node.getTypeDefList()));
        json.put("varDefs", varDefsToJson(node.getVarDefList()));
        json.put("commands", commandsToJson(node.getCmdList()));
        JSONArray typeCatalog = typeCatalogToJson(node);
        json.put("typeCatalog", typeCatalog);
        json.put("typeOptions", typeOptionsToJson(typeCatalog));

        return json;
    }

    private JSONArray typeDefsToJson(List<DataTypeDefinition> defs) {
        JSONArray list = new JSONArray();
        if (defs == null) {
            return list;
        }
        for (DataTypeDefinition def : defs) {
            if (def != null) {
                list.put(typeDefToJson(def));
            }
        }
        return list;
    }

    private JSONObject typeDefToJson(DataTypeDefinition def) {
        JSONObject json = new JSONObject();
        json.put("name", def.getName());
        json.put("flavour", def.getFlavour().name());
        json.put("syntax", def.getConcreteSyntax());
        if (def instanceof ListTypeDefinition) {
            json.put("elementType", ((ListTypeDefinition) def).getType());
        }
        if (def instanceof StructTypeDefinition) {
            JSONArray members = new JSONArray();
            for (MemberDefinition member : ((StructTypeDefinition) def).getMemberList()) {
                if (member == null) {
                    continue;
                }
                JSONObject entry = new JSONObject();
                entry.put("name", member.getName());
                entry.put("type", member.getType());
                members.put(entry);
            }
            json.put("members", members);
        }
        return json;
    }

    private JSONArray varDefsToJson(List<VariableDefinition> defs) {
        JSONArray list = new JSONArray();
        if (defs == null) {
            return list;
        }
        for (VariableDefinition def : defs) {
            if (def != null) {
                list.put(varDefToJson(def));
            }
        }
        return list;
    }

    private JSONObject varDefToJson(VariableDefinition def) {
        JSONObject json = new JSONObject();
        json.put("name", def.getName());
        json.put("type", def.getType());
        Expression exp = def.getExp();
        json.put("expression", exp != null ? exp.getConcreteSyntax() : "");
        json.put("syntax", def.getConcreteSyntax());
        return json;
    }

    private JSONArray commandsToJson(List<Command> commands) {
        JSONArray list = new JSONArray();
        if (commands == null) {
            return list;
        }
        for (Command cmd : commands) {
            if (cmd != null) {
                list.put(commandToJson(cmd));
            }
        }
        return list;
    }

    private JSONObject commandToJson(Command cmd) {
        JSONObject json = new JSONObject();
        json.put("text", cmd.getConcreteSyntax());
        json.put("syntax", cmd.getConcreteSyntax());
        return json;
    }

    private JSONArray typeCatalogToJson(BasicNode node) {
        JSONArray catalog = new JSONArray();
        if (node == null) {
            return catalog;
        }
        Set<String> seen = new LinkedHashSet<>();
        BasicNode current = node;
        boolean isLocal = true;
        while (current != null) {
            for (DataTypeDefinition def : current.getTypeDefList()) {
                if (def == null) {
                    continue;
                }
                String name = def.getName();
                if (name == null || name.isBlank() || !seen.add(name)) {
                    continue;
                }
                JSONObject entry = typeDefToJson(def);
                entry.put("ownerId", current.getId());
                entry.put("ownerName", current.getName());
                entry.put("scope", isLocal ? "local" : "inherited");
                catalog.put(entry);
            }
            current = current.getParentNode();
            isLocal = false;
        }
        return catalog;
    }

    private JSONArray typeOptionsToJson(JSONArray catalog) {
        LinkedHashSet<String> options = new LinkedHashSet<>();
        options.add("Int");
        options.add("Bool");
        options.add("Float");
        options.add("String");
        if (catalog != null) {
            for (int i = 0; i < catalog.length(); i++) {
                JSONObject entry = catalog.optJSONObject(i);
                if (entry == null) {
                    continue;
                }
                String name = entry.optString("name", "").trim();
                if (!name.isBlank()) {
                    options.add(name);
                }
            }
        }
        JSONArray list = new JSONArray();
        for (String option : options) {
            list.put(option);
        }
        return list;
    }

    private JSONObject edgeToJson(AbstractEdge edge, int index) {
        JSONObject json = new JSONObject();
        json.put("id", "E" + index);
        json.put("type", edgeType(edge));

        String sourceId = edge.getSourceUnid();
        if (sourceId == null || sourceId.isBlank()) {
            sourceId = edge.getSourceNode() != null ? edge.getSourceNode().getId() : "";
        }
        String targetId = edge.getTargetUnid();
        if (targetId == null || targetId.isBlank()) {
            targetId = edge.getTargetNode() != null ? edge.getTargetNode().getId() : "";
        }
        json.put("sourceId", sourceId);
        json.put("targetId", targetId);

        JSONObject graphics = new JSONObject();
        JSONArray points = new JSONArray();
        EdgeGraphics edgeGraphics = edge.getGraphics();
        if (edgeGraphics != null) {
            EdgeArrow arrow = edgeGraphics.getConnection();
            if (arrow != null) {
                for (EdgePoint point : arrow.getPointList()) {
                    JSONObject pointJson = new JSONObject();
                    pointJson.put("x", point.getXPos());
                    pointJson.put("y", point.getYPos());
                    pointJson.put("cx", point.getCtrlXPos());
                    pointJson.put("cy", point.getCtrlYPos());
                    points.put(pointJson);
                }
            }
        }
        graphics.put("points", points);
        json.put("graphics", graphics);

        if (edge instanceof GuargedEdge) {
            Expression condition = ((GuargedEdge) edge).getCondition();
            if (condition != null) {
                json.put("condition", condition.getConcreteSyntax());
            }
        }
        if (edge instanceof InterruptEdge) {
            Expression condition = ((InterruptEdge) edge).getCondition();
            if (condition != null) {
                json.put("condition", condition.getConcreteSyntax());
            }
        }
        if (edge instanceof RandomEdge) {
            json.put("probability", ((RandomEdge) edge).getProbability());
        }
        if (edge instanceof TimeoutEdge) {
            TimeoutEdge timeoutEdge = (TimeoutEdge) edge;
            if (timeoutEdge.getExpression() != null) {
                json.put("timeoutExpr", timeoutEdge.getExpression().getConcreteSyntax());
            }
            if (timeoutEdge.getTimeout() != Long.MIN_VALUE) {
                json.put("timeoutMs", timeoutEdge.getTimeout());
            }
        }

        JSONArray altStartMap = new JSONArray();
        Map<Tuple<String, BasicNode>, Tuple<String, BasicNode>> altMap = edge.getAltMap();
        if (altMap != null) {
            for (Map.Entry<Tuple<String, BasicNode>, Tuple<String, BasicNode>> entry : altMap.entrySet()) {
                JSONObject entryJson = new JSONObject();
                entryJson.put("startId", entry.getKey().getFirst());
                entryJson.put("altStartId", entry.getValue().getFirst());
                altStartMap.put(entryJson);
            }
        }
        json.put("altStartMap", altStartMap);

        return json;
    }

    private JSONObject commentToJson(CommentBadge comment, int index) {
        JSONObject json = new JSONObject();
        json.put("id", "C" + index);
        json.put("text", comment.getHTMLText());
        JSONObject rectJson = new JSONObject();
        CommentGraphics graphics = comment.getGraphics();
        CommentBoundary rect = (graphics != null) ? graphics.getRectangle() : null;
        if (rect != null) {
            rectJson.put("x", rect.getXPos());
            rectJson.put("y", rect.getYPos());
            rectJson.put("w", rect.getWidth());
            rectJson.put("h", rect.getHeight());
        } else {
            rectJson.put("x", 0);
            rectJson.put("y", 0);
            rectJson.put("w", 0);
            rectJson.put("h", 0);
        }
        json.put("rect", rectJson);
        return json;
    }

    private JSONObject scriptSnapshotToJson(EditorProject project, String projectId) {
        SceneScript script = project.getSceneScript();
        String text = script.getText();
        ScriptDiagnostics.Result diagnostics = ScriptDiagnostics.analyze(text);
        JSONObject response = new JSONObject();
        if (projectId != null && !projectId.isBlank()) {
            response.put("projectId", projectId);
        }
        response.put("text", text);
        response.put("version", script.getHashCode());
        response.put("parseOk", diagnostics.isParseOk());
        response.put("parseErrors", diagnosticsToJson(diagnostics.getDiagnostics()));
        return response;
    }

    private JSONObject scriptScenesToJson(SceneScript script) {
        Map<String, Map<String, Integer>> groups = new LinkedHashMap<>();
        for (SceneObject scene : script.getSceneList()) {
            String lang = scene.getLanguage();
            if (lang == null) {
                lang = "";
            }
            String name = scene.getName();
            groups.computeIfAbsent(lang, key -> new LinkedHashMap<>());
            Map<String, Integer> group = groups.get(lang);
            group.put(name, group.getOrDefault(name, 0) + 1);
        }
        JSONArray languages = new JSONArray();
        for (Map.Entry<String, Map<String, Integer>> entry : groups.entrySet()) {
            JSONObject lang = new JSONObject();
            lang.put("language", entry.getKey());
            JSONArray list = new JSONArray();
            for (Map.Entry<String, Integer> groupEntry : entry.getValue().entrySet()) {
                JSONObject group = new JSONObject();
                group.put("name", groupEntry.getKey());
                group.put("count", groupEntry.getValue());
                list.put(group);
            }
            lang.put("groups", list);
            languages.put(lang);
        }
        JSONObject response = new JSONObject();
        response.put("languages", languages);
        return response;
    }

    private JSONObject scriptElementsToJson(EditorProject project) {
        JSONObject response = new JSONObject();
        ActiconConfig acticon = project.getActicon();
        JSONArray actions = new JSONArray();
        if (acticon != null) {
            for (ActiconAction action : acticon.getActionList()) {
                JSONObject entry = new JSONObject();
                entry.put("name", action.getActionName());
                entry.put("script", action.toScript());
                actions.put(entry);
            }
        }
        response.put("acticon", actions);

        JSONArray gesticon = new JSONArray();
        for (GesticonAgent agent : project.getGesticon().getAgentList()) {
            JSONObject agentJson = new JSONObject();
            agentJson.put("agent", agent.getAgentName());
            agentJson.put("icon", agent.getAgentIcon());
            JSONArray gestures = new JSONArray();
            for (GesticonGesture gesture : agent.getGestureList()) {
                JSONObject gestureJson = new JSONObject();
                gestureJson.put("character", gesture.getCharacter());
                gestureJson.put("animName", gesture.getAnimName());
                gestureJson.put("animPath", gesture.getAnimPath());
                gestureJson.put("category", gesture.getCategory());
                gestureJson.put("blendable", gesture.isBlendable());
                gestureJson.put("duration", gesture.getDuration());
                gestureJson.put("script", gesture.toScript());
                gestures.put(gestureJson);
            }
            agentJson.put("gestures", gestures);
            gesticon.put(agentJson);
        }
        response.put("gesticon", gesticon);

        VisiconConfig visiconConfig = project.getVisicon();
        JSONArray visicon = new JSONArray();
        if (visiconConfig != null) {
            for (VisiconAgent agent : visiconConfig.getAgentList()) {
                JSONObject agentJson = new JSONObject();
                agentJson.put("agent", agent.getAgentName());
                agentJson.put("icon", agent.getAgentIcon());
                JSONArray visemes = new JSONArray();
                for (VisiconViseme viseme : agent.getVisemeList()) {
                    JSONObject visemeJson = new JSONObject();
                    visemeJson.put("key", viseme.getKey());
                    visemeJson.put("value", viseme.getValue());
                    visemes.put(visemeJson);
                }
                agentJson.put("visemes", visemes);
                visicon.put(agentJson);
            }
        }
        response.put("visicon", visicon);
        return response;
    }

    private JSONObject functionsToJson(EditorProject project) {
        JSONArray functions = new JSONArray();
        Map<String, FunctionDefinition> defs = project.getSceneFlow().getUsrCmdDefMap();
        for (FunctionDefinition def : defs.values()) {
            JSONObject entry = new JSONObject();
            entry.put("name", def.getName());
            entry.put("class", def.getClassName());
            entry.put("method", def.getMethod());
            entry.put("active", def.isActive());
            JSONArray args = new JSONArray();
            for (ArgumentDefinition arg : def.getParamList()) {
                JSONObject argJson = new JSONObject();
                argJson.put("name", arg.getName());
                argJson.put("type", arg.getType());
                args.put(argJson);
            }
            entry.put("args", args);
            functions.put(entry);
        }
        JSONObject response = new JSONObject();
        response.put("functions", functions);
        return response;
    }

    private JSONObject typesToJson(EditorProject project) {
        JSONArray custom = new JSONArray();
        for (DataTypeDefinition def : project.getSceneFlow().getTypeDefList()) {
            JSONObject entry = new JSONObject();
            entry.put("name", def.getName());
            entry.put("flavour", def.getFlavour().name());
            if (def instanceof ListTypeDefinition) {
                entry.put("elementType", ((ListTypeDefinition) def).getType());
            }
            if (def instanceof StructTypeDefinition) {
                JSONArray members = new JSONArray();
                for (MemberDefinition member : ((StructTypeDefinition) def).getMemberList()) {
                    JSONObject memberJson = new JSONObject();
                    memberJson.put("name", member.getName());
                    memberJson.put("type", member.getType());
                    members.put(memberJson);
                }
                entry.put("members", members);
            }
            custom.put(entry);
        }
        JSONObject response = new JSONObject();
        response.put("primitives", new JSONArray(Arrays.asList("Int", "Float", "Bool", "String")));
        response.put("custom", custom);
        return response;
    }

    private JSONObject configToJson(EditorProject project) {
        JSONObject config = new JSONObject();
        EditorConfig editorConfig = project.getEditorConfig();
        for (Object keyObj : editorConfig.getKeySet()) {
            String key = keyObj.toString();
            config.put(key, editorConfig.getProperty(key));
        }
        // Add computed grid spacing values for frontend consistency
        config.put("grid_space_x", editorConfig.sGRID_XSPACE);
        config.put("grid_space_y", editorConfig.sGRID_YSPACE);
        JSONObject response = new JSONObject();
        response.put("config", config);
        return response;
    }

    private JSONArray configFeaturesToJson(List<ConfigFeature> features) {
        JSONArray list = new JSONArray();
        if (features == null) {
            return list;
        }
        for (ConfigFeature feature : features) {
            if (feature == null) {
                continue;
            }
            JSONObject entry = new JSONObject();
            entry.put("key", feature.getKey());
            entry.put("value", feature.getValue());
            list.put(entry);
        }
        return list;
    }

    private ArrayList<ConfigFeature> configFeaturesFromJson(JSONArray features) {
        ArrayList<ConfigFeature> list = new ArrayList<>();
        if (features == null) {
            return list;
        }
        for (int i = 0; i < features.length(); i++) {
            JSONObject entry = features.optJSONObject(i);
            if (entry == null) {
                continue;
            }
            String key = entry.optString("key", "");
            String value = entry.optString("value", "");
            if (key.isBlank()) {
                continue;
            }
            list.add(new ConfigFeature("Feature", key, value));
        }
        return list;
    }

    private JSONObject projectConfigToJson(EditorProject project) {
        ProjectConfig projectConfig = project.getProjectConfig();
        JSONObject config = new JSONObject();
        config.put("name", projectConfig.getProjectName());
        JSONArray plugins = new JSONArray();
        for (PluginConfig plugin : projectConfig.getPluginConfigList()) {
            JSONObject entry = new JSONObject();
            entry.put("type", plugin.getPluginType());
            entry.put("name", plugin.getPluginName());
            entry.put("className", plugin.getClassName());
            entry.put("load", plugin.isMarkedtoLoad());
            entry.put("features", configFeaturesToJson(plugin.getEntryList()));
            plugins.put(entry);
        }
        JSONArray agents = new JSONArray();
        for (AgentConfig agent : projectConfig.getAgentConfigList()) {
            JSONObject entry = new JSONObject();
            entry.put("name", agent.getAgentName());
            entry.put("device", agent.getDeviceName());
            entry.put("features", configFeaturesToJson(agent.getEntryList()));
            agents.put(entry);
        }
        JSONObject player = new JSONObject();
        PlayerConfig playerConfig = projectConfig.getPlayerConfig();
        player.put("features", configFeaturesToJson(playerConfig.getEntryList()));
        config.put("plugins", plugins);
        config.put("agents", agents);
        config.put("player", player);
        JSONObject response = new JSONObject();
        response.put("config", config);
        return response;
    }

    private JSONObject preferencesToJson() {
        JSONObject prefs = new JSONObject();
        for (Object keyObj : PreferencesDesktop.getKeySet()) {
            String key = keyObj.toString();
            prefs.put(key, PreferencesDesktop.getProperty(key));
        }
        JSONObject response = new JSONObject();
        response.put("preferences", prefs);
        return response;
    }

    private JSONArray diagnosticsToJson(List<ScriptDiagnostics.Diagnostic> diagnostics) {
        JSONArray list = new JSONArray();
        if (diagnostics == null) {
            return list;
        }
        for (ScriptDiagnostics.Diagnostic diag : diagnostics) {
            JSONObject entry = new JSONObject();
            entry.put("from", diag.getFrom());
            entry.put("to", diag.getTo());
            entry.put("line", diag.getLine());
            entry.put("column", diag.getColumn());
            entry.put("severity", diag.getSeverity());
            entry.put("message", diag.getMessage());
            entry.put("source", diag.getSource());
            list.put(entry);
        }
        return list;
    }

    // Phase 6: Headless version of applyPreferencesUpdate
    private JSONObject applyPreferencesUpdate(JSONObject values) {
        try {
            for (Iterator<String> it = values.keys(); it.hasNext(); ) {
                String key = it.next();
                Object valueObj = values.get(key);
                String value = valueObj == JSONObject.NULL ? "" : String.valueOf(valueObj);
                PreferencesDesktop.setProperty(key, value);
            }
            PreferencesDesktop.save();
            // Phase 6: Removed EditorInstance.getInstance().refresh() - no Swing UI to refresh
            return preferencesToJson();
        } catch (Exception exc) {
            JSONObject error = new JSONObject();
            error.put("error", "PREFERENCES_UPDATE_FAILED");
            error.put("message", exc.getMessage());
            return error;
        }
    }

    // Phase 6: Headless version of applyConfigUpdate
    private JSONObject applyConfigUpdate(String projectId, JSONObject values) {
        // Phase 6: Use EditorProjectService instead of EditorInstance
        EditorProject project = mEditorProjectService.getProject(projectId);
        if (project == null) {
            JSONObject error = new JSONObject();
            error.put("error", "PROJECT_NOT_FOUND");
            error.put("message", "Project not found");
            return error;
        }
        EditorConfig config = project.getEditorConfig();
        for (Iterator<String> it = values.keys(); it.hasNext(); ) {
            String key = it.next();
            Object valueObj = values.get(key);
            String value = valueObj == JSONObject.NULL ? "" : String.valueOf(valueObj);
            config.setProperty(key, value);
        }
        boolean saved = false;
        boolean pending = false;
        File base = project.getProjectFile();
        if (base != null && base.exists()) {
            saved = config.save(base);
        } else {
            pending = true;
        }
        // Phase 6: Removed editor.refresh() - no Swing UI to refresh
        JSONObject response = configToJson(project);
        response.put("saved", saved);
        response.put("pending", pending);
        return response;
    }

    private boolean writeProjectConfig(EditorProject project) {
        File base = project.getProjectFile();
        if (base == null) {
            return false;
        }
        if (!base.exists() && !base.mkdirs()) {
            return false;
        }
        File file = new File(base, "project.xml");
        try {
            if (!file.exists() && !file.createNewFile()) {
                return false;
            }
        } catch (IOException exc) {
            return false;
        }
        return XMLUtilities.writeToXMLFile(project.getProjectConfig(), file, "UTF-8");
    }

    // Phase 6: Headless version of applyProjectConfigUpdate
    private JSONObject applyProjectConfigUpdate(String projectId, JSONObject payload) {
        // Phase 6: Use EditorProjectService instead of EditorInstance
        EditorProject project = mEditorProjectService.getProject(projectId);
        if (project == null) {
            JSONObject error = new JSONObject();
            error.put("error", "PROJECT_NOT_FOUND");
            error.put("message", "Project not found");
            return error;
        }
        ProjectConfig config = project.getProjectConfig();
        String name = payload.optString("name", config.getProjectName());
        config.setProjectName(name);

        List<PluginConfig> pluginList = config.getPluginConfigList();
        pluginList.clear();
        JSONArray plugins = payload.optJSONArray("plugins");
        if (plugins != null) {
            for (int i = 0; i < plugins.length(); i++) {
                JSONObject entry = plugins.optJSONObject(i);
                if (entry == null) {
                    continue;
                }
                String type = entry.optString("type", "device");
                String pluginName = entry.optString("name", "");
                String className = entry.optString("className", "");
                boolean load = entry.optBoolean("load", true);
                ArrayList<ConfigFeature> features = configFeaturesFromJson(entry.optJSONArray("features"));
                pluginList.add(new PluginConfig(type, pluginName, className, load, features));
            }
        }

        List<AgentConfig> agentList = config.getAgentConfigList();
        agentList.clear();
        JSONArray agents = payload.optJSONArray("agents");
        if (agents != null) {
            for (int i = 0; i < agents.length(); i++) {
                JSONObject entry = agents.optJSONObject(i);
                if (entry == null) {
                    continue;
                }
                String agentName = entry.optString("name", "");
                String device = entry.optString("device", "");
                ArrayList<ConfigFeature> features = configFeaturesFromJson(entry.optJSONArray("features"));
                agentList.add(new AgentConfig(agentName, device, features));
            }
        }

        PlayerConfig player = config.getPlayerConfig();
        player.getEntryList().clear();
        JSONObject playerPayload = payload.optJSONObject("player");
        ArrayList<ConfigFeature> playerFeatures = configFeaturesFromJson(
                playerPayload != null ? playerPayload.optJSONArray("features") : null);
        player.getEntryList().addAll(playerFeatures);

        boolean saved = false;
        boolean pending = false;
        File base = project.getProjectFile();
        if (base != null && base.exists()) {
            saved = writeProjectConfig(project);
        } else {
            pending = true;
        }
        // Phase 6: Removed editor.refresh() - no Swing UI to refresh
        JSONObject response = projectConfigToJson(project);
        response.put("saved", saved);
        response.put("pending", pending);
        return response;
    }

    // Phase 6: Headless version of applyScriptUpdate
    private JSONObject applyScriptUpdate(String projectId, String text, Integer expectedVersion, boolean force) {
        try {
            EditorProject project = mEditorProjectService.getProject(projectId);
            if (project == null) {
                JSONObject error = new JSONObject();
                error.put("error", "PROJECT_NOT_FOUND");
                error.put("message", "Project not found");
                return error;
            }
            SceneScript script = project.getSceneScript();
            int currentVersion = script.getHashCode();
            if (!force && expectedVersion != null && expectedVersion != currentVersion) {
                JSONObject response = new JSONObject();
                response.put("applied", false);
                response.put("parseOk", false);
                response.put("reason", "VERSION_MISMATCH");
                response.put("version", currentVersion);
                response.put("text", script.getText());
                return response;
            }
            boolean parseOk = script.parseTXT(text);
            ScriptDiagnostics.Result diagnostics = parseOk
                    ? new ScriptDiagnostics.Result(true, List.of())
                    : ScriptDiagnostics.analyze(text);
            JSONObject response = new JSONObject();
            response.put("applied", parseOk);
            response.put("parseOk", parseOk);
            response.put("version", script.getHashCode());
            response.put("text", script.getText());
            response.put("parseErrors", diagnosticsToJson(diagnostics.getDiagnostics()));
            response.put("dirty", project.hasChanged());
            if (!parseOk) {
                response.put("reason", "PARSE_FAILED");
            }
            return response;
        } catch (Exception exc) {
            JSONObject error = new JSONObject();
            error.put("error", "SCRIPT_UPDATE_FAILED");
            error.put("message", exc.getMessage());
            return error;
        }
    }

    // Phase 6: Headless version of applyRuntimeVariableUpdate
    private JSONObject applyRuntimeVariableUpdate(String projectId, String name, String valueExpr) {
        try {
            // Phase 6: Use EditorProjectService instead of EditorInstance
            EditorProject project = mEditorProjectService.getProject(projectId);
            if (project == null) {
                return null;
            }
            VariableDefinition def = findRuntimeVariableDefinitionHeadless(project, name);
            if (def == null) {
                JSONObject error = new JSONObject();
                error.put("error", "VAR_NOT_FOUND");
                error.put("message", "Variable not found");
                return error;
            }
            Expression exp;
            try {
                String trimmedExpr = valueExpr != null ? valueExpr.trim() : "";
                Object parsed = GlueParser.run(trimmedExpr);
                if (!(parsed instanceof Expression)) {
                    JSONObject error = new JSONObject();
                    error.put("error", "PARSE_FAILED");
                    error.put("message", "Expression could not be parsed");
                    return error;
                }
                exp = (Expression) parsed;
            } catch (Exception exc) {
                JSONObject error = new JSONObject();
                error.put("error", "PARSE_FAILED");
                error.put("message", exc.getMessage());
                return error;
            }
            if (!isSupportedRuntimeExpression(exp)) {
                JSONObject error = new JSONObject();
                error.put("error", "UNSUPPORTED_EXPRESSION");
                error.put("message", "Expression type is not supported");
                return error;
            }
            boolean ok = applyRuntimeExpression(project, name, exp);
            if (!ok) {
                JSONObject error = new JSONObject();
                error.put("error", "SET_FAILED");
                error.put("message", "Failed to update variable");
                return error;
            }
            JSONObject response = new JSONObject();
            response.put("projectId", projectId);
            response.put("name", name);
            String value = resolveVariableValue(project, name);
            if (value != null) {
                response.put("value", value);
            }
            return response;
        } catch (Exception exc) {
            JSONObject error = new JSONObject();
            error.put("error", "SET_FAILED");
            error.put("message", exc.getMessage());
            return error;
        }
    }

    // Phase 6: Headless version of findRuntimeVariableDefinition
    private VariableDefinition findRuntimeVariableDefinitionHeadless(EditorProject project, String name) {
        if (project == null || name == null || name.isBlank()) {
            return null;
        }
        SceneFlow sceneFlow = project.getSceneFlow();
        if (sceneFlow == null) {
            return null;
        }
        // Search in root sceneflow variables
        for (VariableDefinition def : sceneFlow.getVarDefList()) {
            if (name.equals(def.getName())) {
                return def;
            }
        }
        // Search recursively in all supernodes
        return findVariableDefinitionRecursive(sceneFlow, name);
    }

    private VariableDefinition findVariableDefinitionRecursive(SuperNode node, String name) {
        if (node == null || name == null) {
            return null;
        }
        for (BasicNode child : node.getNodeAndSuperNodeList()) {
            if (child instanceof SuperNode) {
                SuperNode superNode = (SuperNode) child;
                for (VariableDefinition def : superNode.getVarDefList()) {
                    if (name.equals(def.getName())) {
                        return def;
                    }
                }
                VariableDefinition found = findVariableDefinitionRecursive(superNode, name);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private JSONObject applyRuntimeQuery(String query) {
        JSONObject response = new JSONObject();
        int count = 0;
        try {
            String trimmed = query != null ? query.trim() : "";
            count = JPLEngine.query(trimmed).size();
        } catch (Exception exc) {
            mLogger.failure(exc.toString());
        }
        response.put("count", count);
        return response;
    }

    private VariableDefinition findRuntimeVariableDefinition(ProjectEditor editor, String name) {
        if (editor == null || name == null || name.isBlank()) {
            return null;
        }
        EditorProject project = editor.getEditorProject();
        if (project == null || project.getSceneFlow() == null) {
            return null;
        }
        for (VariableDefinition def : project.getSceneFlow().getVarDefList()) {
            if (name.equals(def.getName())) {
                return def;
            }
        }
        SceneFlowManager manager = editor.getSceneFlowEditor().getSceneFlowManager();
        if (manager == null) {
            return null;
        }
        SuperNode current = manager.getCurrentActiveSuperNode();
        if (current == null) {
            return null;
        }
        for (VariableDefinition def : current.getVarDefList()) {
            if (name.equals(def.getName())) {
                return def;
            }
        }
        return null;
    }

    private boolean isSupportedRuntimeExpression(Expression exp) {
        if (exp instanceof BoolLiteral
                || exp instanceof IntLiteral
                || exp instanceof FloatLiteral
                || exp instanceof StringLiteral) {
            return true;
        }
        if (exp instanceof UnaryExpression) {
            Expression inner = ((UnaryExpression) exp).getExp();
            return inner instanceof IntLiteral || inner instanceof FloatLiteral;
        }
        return false;
    }

    private boolean applyRuntimeExpression(EditorProject project, String name, Expression exp) {
        if (exp == null || project == null || name == null || name.isBlank()) {
            return false;
        }
        if (exp instanceof BoolLiteral) {
            return project.setVariable(name, ((BoolLiteral) exp).getValue());
        }
        if (exp instanceof IntLiteral) {
            return project.setVariable(name, ((IntLiteral) exp).getValue());
        }
        if (exp instanceof FloatLiteral) {
            return project.setVariable(name, ((FloatLiteral) exp).getValue());
        }
        if (exp instanceof StringLiteral) {
            return project.setVariable(name, ((StringLiteral) exp).getValue());
        }
        if (exp instanceof UnaryExpression) {
            Expression inner = ((UnaryExpression) exp).getExp();
            if (inner instanceof IntLiteral) {
                return project.setVariable(name, -1 * ((IntLiteral) inner).getValue());
            }
            if (inner instanceof FloatLiteral) {
                return project.setVariable(name, -1 * ((FloatLiteral) inner).getValue());
            }
        }
        return false;
    }

    private JSONObject runtimeToJson(ProjectEditor editor) {
        EditorProject project = editor.getEditorProject();
        SceneFlowManager manager = editor.getSceneFlowEditor().getSceneFlowManager();
        SuperNode current = manager.getCurrentActiveSuperNode();
        JSONObject response = new JSONObject();
        String state = project.isRunning()
                ? (project.isPaused() ? "paused" : "running")
                : "stopped";
        response.put("state", state);
        Map<String, DataTypeDefinition> typeMap = new LinkedHashMap<>();
        for (DataTypeDefinition def : project.getSceneFlow().getTypeDefList()) {
            typeMap.put(def.getName(), def);
        }
        JSONArray globals = new JSONArray();
        for (VariableDefinition def : project.getSceneFlow().getVarDefList()) {
            globals.put(variableToJson(def, typeMap, "global", project));
        }
        JSONArray locals = new JSONArray();
        for (VariableDefinition def : current.getVarDefList()) {
            locals.put(variableToJson(def, typeMap, "local", project));
        }
        response.put("globalVariables", globals);
        response.put("localVariables", locals);
        return response;
    }

    // Phase 6: Headless version of runtimeToJson
    private JSONObject runtimeToJson(EditorProject project) {
        // Use root sceneflow as current active supernode since we don't have SceneFlowManager
        SuperNode current = project.getSceneFlow();
        JSONObject response = new JSONObject();
        String state = project.isRunning()
                ? (project.isPaused() ? "paused" : "running")
                : "stopped";
        response.put("state", state);
        Map<String, DataTypeDefinition> typeMap = new LinkedHashMap<>();
        for (DataTypeDefinition def : project.getSceneFlow().getTypeDefList()) {
            typeMap.put(def.getName(), def);
        }
        JSONArray globals = new JSONArray();
        for (VariableDefinition def : project.getSceneFlow().getVarDefList()) {
            globals.put(variableToJson(def, typeMap, "global", project));
        }
        JSONArray locals = new JSONArray();
        for (VariableDefinition def : current.getVarDefList()) {
            locals.put(variableToJson(def, typeMap, "local", project));
        }
        response.put("globalVariables", globals);
        response.put("localVariables", locals);
        return response;
    }

    private JSONObject variableToJson(VariableDefinition def, Map<String, DataTypeDefinition> typeMap, String scope, RunTimeProject project) {
        JSONObject json = new JSONObject();
        json.put("name", def.getName());
        json.put("type", def.getType());
        json.put("typeFlavor", resolveTypeFlavor(def.getType(), typeMap));
        json.put("expr", def.getExp() != null ? def.getExp().getConcreteSyntax() : "");
        json.put("scope", scope);
        String value = resolveVariableValue(project, def.getName());
        if (value != null) {
            json.put("value", value);
        }
        return json;
    }

    private String resolveVariableValue(RunTimeProject project, String name) {
        if (project == null || name == null || name.isBlank()) {
            return null;
        }
        AbstractValue value = project.getValueOf(name);
        if (value == null) {
            return null;
        }
        return sanitizeVariableValue(value.getConcreteSyntax());
    }

    private String sanitizeVariableValue(String value) {
        if (value == null) {
            return null;
        }
        return value.replaceAll("#[a-zA-Z]#", "");
    }

    private String resolveTypeFlavor(String type, Map<String, DataTypeDefinition> typeMap) {
        if (type == null) {
            return "Primitive";
        }
        DataTypeDefinition def = typeMap.get(type);
        if (def != null) {
            return def.getFlavour().name();
        }
        if ("Int".equalsIgnoreCase(type)
                || "Float".equalsIgnoreCase(type)
                || "Bool".equalsIgnoreCase(type)
                || "String".equalsIgnoreCase(type)) {
            return "Primitive";
        }
        return "Primitive";
    }

    private Set<String> collectAltStartIds(SceneFlowManager manager, SuperNode target) {
        Set<String> altStartIds = new LinkedHashSet<>();
        SuperNode parent = manager.getParentSuperNode(target);
        if (parent == null) {
            return altStartIds;
        }
        for (BasicNode node : parent.getNodeAndSuperNodeList()) {
            for (AbstractEdge edge : node.getEdgeList()) {
                if (!target.getId().equals(edge.getTargetUnid())) {
                    continue;
                }
                Map<Tuple<String, BasicNode>, Tuple<String, BasicNode>> altMap = edge.getAltMap();
                if (altMap == null) {
                    continue;
                }
                for (Tuple<String, BasicNode> alt : altMap.values()) {
                    if (alt != null && alt.getFirst() != null && !alt.getFirst().isEmpty()) {
                        altStartIds.add(alt.getFirst());
                    }
                }
            }
        }
        return altStartIds;
    }

    private List<SuperNode> findPathToSuperNode(SceneFlow root, String targetId) {
        if (targetId == null || targetId.isBlank()) {
            return new ArrayList<>();
        }
        List<SuperNode> path = new ArrayList<>();
        if (findPathRecursive(root, targetId, path)) {
            return path;
        }
        return null;
    }

    private boolean findPathRecursive(SuperNode current, String targetId, List<SuperNode> path) {
        path.add(current);
        if (targetId.equals(current.getId())) {
            return true;
        }
        for (SuperNode child : current.getSuperNodeList()) {
            if (findPathRecursive(child, targetId, path)) {
                return true;
            }
        }
        path.remove(path.size() - 1);
        return false;
    }

    private SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId) {
        if (superNodeId == null || superNodeId.isBlank()) {
            return null;
        }
        if (ROOT_SUPERNODE_ID.equals(superNodeId)) {
            return sceneFlow;
        }
        List<SuperNode> path = findPathToSuperNode(sceneFlow, superNodeId);
        if (path == null || path.isEmpty()) {
            return null;
        }
        return path.get(path.size() - 1);
    }

    private String edgeType(AbstractEdge edge) {
        if (edge instanceof GuargedEdge) {
            return "CEDGE";
        }
        if (edge instanceof RandomEdge) {
            return "PEDGE";
        }
        if (edge instanceof InterruptEdge) {
            return "IEDGE";
        }
        if (edge instanceof ForkingEdge) {
            return "FEDGE";
        }
        if (edge instanceof TimeoutEdge) {
            return "TEDGE";
        }
        if (edge instanceof EpsilonEdge) {
            return "EEDGE";
        }
        return "UNKNOWN";
    }

    private Edge.TYPE parseEdgeCreateType(String value) {
        if (value == null || value.isBlank()) {
            return Edge.TYPE.EEDGE;
        }
        String normalized = value.trim().toUpperCase(Locale.ROOT);
        switch (normalized) {
            case "EPSILON":
            case "EEDGE":
                return Edge.TYPE.EEDGE;
            case "CEDGE":
            case "CONDITIONAL":
            case "CONDITION":
                return Edge.TYPE.CEDGE;
            case "IEDGE":
            case "INTERRUPT":
            case "INTERRUPTIVE":
                return Edge.TYPE.IEDGE;
            case "PEDGE":
            case "PROBABILITY":
            case "PROB":
                return Edge.TYPE.PEDGE;
            case "TEDGE":
            case "TIMEOUT":
                return Edge.TYPE.TEDGE;
            case "FEDGE":
            case "FORK":
            case "FORKING":
                return Edge.TYPE.FEDGE;
            default:
                return null;
        }
    }

    private ProjectEditor findProjectEditorById(String projectId, EditorInstance instance) {
        JTabbedPane tabs = instance.getProjectEditors();
        for (int i = 0; i < tabs.getTabCount(); i++) {
            Component comp = tabs.getComponentAt(i);
            if (comp instanceof ProjectEditor) {
                ProjectEditor editor = (ProjectEditor) comp;
                if (projectId.equals(projectIdFor(editor))) {
                    return editor;
                }
            }
        }
        return null;
    }

    private String projectIdFor(ProjectEditor editor) {
        return "p" + Integer.toHexString(System.identityHashCode(editor));
    }

    private JSONObject readJsonBody(Context ctx) {
        String body = ctx.body();
        if (body == null || body.isBlank()) {
            return new JSONObject();
        }
        return new JSONObject(body);
    }

    private void writeJson(Context ctx, JSONObject json) {
        ctx.contentType("application/json");
        ctx.result(json.toString());
    }

    private void writeError(Context ctx, int status, String error, String message) {
        JSONObject payload = new JSONObject();
        payload.put("error", error);
        payload.put("message", message);
        ctx.status(status);
        writeJson(ctx, payload);
    }

    private void sendResponse(WsContext ctx, String requestId, String name, JSONObject payload) {
        JSONObject response = new JSONObject();
        response.put("type", "response");
        if (requestId != null) {
            response.put("id", requestId);
        }
        response.put("name", name);
        JSONObject payloadObj = payload == null ? new JSONObject() : payload;
        response.put("payload", payloadObj);
        ctx.send(response.toString());
        maybeEmitUiSceneFlowSnapshot(payloadObj);
    }

    private void sendError(WsContext ctx, String requestId, String name, String message) {
        JSONObject response = new JSONObject();
        response.put("type", "error");
        if (requestId != null) {
            response.put("id", requestId);
        }
        response.put("name", name);
        JSONObject payload = new JSONObject();
        payload.put("message", message);
        response.put("payload", payload);
        ctx.send(response.toString());
    }

    private void maybeEmitUiSceneFlowSnapshot(JSONObject payload) {
        if (payload == null) {
            return;
        }
        JSONObject snapshot = payload.optJSONObject("snapshot");
        if (snapshot != null) {
            emitUiSceneFlowSnapshot(snapshot);
            return;
        }
        if (payload.has("nodes") && payload.has("edges") && payload.has("superNodeId")) {
            emitUiSceneFlowSnapshot(payload);
        }
    }

    private void appendDirty(ProjectEditor editor, JSONObject payload) {
        if (editor == null || payload == null || editor.getEditorProject() == null) {
            return;
        }
        payload.put("dirty", editor.getEditorProject().hasChanged());
    }

    private void broadcastDirtyIfPresent(JSONObject response, String projectId) {
        if (response == null || projectId == null || projectId.isBlank() || !response.has("dirty")) {
            return;
        }
        boolean dirty = response.optBoolean("dirty", false);
        emitUiProjectDirty(projectId, dirty, List.of("sceneflow"));
    }

    private void broadcast(JSONObject message) {
        if (mBroadcastExecutor.isShutdown()) {
            return;
        }
        final String payload = message.toString();
        mBroadcastExecutor.execute(() -> {
            for (WsContext ctx : new ArrayList<>(mSockets)) {
                try {
                    ctx.send(payload);
                } catch (Exception e) {
                    mSockets.remove(ctx);
                    mLogger.warning("Web UI WS send failed: " + e.getMessage());
                }
            }
        });
    }

    private boolean isFinite(double value) {
        return Double.isFinite(value);
    }

    private int clampPositive(int value) {
        return Math.max(1, value);
    }

    private Point clampPointToPositive(Point input) {
        if (input == null) {
            return null;
        }
        return new Point(clampPositive(input.x), clampPositive(input.y));
    }

    private void moveNode(WorkSpacePanel workSpace, Node node, Point target, boolean snap) {
        if (workSpace == null || node == null || target == null) {
            return;
        }
        Point current = node.getLocation();
        if (current == null) {
            current = new Point(node.getX(), node.getY());
        }
        if (current.equals(target)) {
            return;
        }
        workSpace.getGridManager().freeGridPosition(current);
        Point finalTarget = target;
        if (snap) {
            Point gridTarget = workSpace.getGridManager().getNodeLocation(target);
            if (gridTarget != null) {
                finalTarget = gridTarget;
            }
        }
        finalTarget = clampPointToPositive(finalTarget);
        Point delta = new Point(finalTarget.x - current.x, finalTarget.y - current.y);
        node.resetLocation(finalTarget);
        NodeGraphics graphics = node.getDataNode().getGraphics();
        if (graphics == null) {
            graphics = new NodeGraphics(node.getX(), node.getY());
            node.getDataNode().setGraphics(graphics);
        } else {
            graphics.setPosition(node.getX(), node.getY());
        }
        for (Edge edge : workSpace.getEdges()) {
            if (edge != null && edge.mEg != null) {
                edge.mEg.updateDrawingParameters();
            }
        }
        CmdBadge badge = workSpace.getCmdBadge(node);
        if (badge != null) {
            badge.updateLocation(delta);
        }
        workSpace.revalidate();
        workSpace.repaint(100);
    }

    private BasicNode resolveNodeById(SuperNode superNode, String nodeId) {
        if (superNode == null || nodeId == null) {
            return null;
        }
        if (nodeId.equals(superNode.getId())) {
            return superNode;
        }
        for (BasicNode node : superNode.getNodeAndSuperNodeList()) {
            if (nodeId.equals(node.getId())) {
                return node;
            }
        }
        return null;
    }

    // Phase 6: Recursive helper to find node anywhere in sceneflow
    private BasicNode findNodeRecursive(SuperNode root, String nodeId) {
        if (root == null || nodeId == null) {
            return null;
        }
        // Check if root itself matches
        if (nodeId.equals(root.getId())) {
            return root;
        }
        // Check direct children
        for (BasicNode node : root.getNodeAndSuperNodeList()) {
            if (nodeId.equals(node.getId())) {
                return node;
            }
            // Recursively search in child supernodes
            if (node instanceof SuperNode) {
                BasicNode found = findNodeRecursive((SuperNode) node, nodeId);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    // Phase 6: Helper to resolve the target SuperNode for snapshot based on superNodeId parameter
    private SuperNode resolveSnapshotTarget(SceneFlow sceneFlow, String superNodeId) {
        if (superNodeId == null || superNodeId.isBlank() || ROOT_SUPERNODE_ID.equals(superNodeId)) {
            return sceneFlow;
        }
        BasicNode found = findNodeRecursive(sceneFlow, superNodeId);
        if (found instanceof SuperNode) {
            return (SuperNode) found;
        }
        return sceneFlow; // Fallback to root
    }

    // Phase 6: Generate unique node ID with N/S prefix
    private String generateNextNodeId(SuperNode root, boolean isSuperNode) {
        int maxId = 0;
        String prefix = isSuperNode ? "S" : "N";
        maxId = Math.max(maxId, extractMaxId(root, prefix));
        return prefix + (maxId + 1);
    }

    private int extractMaxId(SuperNode root, String prefix) {
        int max = 0;
        if (root.getId().startsWith(prefix)) {
            try {
                int id = Integer.parseInt(root.getId().substring(prefix.length()));
                max = Math.max(max, id);
            } catch (NumberFormatException ignored) {
            }
        }
        for (BasicNode node : root.getNodeAndSuperNodeList()) {
            if (node.getId().startsWith(prefix)) {
                try {
                    int id = Integer.parseInt(node.getId().substring(prefix.length()));
                    max = Math.max(max, id);
                } catch (NumberFormatException ignored) {
                }
            }
            if (node instanceof SuperNode) {
                max = Math.max(max, extractMaxId((SuperNode) node, prefix));
            }
        }
        return max;
    }

    // Phase 6: Reassign IDs to all nodes recursively (headless version of IDManager.reassignAllIDs)
    private void reassignAllIDsHeadless(SceneFlow sceneFlow, List<BasicNode> nodes) {
        for (BasicNode node : nodes) {
            reassignNodeIdRecursive(sceneFlow, node);
        }
    }

    private void reassignNodeIdRecursive(SceneFlow sceneFlow, BasicNode node) {
        if (node == null) {
            return;
        }
        // Generate new ID based on whether it's a SuperNode
        boolean isSuper = node instanceof SuperNode;
        String newId = generateNextNodeId(sceneFlow, isSuper);
        node.setId(newId);
        // Recursively process children if this is a SuperNode
        if (isSuper) {
            SuperNode superNode = (SuperNode) node;
            for (BasicNode child : superNode.getNodeAndSuperNodeList()) {
                reassignNodeIdRecursive(sceneFlow, child);
            }
        }
    }

    // Phase 6: Release dock points for an edge before deletion
    private void releaseDockPointsForEdge(AbstractEdge edge, EditorConfig config) {
        if (edge == null) return;

        int nodeWidth = config != null ? config.sNODEWIDTH : 90;
        int nodeHeight = config != null ? config.sNODEHEIGHT : 90;

        EdgeGraphics edgeGraphics = edge.getGraphics();
        if (edgeGraphics == null || edgeGraphics.getConnection() == null) return;

        List<EdgePoint> points = edgeGraphics.getConnection().getPointList();
        if (points == null || points.size() < 2) return;

        EdgePoint startPt = points.get(0);
        EdgePoint endPt = points.get(points.size() - 1);

        // Release source dock point
        BasicNode sourceNode = edge.getSourceNode();
        if (sourceNode != null) {
            NodeGraphics srcGraphics = sourceNode.getGraphics();
            NodePosition srcPos = srcGraphics != null ? srcGraphics.getPosition() : null;
            double srcX = srcPos != null ? srcPos.getXPos() : 0;
            double srcY = srcPos != null ? srcPos.getYPos() : 0;
            int srcDockIdx = findDockPointIndex(srcX, srcY, nodeWidth, nodeHeight,
                    sourceNode instanceof SuperNode, startPt.getXPos(), startPt.getYPos());
            if (srcDockIdx >= 0) {
                releaseDockPoint(sourceNode.getId(), srcDockIdx, true);
            }
        }

        // Release target dock point
        BasicNode targetNode = edge.getTargetNode();
        if (targetNode != null) {
            NodeGraphics tgtGraphics = targetNode.getGraphics();
            NodePosition tgtPos = tgtGraphics != null ? tgtGraphics.getPosition() : null;
            double tgtX = tgtPos != null ? tgtPos.getXPos() : 0;
            double tgtY = tgtPos != null ? tgtPos.getYPos() : 0;
            int tgtDockIdx = findDockPointIndex(tgtX, tgtY, nodeWidth, nodeHeight,
                    targetNode instanceof SuperNode, endPt.getXPos(), endPt.getYPos());
            if (tgtDockIdx >= 0) {
                releaseDockPoint(targetNode.getId(), tgtDockIdx, false);
            }
        }
    }

    // Phase 6: Remove all edges connected to a node
    private void removeConnectedEdges(SuperNode parent, BasicNode node, EditorConfig config) {
        if (parent == null || node == null) {
            return;
        }
        String nodeId = node.getId();
        // Iterate through all nodes in parent and remove edges connected to the deleted node
        for (BasicNode n : parent.getNodeAndSuperNodeList()) {
            // Remove C edges
            List<GuargedEdge> cEdgesToRemove = new ArrayList<>();
            for (AbstractEdge edge : n.getEdgeList()) {
                if (edge instanceof GuargedEdge) {
                    GuargedEdge cEdge = (GuargedEdge) edge;
                    BasicNode source = cEdge.getSourceNode();
                    BasicNode target = cEdge.getTargetNode();
                    if ((source != null && nodeId.equals(source.getId())) || (target != null && nodeId.equals(target.getId()))) {
                        cEdgesToRemove.add(cEdge);
                    }
                }
            }
            for (GuargedEdge edge : cEdgesToRemove) {
                releaseDockPointsForEdge(edge, config);
                BasicNode source = edge.getSourceNode();
                if (source != null) {
                    source.removeCEdge(edge);
                }
            }
            // Remove I edges
            List<InterruptEdge> iEdgesToRemove = new ArrayList<>();
            for (AbstractEdge edge : n.getEdgeList()) {
                if (edge instanceof InterruptEdge) {
                    InterruptEdge iEdge = (InterruptEdge) edge;
                    BasicNode source = iEdge.getSourceNode();
                    BasicNode target = iEdge.getTargetNode();
                    if ((source != null && nodeId.equals(source.getId())) || (target != null && nodeId.equals(target.getId()))) {
                        iEdgesToRemove.add(iEdge);
                    }
                }
            }
            for (InterruptEdge edge : iEdgesToRemove) {
                releaseDockPointsForEdge(edge, config);
                BasicNode source = edge.getSourceNode();
                if (source != null) {
                    source.removeIEdge(edge);
                }
            }
            // Remove P edges
            List<RandomEdge> pEdgesToRemove = new ArrayList<>();
            for (AbstractEdge edge : n.getEdgeList()) {
                if (edge instanceof RandomEdge) {
                    RandomEdge pEdge = (RandomEdge) edge;
                    BasicNode source = pEdge.getSourceNode();
                    BasicNode target = pEdge.getTargetNode();
                    if ((source != null && nodeId.equals(source.getId())) || (target != null && nodeId.equals(target.getId()))) {
                        pEdgesToRemove.add(pEdge);
                    }
                }
            }
            for (RandomEdge edge : pEdgesToRemove) {
                releaseDockPointsForEdge(edge, config);
                BasicNode source = edge.getSourceNode();
                if (source != null) {
                    source.removePEdge(edge);
                }
            }
            // Remove F edges
            List<ForkingEdge> fEdgesToRemove = new ArrayList<>();
            for (AbstractEdge edge : n.getEdgeList()) {
                if (edge instanceof ForkingEdge) {
                    ForkingEdge fEdge = (ForkingEdge) edge;
                    BasicNode source = fEdge.getSourceNode();
                    BasicNode target = fEdge.getTargetNode();
                    if ((source != null && nodeId.equals(source.getId())) || (target != null && nodeId.equals(target.getId()))) {
                        fEdgesToRemove.add(fEdge);
                    }
                }
            }
            for (ForkingEdge edge : fEdgesToRemove) {
                releaseDockPointsForEdge(edge, config);
                BasicNode source = edge.getSourceNode();
                if (source != null) {
                    source.removeFEdge(edge);
                }
            }
            // Remove D-edge (default edge - includes EpsilonEdge and TimeoutEdge)
            AbstractEdge dEdge = n.getDedge();
            if (dEdge != null) {
                BasicNode target = dEdge.getTargetNode();
                String targetId = dEdge.getTargetUnid();
                if ((target != null && nodeId.equals(target.getId())) || nodeId.equals(targetId)) {
                    releaseDockPointsForEdge(dEdge, config);
                    n.removeDEdge();
                }
            }
        }
    }

    private BasicNode resolveNodeForDefinitions(SuperNode superNode, String nodeId) {
        if (superNode == null) {
            return null;
        }
        if (nodeId == null || nodeId.isBlank()) {
            return superNode;
        }
        return resolveNodeById(superNode, nodeId);
    }

    private void updateStartFlag(BasicNode dataNode, Node guiNode, boolean isStart) {
        if (dataNode == null) {
            return;
        }
        SuperNode parent = dataNode.getParentNode();
        if (parent == null) {
            return;
        }
        Map<String, BasicNode> startMap = parent.getStartNodeMap();
        boolean currentlyStart = startMap.containsKey(dataNode.getId());
        if (isStart == currentlyStart) {
            return;
        }
        if (isStart) {
            startMap.put(dataNode.getId(), dataNode);
            occupyStartSignDockPoint(dataNode.getId());
            if (guiNode != null) {
                guiNode.addStartSign();
            }
        } else {
            startMap.remove(dataNode.getId());
            releaseStartSignDockPoint(dataNode.getId());
            if (guiNode != null) {
                guiNode.removeStartSign();
            }
        }
    }

    private void applyNodeState(BasicNode dataNode, Node guiNode, WorkSpacePanel workSpace, String name, String comment, boolean isStart) {
        if (dataNode == null) {
            return;
        }
        if (!dataNode.isHistoryNode()) {
            String safeName = (name == null || name.isBlank()) ? dataNode.getId() : name;
            dataNode.setName(safeName);
        }
        dataNode.setComment(comment == null ? "" : comment);
        updateStartFlag(dataNode, guiNode, isStart);
        if (guiNode != null) {
            guiNode.update(null, null);
        }
        if (workSpace != null) {
            workSpace.revalidate();
            workSpace.repaint(100);
        }
    }

    private void applyCommentState(CommentBadge badge, Comment guiComment, WorkSpacePanel workSpace,
                                   CommentBoundary boundary, String htmlText) {
        if (badge == null) {
            return;
        }
        CommentGraphics graphics = badge.getGraphics();
        if (graphics == null) {
            graphics = new CommentGraphics();
            badge.setGraphics(graphics);
        }
        if (boundary != null) {
            CommentBoundary next = boundary.getCopy();
            graphics.setRectangle(next);
            if (guiComment != null) {
                guiComment.setBounds(next.getXPos(), next.getYPos(), next.getWidth(), next.getHeight());
                guiComment.revalidate();
                guiComment.repaint(100);
            }
        }
        if (htmlText != null) {
            badge.setHTMLText(htmlText);
        }
        if (workSpace != null) {
            workSpace.revalidate();
            workSpace.repaint(100);
        }
    }

    private boolean commentBoundaryEquals(CommentBoundary left, CommentBoundary right) {
        if (left == null && right == null) {
            return true;
        }
        if (left == null || right == null) {
            return false;
        }
        return left.getXPos() == right.getXPos()
                && left.getYPos() == right.getYPos()
                && left.getWidth() == right.getWidth()
                && left.getHeight() == right.getHeight();
    }

    private List<DataTypeDefinition> copyTypeDefList(List<DataTypeDefinition> list) {
        List<DataTypeDefinition> copy = new ArrayList<>();
        if (list == null) {
            return copy;
        }
        for (DataTypeDefinition def : list) {
            if (def != null) {
                copy.add(def.getCopy());
            }
        }
        return copy;
    }

    private List<VariableDefinition> copyVarDefList(List<VariableDefinition> list) {
        List<VariableDefinition> copy = new ArrayList<>();
        if (list == null) {
            return copy;
        }
        for (VariableDefinition def : list) {
            if (def != null) {
                copy.add(def.getCopy());
            }
        }
        return copy;
    }

    private List<Command> copyCommandList(List<Command> list) {
        List<Command> copy = new ArrayList<>();
        if (list == null) {
            return copy;
        }
        for (Command cmd : list) {
            if (cmd != null) {
                copy.add(cmd.getCopy());
            }
        }
        return copy;
    }

    private void applyTypeDefList(BasicNode node, List<DataTypeDefinition> list) {
        if (node == null) {
            return;
        }
        ArrayList<DataTypeDefinition> copy = new ArrayList<>();
        if (list != null) {
            for (DataTypeDefinition def : list) {
                if (def != null) {
                    copy.add(def.getCopy());
                }
            }
        }
        node.setTypeDefList(copy);
    }

    private void applyVarDefList(BasicNode node, List<VariableDefinition> list) {
        if (node == null) {
            return;
        }
        ArrayList<VariableDefinition> copy = new ArrayList<>();
        if (list != null) {
            for (VariableDefinition def : list) {
                if (def != null) {
                    copy.add(def.getCopy());
                }
            }
        }
        node.setVarDefList(copy);
    }

    private void applyCommandList(BasicNode node, List<Command> list) {
        if (node == null) {
            return;
        }
        ArrayList<Command> copy = new ArrayList<>();
        if (list != null) {
            for (Command cmd : list) {
                if (cmd != null) {
                    copy.add(cmd.getCopy());
                }
            }
        }
        node.setCmdList(copy);
    }

    private void applyVarRenameSnapshot(EditorProject project, VarRenameSnapshot snapshot, boolean useAfter) {
        if (project == null || snapshot == null) {
            return;
        }
        Map<BasicNode, List<VariableDefinition>> varDefs = useAfter ? snapshot.nodeVarDefsAfter : snapshot.nodeVarDefsBefore;
        for (Map.Entry<BasicNode, List<VariableDefinition>> entry : varDefs.entrySet()) {
            applyVarDefList(entry.getKey(), entry.getValue());
        }
        Map<BasicNode, List<Command>> nodeCommands = useAfter ? snapshot.nodeCommandsAfter : snapshot.nodeCommandsBefore;
        for (Map.Entry<BasicNode, List<Command>> entry : nodeCommands.entrySet()) {
            applyCommandList(entry.getKey(), entry.getValue());
        }
        Map<AbstractEdge, List<Command>> edgeCommands = useAfter ? snapshot.edgeCommandsAfter : snapshot.edgeCommandsBefore;
        for (Map.Entry<AbstractEdge, List<Command>> entry : edgeCommands.entrySet()) {
            applyEdgeCommandList(entry.getKey(), entry.getValue());
        }
        Map<AbstractEdge, Expression> edgeConditions = useAfter ? snapshot.edgeConditionsAfter : snapshot.edgeConditionsBefore;
        for (Map.Entry<AbstractEdge, Expression> entry : edgeConditions.entrySet()) {
            applyEdgeCondition(entry.getKey(), entry.getValue());
        }
        Map<AbstractEdge, Expression> edgeTimeouts = useAfter ? snapshot.edgeTimeoutsAfter : snapshot.edgeTimeoutsBefore;
        for (Map.Entry<AbstractEdge, Expression> entry : edgeTimeouts.entrySet()) {
            applyEdgeTimeoutExpression(entry.getKey(), entry.getValue());
        }
        if (snapshot.scriptChanged) {
            SceneScript script = project.getSceneScript();
            String text = useAfter ? snapshot.scriptAfter : snapshot.scriptBefore;
            if (text != null) {
                script.parseTXT(text);
            }
        }
    }

    private VarRenameSnapshot renameVariableReferences(EditorProject project, String oldName, String newName, StringBuilder error) {
        if (project == null || oldName == null || newName == null) {
            return null;
        }
        if (oldName.isBlank() || newName.isBlank() || oldName.equals(newName)) {
            return new VarRenameSnapshot();
        }
        Pattern identifierPattern = buildIdentifierPattern(oldName);
        Pattern scriptPattern = buildSceneScriptVarPattern(oldName);
        VarRenameSnapshot snapshot = new VarRenameSnapshot();
        SuperNode root = project.getSceneFlow();
        List<BasicNode> nodes = collectSceneFlowNodes(root);
        for (BasicNode node : nodes) {
            if (node == null) {
                continue;
            }
            List<VariableDefinition> updatedVarDefs = renameVarDefList(node.getVarDefList(), identifierPattern, oldName, newName, error);
            if (error != null && error.length() > 0) {
                applyVarRenameSnapshot(project, snapshot, false);
                return null;
            }
            if (updatedVarDefs != null) {
                snapshot.nodeVarDefsBefore.put(node, copyVarDefList(node.getVarDefList()));
                snapshot.nodeVarDefsAfter.put(node, copyVarDefList(updatedVarDefs));
                node.setVarDefList(new ArrayList<>(updatedVarDefs));
            }
            List<Command> updatedCommands = renameCommandList(node.getCmdList(), identifierPattern, newName, error);
            if (error != null && error.length() > 0) {
                applyVarRenameSnapshot(project, snapshot, false);
                return null;
            }
            if (updatedCommands != null) {
                snapshot.nodeCommandsBefore.put(node, copyCommandList(node.getCmdList()));
                snapshot.nodeCommandsAfter.put(node, copyCommandList(updatedCommands));
                node.setCmdList(new ArrayList<>(updatedCommands));
            }
            for (AbstractEdge edge : node.getEdgeList()) {
                if (edge == null) {
                    continue;
                }
                List<Command> updatedEdgeCommands = renameCommandList(edge.getCmdList(), identifierPattern, newName, error);
                if (error != null && error.length() > 0) {
                    applyVarRenameSnapshot(project, snapshot, false);
                    return null;
                }
                if (updatedEdgeCommands != null) {
                    snapshot.edgeCommandsBefore.put(edge, copyCommandList(edge.getCmdList()));
                    snapshot.edgeCommandsAfter.put(edge, copyCommandList(updatedEdgeCommands));
                    edge.setCmdList(new ArrayList<>(updatedEdgeCommands));
                }
                Expression condition = edgeCondition(edge);
                Expression updatedCondition = renameExpression(condition, identifierPattern, newName, error);
                if (error != null && error.length() > 0) {
                    applyVarRenameSnapshot(project, snapshot, false);
                    return null;
                }
                if (!Objects.equals(expressionSyntax(condition), expressionSyntax(updatedCondition))) {
                    snapshot.edgeConditionsBefore.put(edge, copyExpression(condition));
                    snapshot.edgeConditionsAfter.put(edge, copyExpression(updatedCondition));
                    applyEdgeCondition(edge, updatedCondition);
                }
                Expression timeoutExpr = edgeTimeoutExpression(edge);
                Expression updatedTimeout = renameExpression(timeoutExpr, identifierPattern, newName, error);
                if (error != null && error.length() > 0) {
                    applyVarRenameSnapshot(project, snapshot, false);
                    return null;
                }
                if (!Objects.equals(expressionSyntax(timeoutExpr), expressionSyntax(updatedTimeout))) {
                    snapshot.edgeTimeoutsBefore.put(edge, copyExpression(timeoutExpr));
                    snapshot.edgeTimeoutsAfter.put(edge, copyExpression(updatedTimeout));
                    applyEdgeTimeoutExpression(edge, updatedTimeout);
                }
            }
        }
        SceneScript script = project.getSceneScript();
        String scriptText = script.getText();
        boolean scriptRenamed = renameSceneScriptParameters(script, oldName, newName, scriptPattern);
        if (scriptRenamed) {
            snapshot.scriptBefore = scriptText;
            snapshot.scriptAfter = script.getText();
            snapshot.scriptChanged = true;
        } else {
            String updatedText = replaceVariableToken(scriptText, scriptPattern, "$" + newName);
            if (!Objects.equals(scriptText, updatedText)) {
                snapshot.scriptBefore = scriptText;
                boolean parseOk = script.parseTXT(updatedText);
                if (!parseOk) {
                    if (error != null && error.length() == 0) {
                        error.append("Scene script update failed.");
                    }
                    applyVarRenameSnapshot(project, snapshot, false);
                    return null;
                }
                snapshot.scriptAfter = script.getText();
                snapshot.scriptChanged = true;
            }
        }
        return snapshot;
    }

    private boolean renameSceneScriptParameters(SceneScript script, String oldName, String newName, Pattern scriptPattern) {
        if (script == null || oldName == null || newName == null) {
            return false;
        }
        boolean changed = false;
        for (ScriptEntity entity : script.getEntityList()) {
            if (entity instanceof SceneObject) {
                changed |= renameSceneObjectParameters((SceneObject) entity, oldName, newName, scriptPattern);
            } else if (entity instanceof ActionObject) {
                changed |= renameActionObjectParameters((ActionObject) entity, oldName, newName, scriptPattern);
            }
        }
        return changed;
    }

    private boolean renameSceneObjectParameters(SceneObject scene, String oldName, String newName, Pattern scriptPattern) {
        boolean changed = false;
        if (scene == null) {
            return false;
        }
        for (SceneTurn turn : scene.getTurnList()) {
            changed |= renameSceneTurnParameters(turn, oldName, newName, scriptPattern);
        }
        return changed;
    }

    private boolean renameSceneTurnParameters(SceneTurn turn, String oldName, String newName, Pattern scriptPattern) {
        boolean changed = false;
        if (turn == null) {
            return false;
        }
        for (SceneUttr utterance : turn.getUttrList()) {
            changed |= renameSceneUttrParameters(utterance, oldName, newName, scriptPattern);
        }
        return changed;
    }

    private boolean renameSceneUttrParameters(SceneUttr utterance, String oldName, String newName, Pattern scriptPattern) {
        boolean changed = false;
        if (utterance == null) {
            return false;
        }
        for (UttrElement element : utterance.getWordList()) {
            if (element instanceof SceneParam) {
                SceneParam param = (SceneParam) element;
                if (oldName.equals(param.getName())) {
                    param.setName(newName);
                    changed = true;
                }
            } else if (element instanceof SceneWord) {
                SceneWord word = (SceneWord) element;
                String before = word.getText();
                String after = replaceVariableToken(before, scriptPattern, "$" + newName);
                if (!Objects.equals(before, after)) {
                    word.setText(after);
                    changed = true;
                }
            } else if (element instanceof ActionObject) {
                changed |= renameActionObjectParameters((ActionObject) element, oldName, newName, scriptPattern);
            }
        }
        return changed;
    }

    private boolean renameActionObjectParameters(ActionObject action, String oldName, String newName, Pattern scriptPattern) {
        if (action == null) {
            return false;
        }
        boolean changed = false;
        for (ActionFeature feature : action.getFeatureList()) {
            if (feature instanceof ActionParam) {
                ActionParam param = (ActionParam) feature;
                if (oldName.equals(param.getVal())) {
                    param.setVal(newName);
                    changed = true;
                }
            } else {
                String before = feature.getVal();
                String after = replaceVariableToken(before, scriptPattern, "$" + newName);
                if (!Objects.equals(before, after)) {
                    feature.setVal(after);
                    changed = true;
                }
            }
        }
        return changed;
    }

    private List<BasicNode> collectSceneFlowNodes(SuperNode root) {
        List<BasicNode> nodes = new ArrayList<>();
        collectSceneFlowNodes(root, nodes);
        return nodes;
    }

    private void collectSceneFlowNodes(SuperNode node, List<BasicNode> nodes) {
        if (node == null) {
            return;
        }
        nodes.add(node);
        for (BasicNode child : node.getNodeList()) {
            nodes.add(child);
        }
        for (SuperNode child : node.getSuperNodeList()) {
            collectSceneFlowNodes(child, nodes);
        }
    }

    private List<VariableDefinition> renameVarDefList(
            List<VariableDefinition> list,
            Pattern pattern,
            String oldName,
            String newName,
            StringBuilder error) {
        if (list == null || list.isEmpty()) {
            return null;
        }
        List<VariableDefinition> updated = new ArrayList<>();
        boolean changed = false;
        for (VariableDefinition def : list) {
            if (def == null) {
                continue;
            }
            VariableDefinition copy = def.getCopy();
            if (oldName.equals(copy.getName())) {
                copy.setName(newName);
                changed = true;
            }
            Expression exp = copy.getExp();
            Expression updatedExp = renameExpression(exp, pattern, newName, error);
            if (exp != null && updatedExp == null) {
                return null;
            }
            if (!Objects.equals(expressionSyntax(exp), expressionSyntax(updatedExp))) {
                copy.setExp(updatedExp);
                changed = true;
            }
            updated.add(copy);
        }
        return changed ? updated : null;
    }

    private List<Command> renameCommandList(
            List<Command> list,
            Pattern pattern,
            String newName,
            StringBuilder error) {
        if (list == null || list.isEmpty()) {
            return null;
        }
        List<Command> updated = new ArrayList<>();
        boolean changed = false;
        for (Command cmd : list) {
            if (cmd == null) {
                continue;
            }
            String before = cmd.getConcreteSyntax();
            String after = replaceVariableToken(before, pattern, newName);
            if (Objects.equals(before, after)) {
                updated.add(cmd.getCopy());
                continue;
            }
            Command parsed = parseCommandText(after, error);
            if (parsed == null) {
                return null;
            }
            updated.add(parsed);
            changed = true;
        }
        return changed ? updated : null;
    }

    private Expression renameExpression(Expression expr, Pattern pattern, String newName, StringBuilder error) {
        if (expr == null) {
            return null;
        }
        String before = expressionSyntax(expr);
        String after = replaceVariableToken(before, pattern, newName);
        if (Objects.equals(before, after)) {
            return expr;
        }
        try {
            Object parsed = GlueParser.run(after.trim());
            if (!(parsed instanceof Expression)) {
                if (error != null && error.length() == 0) {
                    error.append("Expression parse failed.");
                }
                return null;
            }
            return (Expression) parsed;
        } catch (Exception ex) {
            if (error != null && error.length() == 0) {
                String msg = ex.getMessage();
                error.append(msg != null && !msg.isBlank() ? msg : "Expression parse failed.");
            }
            return null;
        }
    }

    private void applyEdgeCommandList(AbstractEdge edge, List<Command> list) {
        if (edge == null) {
            return;
        }
        edge.setCmdList(copyCommandList(list));
    }

    private void applyEdgeCondition(AbstractEdge edge, Expression condition) {
        if (edge instanceof GuargedEdge) {
            ((GuargedEdge) edge).setCondition(copyExpression(condition));
        } else if (edge instanceof InterruptEdge) {
            ((InterruptEdge) edge).setCondition(copyExpression(condition));
        }
    }

    private void applyEdgeTimeoutExpression(AbstractEdge edge, Expression expr) {
        if (edge instanceof TimeoutEdge) {
            ((TimeoutEdge) edge).setExpression(copyExpression(expr));
        }
    }

    private Pattern buildIdentifierPattern(String name) {
        return Pattern.compile("(?<![A-Za-z0-9_])" + Pattern.quote(name) + "(?![A-Za-z0-9_])");
    }

    private Pattern buildSceneScriptVarPattern(String name) {
        return Pattern.compile("\\$" + Pattern.quote(name) + "(?![A-Za-z0-9_])");
    }

    private String replaceVariableToken(String text, Pattern pattern, String replacement) {
        if (text == null || text.isEmpty()) {
            return text;
        }
        Matcher matcher = pattern.matcher(text);
        if (!matcher.find()) {
            return text;
        }
        return matcher.replaceAll(Matcher.quoteReplacement(replacement));
    }

    private DataTypeDefinition parseTypeDef(JSONObject source, StringBuilder error) {
        if (source == null) {
            if (error != null) {
                error.append("Missing type definition.");
            }
            return null;
        }
        String name = source.optString("name", "").trim();
        if (name.isBlank()) {
            if (error != null) {
                error.append("Type name is required.");
            }
            return null;
        }
        String flavourRaw = source.optString("flavour", "").trim();
        DataTypeDefinition.Flavour flavour;
        try {
            flavour = DataTypeDefinition.Flavour.valueOf(flavourRaw);
        } catch (Exception ex) {
            if (error != null) {
                error.append("Type flavour is required.");
            }
            return null;
        }
        if (flavour == DataTypeDefinition.Flavour.List) {
            String elementType = source.optString("elementType", "").trim();
            if (elementType.isBlank()) {
                elementType = "Int";
            }
            return new ListTypeDefinition(name, elementType);
        }
        ArrayList<MemberDefinition> members = new ArrayList<>();
        JSONArray list = source.optJSONArray("members");
        if (list != null) {
            for (int i = 0; i < list.length(); i++) {
                JSONObject entry = list.optJSONObject(i);
                if (entry == null) {
                    if (error != null) {
                        error.append("Invalid struct member.");
                    }
                    return null;
                }
                String memberName = entry.optString("name", "").trim();
                String memberType = entry.optString("type", "").trim();
                if (memberName.isBlank() || memberType.isBlank()) {
                    if (error != null) {
                        error.append("Member name and type are required.");
                    }
                    return null;
                }
                members.add(new MemberDefinition(memberName, memberType));
            }
        }
        return new StructTypeDefinition(name, members);
    }

    private VariableDefinition parseVarDef(JSONObject source, BasicNode node, StringBuilder error) {
        if (source == null) {
            if (error != null) {
                error.append("Missing variable definition.");
            }
            return null;
        }
        String name = source.optString("name", "").trim();
        if (name.isBlank()) {
            if (error != null) {
                error.append("Variable name is required.");
            }
            return null;
        }
        String type = source.optString("type", "").trim();
        if (type.isBlank()) {
            if (error != null) {
                error.append("Variable type is required.");
            }
            return null;
        }
        String expressionText = source.has("expression") ? source.optString("expression", "") : "";
        Expression exp = null;
        if (expressionText == null || expressionText.trim().isEmpty()) {
            exp = defaultExpressionForType(type, node);
            if (exp == null) {
                if (error != null) {
                    error.append("Expression is required.");
                }
                return null;
            }
        } else {
            Command parsed;
            try {
                parsed = GlueParser.run(expressionText.trim());
            } catch (Exception ex) {
                if (error != null) {
                    String msg = ex.getMessage();
                    error.append(msg != null && !msg.isBlank() ? msg : "Expression parse failed.");
                }
                return null;
            }
            if (!(parsed instanceof Expression)) {
                if (error != null) {
                    error.append("Expression parse failed.");
                }
                return null;
            }
            exp = (Expression) parsed;
        }
        return new VariableDefinition(name, type, exp);
    }

    private Command parseCommandText(String input, StringBuilder error) {
        String text = input == null ? "" : input.trim();
        if (text.isEmpty()) {
            if (error != null) {
                error.append("Command text is required.");
            }
            return null;
        }
        Command parsed;
        try {
            parsed = GlueParser.run(text);
        } catch (Exception ex) {
            if (error != null) {
                String msg = ex.getMessage();
                error.append(msg != null && !msg.isBlank() ? msg : "Command parse failed.");
            }
            return null;
        }
        if (parsed == null) {
            if (error != null) {
                error.append("Command parse failed.");
            }
            return null;
        }
        return parsed;
    }

    private Expression defaultExpressionForType(String type, BasicNode node) {
        if (type == null) {
            return null;
        }
        String trimmed = type.trim();
        if (trimmed.equalsIgnoreCase("Int")) {
            return new IntLiteral(0);
        }
        if (trimmed.equalsIgnoreCase("Bool")) {
            return new BoolLiteral(true);
        }
        if (trimmed.equalsIgnoreCase("Float")) {
            return new FloatLiteral(0);
        }
        if (trimmed.equalsIgnoreCase("String")) {
            return new StringLiteral("");
        }
        DataTypeDefinition def = findTypeDefInHierarchy(node, trimmed);
        if (def instanceof ListTypeDefinition) {
            return new ArrayExpression();
        }
        if (def instanceof StructTypeDefinition) {
            return new StructExpression();
        }
        return null;
    }

    private DataTypeDefinition findTypeDefInHierarchy(BasicNode node, String name) {
        if (node == null || name == null) {
            return null;
        }
        BasicNode current = node;
        while (current != null) {
            for (DataTypeDefinition def : current.getTypeDefList()) {
                if (def != null && name.equals(def.getName())) {
                    return def;
                }
            }
            current = current.getParentNode();
        }
        return null;
    }

    private boolean isIntVarDefined(SuperNode active, String name) {
        if (active == null || name == null || name.isBlank()) {
            return false;
        }
        for (VariableDefinition def : active.getVarDefList()) {
            if (def == null) {
                continue;
            }
            String defName = def.getName();
            String defType = def.getType();
            if (defName == null || defType == null) {
                continue;
            }
            if (name.equals(defName) && "int".equalsIgnoreCase(defType.trim())) {
                return true;
            }
        }
        return false;
    }

    private Expression edgeCondition(AbstractEdge dataEdge) {
        if (dataEdge instanceof GuargedEdge) {
            return ((GuargedEdge) dataEdge).getCondition();
        }
        if (dataEdge instanceof InterruptEdge) {
            return ((InterruptEdge) dataEdge).getCondition();
        }
        return null;
    }

    private Expression edgeTimeoutExpression(AbstractEdge dataEdge) {
        if (dataEdge instanceof TimeoutEdge) {
            return ((TimeoutEdge) dataEdge).getExpression();
        }
        return null;
    }

    private Expression copyExpression(Expression expr) {
        return expr != null ? expr.getCopy() : null;
    }

    private Integer edgeProbability(AbstractEdge dataEdge) {
        if (dataEdge instanceof RandomEdge) {
            return ((RandomEdge) dataEdge).getProbability();
        }
        return null;
    }

    private int normalizeProbabilityValue(int probability) {
        return probability == Integer.MIN_VALUE ? 0 : probability;
    }

    private BasicNode resolveEdgeSourceNode(AbstractEdge dataEdge, SuperNode active) {
        if (dataEdge == null) {
            return null;
        }
        BasicNode sourceNode = dataEdge.getSourceNode();
        if (sourceNode == null && active != null && dataEdge.getSourceUnid() != null) {
            sourceNode = active.getChildNodeById(dataEdge.getSourceUnid());
        }
        return sourceNode;
    }

    private int sumProbabilities(BasicNode sourceNode, AbstractEdge exclude) {
        if (sourceNode == null) {
            return 0;
        }
        int sum = 0;
        for (RandomEdge edge : sourceNode.getPEdgeList()) {
            if (edge == null || edge == exclude) {
                continue;
            }
            sum += normalizeProbabilityValue(edge.getProbability());
        }
        return sum;
    }

    private void applyPEdgeProbabilities(WorkSpacePanel workSpace, Map<RandomEdge, Integer> values) {
        if (values == null || values.isEmpty()) {
            return;
        }
        for (Map.Entry<RandomEdge, Integer> entry : values.entrySet()) {
            RandomEdge edge = entry.getKey();
            if (edge == null) {
                continue;
            }
            int probability = entry.getValue() != null ? entry.getValue() : 0;
            edge.setProbability(probability);
        }
        if (workSpace != null) {
            for (RandomEdge edge : values.keySet()) {
                Edge guiEdge = findGuiEdgeByData(workSpace, edge);
                if (guiEdge != null) {
                    guiEdge.update();
                    guiEdge.repaint(100);
                }
            }
            workSpace.revalidate();
            workSpace.repaint(100);
        }
    }

    private Long edgeTimeout(AbstractEdge dataEdge) {
        if (dataEdge instanceof TimeoutEdge) {
            return ((TimeoutEdge) dataEdge).getTimeout();
        }
        return null;
    }

    private String edgeConditionSyntax(Expression expr) {
        return expressionSyntax(expr);
    }

    private String expressionSyntax(Expression expr) {
        if (expr == null) {
            return "";
        }
        String syntax = expr.getConcreteSyntax();
        return syntax == null ? "" : syntax;
    }

    private Map<Tuple<String, BasicNode>, Tuple<String, BasicNode>> copyAltStartMap(
            Map<Tuple<String, BasicNode>, Tuple<String, BasicNode>> original) {
        Map<Tuple<String, BasicNode>, Tuple<String, BasicNode>> copy = new LinkedHashMap<>();
        if (original == null) {
            return copy;
        }
        for (Map.Entry<Tuple<String, BasicNode>, Tuple<String, BasicNode>> entry : original.entrySet()) {
            Tuple<String, BasicNode> start = entry.getKey();
            Tuple<String, BasicNode> alt = entry.getValue();
            String startId = start != null ? start.getFirst() : "";
            BasicNode startNode = start != null ? start.getSecond() : null;
            String altId = alt != null ? alt.getFirst() : "";
            BasicNode altNode = alt != null ? alt.getSecond() : null;
            copy.put(new Tuple<>(startId, startNode), new Tuple<>(altId, altNode));
        }
        return copy;
    }

    private String altMapSignature(Map<Tuple<String, BasicNode>, Tuple<String, BasicNode>> map) {
        if (map == null || map.isEmpty()) {
            return "";
        }
        List<String> entries = new ArrayList<>();
        for (Map.Entry<Tuple<String, BasicNode>, Tuple<String, BasicNode>> entry : map.entrySet()) {
            String startId = entry.getKey() != null ? entry.getKey().getFirst() : "";
            String altId = entry.getValue() != null ? entry.getValue().getFirst() : "";
            entries.add(startId + "/" + altId);
        }
        entries.sort(Comparator.naturalOrder());
        return String.join(";", entries);
    }

    private List<EdgePoint> copyEdgePoints(EdgeArrow arrow) {
        List<EdgePoint> copy = new ArrayList<>();
        if (arrow == null || arrow.getPointList() == null) {
            return copy;
        }
        for (EdgePoint point : arrow.getPointList()) {
            if (point != null) {
                copy.add(point.getCopy());
            }
        }
        return copy;
    }

    private Map<AbstractEdge, List<EdgePoint>> captureEdgePointState(WorkSpacePanel workSpace) {
        Map<AbstractEdge, List<EdgePoint>> state = new LinkedHashMap<>();
        if (workSpace == null) {
            return state;
        }
        for (Edge edge : workSpace.getEdges()) {
            if (edge == null) {
                continue;
            }
            AbstractEdge dataEdge = edge.getDataEdge();
            if (dataEdge == null) {
                continue;
            }
            if (edge.mEg != null) {
                edge.mEg.updateDrawingParameters();
            }
            EdgeGraphics graphics = dataEdge.getGraphics();
            EdgeArrow arrow = graphics != null ? graphics.getConnection() : null;
            state.put(dataEdge, copyEdgePoints(arrow));
        }
        return state;
    }

    private Map<AbstractEdge, List<EdgePoint>> captureEdgePointState(List<Edge> guiEdges) {
        Map<AbstractEdge, List<EdgePoint>> state = new LinkedHashMap<>();
        if (guiEdges == null || guiEdges.isEmpty()) {
            return state;
        }
        for (Edge edge : guiEdges) {
            if (edge == null) {
                continue;
            }
            AbstractEdge dataEdge = edge.getDataEdge();
            if (dataEdge == null) {
                continue;
            }
            if (edge.mEg != null) {
                edge.mEg.updateDrawingParameters();
            }
            EdgeGraphics graphics = dataEdge.getGraphics();
            EdgeArrow arrow = graphics != null ? graphics.getConnection() : null;
            state.put(dataEdge, copyEdgePoints(arrow));
        }
        return state;
    }

    private void straightenEdgeGroup(WorkSpacePanel workSpace, List<Edge> guiEdges) {
        if (workSpace == null || guiEdges == null || guiEdges.isEmpty()) {
            return;
        }
        EditorConfig config = workSpace.getEditorConfig();
        double spacing = Math.max(10, config != null ? config.sNODEWIDTH * 0.12 : 12);
        Map<String, List<Edge>> grouped = new LinkedHashMap<>();
        for (Edge edge : guiEdges) {
            if (edge == null) {
                continue;
            }
            Node source = edge.getSourceNode();
            Node target = edge.getTargetNode();
            String sourceId = source != null && source.getDataNode() != null ? source.getDataNode().getId() : null;
            String targetId = target != null && target.getDataNode() != null ? target.getDataNode().getId() : null;
            if (sourceId == null || targetId == null) {
                continue;
            }
            String key = sourceId + "->" + targetId;
            grouped.computeIfAbsent(key, k -> new ArrayList<>()).add(edge);
        }
        for (List<Edge> group : grouped.values()) {
            if (group.isEmpty()) {
                continue;
            }
            for (Edge edge : group) {
                edge.straightenEdge();
                if (edge.mEg != null) {
                    edge.mEg.updateDrawingParameters();
                }
                edge.update();
            }
            if (group.size() == 1) {
                continue;
            }
            Edge sample = group.get(0);
            de.dfki.vsm.editor.util.EdgeGraphics graphics = sample.mEg;
            Point start = graphics != null ? graphics.mAbsoluteStartPos : null;
            Point end = graphics != null ? graphics.mAbsoluteEndPos : null;
            double dx = start != null && end != null ? end.x - start.x : 0;
            double dy = start != null && end != null ? end.y - start.y : 0;
            double length = Math.hypot(dx, dy);
            double perpX = 0;
            double perpY = -1;
            if (Double.isFinite(length) && length > 0.001) {
                perpX = -dy / length;
                perpY = dx / length;
            }
            double midIndex = (group.size() - 1) / 2.0;
            for (int i = 0; i < group.size(); i++) {
                Edge edge = group.get(i);
                de.dfki.vsm.editor.util.EdgeGraphics eg = edge.mEg;
                if (eg == null) {
                    continue;
                }
                double offset = (i - midIndex) * spacing;
                if (Math.abs(offset) < 0.01) {
                    continue;
                }
                int shiftX = (int) Math.round(perpX * offset);
                int shiftY = (int) Math.round(perpY * offset);
                eg.mCCrtl1.translate(shiftX, shiftY);
                eg.mCCrtl2.translate(shiftX, shiftY);
                eg.updateDrawingParameters();
                edge.update();
            }
        }
    }

    private void normalizeEdgeGroup(WorkSpacePanel workSpace, List<Edge> guiEdges) {
        if (workSpace == null || guiEdges == null || guiEdges.isEmpty()) {
            return;
        }
        for (Edge edge : guiEdges) {
            if (edge == null) {
                continue;
            }
            edge.rebuildEdgeNicely();
            if (edge.mEg != null) {
                edge.mEg.updateDrawingParameters();
            }
            edge.update();
        }
    }

    private Map<AbstractEdge, List<EdgePoint>> copyEdgePointState(Map<AbstractEdge, List<EdgePoint>> original) {
        Map<AbstractEdge, List<EdgePoint>> copy = new LinkedHashMap<>();
        if (original == null) {
            return copy;
        }
        for (Map.Entry<AbstractEdge, List<EdgePoint>> entry : original.entrySet()) {
            List<EdgePoint> points = new ArrayList<>();
            if (entry.getValue() != null) {
                for (EdgePoint point : entry.getValue()) {
                    if (point != null) {
                        points.add(point.getCopy());
                    }
                }
            }
            copy.put(entry.getKey(), points);
        }
        return copy;
    }

    private boolean edgePointStateEquals(
            Map<AbstractEdge, List<EdgePoint>> left,
            Map<AbstractEdge, List<EdgePoint>> right) {
        if (left == null || right == null) {
            return left == right;
        }
        if (left.size() != right.size()) {
            return false;
        }
        for (Map.Entry<AbstractEdge, List<EdgePoint>> entry : left.entrySet()) {
            List<EdgePoint> other = right.get(entry.getKey());
            if (other == null) {
                return false;
            }
            if (!edgePointsSignature(entry.getValue()).equals(edgePointsSignature(other))) {
                return false;
            }
        }
        return true;
    }

    private void applyEdgePointState(Map<AbstractEdge, List<EdgePoint>> state, WorkSpacePanel workSpace) {
        if (state == null || state.isEmpty()) {
            return;
        }
        for (Map.Entry<AbstractEdge, List<EdgePoint>> entry : state.entrySet()) {
            applyEdgePointState(entry.getKey(), entry.getValue(), workSpace);
        }
    }

    private String edgePointsSignature(List<EdgePoint> points) {
        if (points == null || points.isEmpty()) {
            return "";
        }
        StringBuilder sb = new StringBuilder();
        for (EdgePoint point : points) {
            if (point == null) {
                continue;
            }
            sb.append(point.getXPos())
                    .append(',')
                    .append(point.getYPos())
                    .append(',')
                    .append(point.getCtrlXPos())
                    .append(',')
                    .append(point.getCtrlYPos())
                    .append(';');
        }
        return sb.toString();
    }

    private boolean edgePointMatches(Point point, EdgePoint edgePoint) {
        if (point == null || edgePoint == null) {
            return false;
        }
        return Math.abs(point.x - edgePoint.getXPos()) <= 1
                && Math.abs(point.y - edgePoint.getYPos()) <= 1;
    }

    private Point edgeEndPoint(List<EdgePoint> points) {
        if (points == null || points.isEmpty()) {
            return null;
        }
        EdgePoint point = points.get(points.size() - 1);
        if (point == null) {
            return null;
        }
        return new Point(point.getXPos(), point.getYPos());
    }

    private List<EdgePoint> parseEdgePoints(JSONArray list) {
        if (list == null) {
            return null;
        }
        List<EdgePoint> points = new ArrayList<>();
        for (int i = 0; i < list.length(); i++) {
            JSONObject entry = list.optJSONObject(i);
            if (entry == null) {
                return null;
            }
            double x = entry.has("x") ? entry.optDouble("x", 0) : 0;
            double y = entry.has("y") ? entry.optDouble("y", 0) : 0;
            double cx = entry.has("cx") ? entry.optDouble("cx", x) : x;
            double cy = entry.has("cy") ? entry.optDouble("cy", y) : y;
            EdgePoint point = new EdgePoint(
                    (int) Math.round(x),
                    (int) Math.round(cx),
                    (int) Math.round(y),
                    (int) Math.round(cy)
            );
            points.add(point);
        }
        return points;
    }

    private void applyEdgePointState(AbstractEdge dataEdge, List<EdgePoint> points, WorkSpacePanel workSpace) {
        if (dataEdge == null || points == null || points.isEmpty()) {
            return;
        }
        EdgeGraphics graphics = dataEdge.getGraphics();
        if (graphics == null) {
            graphics = new EdgeGraphics();
            dataEdge.setGraphics(graphics);
        }
        EdgeArrow arrow = graphics.getConnection();
        if (arrow == null) {
            arrow = new EdgeArrow();
            graphics.setConnection(arrow);
        }
        ArrayList<EdgePoint> stored = new ArrayList<>();
        for (EdgePoint point : points) {
            if (point != null) {
                stored.add(point.getCopy());
            }
        }
        arrow.setPointList(stored);
        if (workSpace != null) {
            Edge guiEdge = findGuiEdgeByData(workSpace, dataEdge);
            if (guiEdge != null && guiEdge.mEg != null && stored.size() >= 2) {
                EdgePoint start = stored.get(0);
                EdgePoint end = stored.get(stored.size() - 1);
                Node sourceNode = guiEdge.getSourceNode();
                Node targetNode = guiEdge.getTargetNode();
                boolean sameNode = sourceNode != null && sourceNode.equals(targetNode);
                if (sourceNode != null && !edgePointMatches(sourceNode.getEdgeDockPoint(guiEdge), start)) {
                    sourceNode.disconnectEdge(guiEdge);
                    Point newStart = sourceNode.connectEdgeAtSourceNode(
                            guiEdge,
                            new Point(start.getXPos(), start.getYPos())
                    );
                    if (newStart != null) {
                        guiEdge.mEg.mAbsoluteStartPos.setLocation(newStart);
                    }
                }
                if (targetNode != null) {
                    Point currentTarget = sameNode
                            ? targetNode.getSelfPointingEdgeDockPoint(guiEdge)
                            : targetNode.getEdgeDockPoint(guiEdge);
                    if (!edgePointMatches(currentTarget, end)) {
                        if (sameNode) {
                            targetNode.disconnectSelfPointingEdge(guiEdge);
                            Point newEnd = targetNode.connectSelfPointingEdge(
                                    guiEdge,
                                    new Point(end.getXPos(), end.getYPos())
                            );
                            if (newEnd != null) {
                                guiEdge.mEg.mAbsoluteEndPos.setLocation(newEnd);
                            }
                        } else {
                            targetNode.disconnectEdge(guiEdge);
                            Point newEnd = targetNode.connectEdgetAtTargetNode(
                                    guiEdge,
                                    new Point(end.getXPos(), end.getYPos())
                            );
                            if (newEnd != null) {
                                guiEdge.mEg.mAbsoluteEndPos.setLocation(newEnd);
                            }
                        }
                    }
                }
                guiEdge.mEg.mCCrtl1.setLocation(start.getCtrlXPos(), start.getCtrlYPos());
                guiEdge.mEg.mCCrtl2.setLocation(end.getCtrlXPos(), end.getCtrlYPos());
                guiEdge.mEg.updateDrawingParameters();
                guiEdge.repaint(100);
            }
            workSpace.revalidate();
            workSpace.repaint(100);
        }
    }

    private Edge retargetEdge(
            WorkSpacePanel workSpace,
            AbstractEdge dataEdge,
            Node sourceNode,
            Node targetNode,
            Point dropPoint) {
        if (workSpace == null || dataEdge == null || sourceNode == null || targetNode == null) {
            return null;
        }
        Edge guiEdge = findGuiEdgeByData(workSpace, dataEdge);
        if (guiEdge == null) {
            return null;
        }
        Node oldTarget = guiEdge.getTargetNode();
        boolean sameNode = sourceNode.equals(targetNode);
        Point sourceDock = sourceNode.disconnectEdge(guiEdge);
        if (oldTarget != null) {
            if (sourceNode.equals(oldTarget)) {
                oldTarget.disconnectSelfPointingEdge(guiEdge);
            } else {
                oldTarget.disconnectEdge(guiEdge);
            }
        }
        workSpace.remove(guiEdge);

        if (sourceNode.getDataNode() != null) {
            dataEdge.setSourceNode(sourceNode.getDataNode());
            dataEdge.setSourceUnid(sourceNode.getDataNode().getId());
        }
        if (targetNode.getDataNode() != null) {
            dataEdge.setTargetNode(targetNode.getDataNode());
            dataEdge.setTargetUnid(targetNode.getDataNode().getId());
        }
        dataEdge.setGraphics(null);

        Edge.TYPE guiType = parseEdgeCreateType(edgeType(dataEdge));
        Point targetDrop = sameNode ? null : dropPoint;
        Point sourceDockPoint = sameNode ? null : sourceDock;
        Edge newEdge = new Edge(workSpace, dataEdge, guiType, sourceNode, targetNode, sourceDockPoint, targetDrop);
        workSpace.add(newEdge);
        newEdge.straightenEdge();
        newEdge.update();
        newEdge.repaint(100);
        workSpace.revalidate();
        workSpace.repaint(100);
        return newEdge;
    }

    private void applyEdgeState(
            AbstractEdge dataEdge,
            Expression condition,
            Integer probability,
            Long timeout,
            Expression timeoutExpr,
            Map<Tuple<String, BasicNode>, Tuple<String, BasicNode>> altMap,
            WorkSpacePanel workSpace) {
        if (dataEdge == null) {
            return;
        }
        if (dataEdge instanceof GuargedEdge && condition != null) {
            ((GuargedEdge) dataEdge).setCondition(condition);
        }
        if (dataEdge instanceof InterruptEdge && condition != null) {
            ((InterruptEdge) dataEdge).setCondition(condition);
        }
        if (dataEdge instanceof RandomEdge && probability != null) {
            ((RandomEdge) dataEdge).setProbability(probability);
        }
        if (dataEdge instanceof TimeoutEdge) {
            TimeoutEdge timeoutEdge = (TimeoutEdge) dataEdge;
            if (timeout != null && timeout >= 0) {
                try {
                    timeoutEdge.setTimeout(timeout);
                } catch (NumberFormatException ex) {
                    mLogger.warning("Invalid timeout during undo/redo: " + ex.getMessage());
                }
            }
            timeoutEdge.setExpression(timeoutExpr != null ? timeoutExpr.getCopy() : null);
        }
        if (altMap != null) {
            dataEdge.setAltMap(copyAltStartMap(altMap));
        }
        if (workSpace != null) {
            Edge guiEdge = findGuiEdgeByData(workSpace, dataEdge);
            if (guiEdge != null) {
                guiEdge.update();
                guiEdge.repaint(100);
            }
            workSpace.revalidate();
            workSpace.repaint(100);
        }
    }

    private String applyEdgeUpdates(AbstractEdge dataEdge, SuperNode active, JSONObject fields, WorkSpacePanel workSpace) {
        if (dataEdge == null || fields == null) {
            return null;
        }
        if (fields.has("condition")) {
            if (!(dataEdge instanceof GuargedEdge) && !(dataEdge instanceof InterruptEdge)) {
                return "Condition is not supported for this edge type.";
            }
            String input = fields.optString("condition", "").trim();
            if (input.isEmpty()) {
                return "Condition is required.";
            }
            Expression expr;
            try {
                Command parsed = GlueParser.run(input);
                if (!(parsed instanceof Expression)) {
                    return "Condition parse failed.";
                }
                expr = (Expression) parsed;
            } catch (Exception ex) {
                return "Condition parse failed.";
            }
            if (dataEdge instanceof GuargedEdge) {
                ((GuargedEdge) dataEdge).setCondition(expr);
            } else {
                ((InterruptEdge) dataEdge).setCondition(expr);
            }
        }
        if (fields.has("probability")) {
            if (!(dataEdge instanceof RandomEdge)) {
                return "Probability is not supported for this edge type.";
            }
            Object raw = fields.get("probability");
            int probability;
            try {
                probability = Integer.parseInt(String.valueOf(raw));
            } catch (NumberFormatException ex) {
                return "Probability must be a number.";
            }
            if (probability < 0 || probability > 100) {
                return "Probability must be between 0 and 100.";
            }
            BasicNode sourceNode = resolveEdgeSourceNode(dataEdge, active);
            if (sourceNode != null) {
                int sumOther = sumProbabilities(sourceNode, dataEdge);
                int total = sumOther + probability;
                if (total != 100) {
                    int remaining = Math.max(0, 100 - sumOther);
                    return "Total probability must be 100%. Remaining: " + remaining + ".";
                }
            }
            ((RandomEdge) dataEdge).setProbability(probability);
        }
        if (fields.has("timeoutExpr")) {
            if (!(dataEdge instanceof TimeoutEdge)) {
                return "Timeout is not supported for this edge type.";
            }
            String input = fields.optString("timeoutExpr", "").trim();
            TimeoutEdge timeoutEdge = (TimeoutEdge) dataEdge;
            if (input.isEmpty()) {
                timeoutEdge.setExpression(null);
            } else {
                Command parsed;
                try {
                    parsed = GlueParser.run(input);
                } catch (Exception ex) {
                    return "Timeout expression parse failed.";
                }
                if (!(parsed instanceof Expression)) {
                    return "Timeout expression parse failed.";
                }
                if (!(parsed instanceof SimpleVariable)) {
                    return "Timeout expression must be an integer variable.";
                }
                String varName = ((SimpleVariable) parsed).getName();
                if (!isIntVarDefined(active, varName)) {
                    return "Timeout expression must be an integer sceneflow variable.";
                }
                timeoutEdge.setExpression((Expression) parsed);
            }
        }
        if (fields.has("timeoutMs")) {
            if (!(dataEdge instanceof TimeoutEdge)) {
                return "Timeout is not supported for this edge type.";
            }
            Object raw = fields.get("timeoutMs");
            long timeout;
            try {
                timeout = Long.parseLong(String.valueOf(raw));
            } catch (NumberFormatException ex) {
                return "Timeout must be a number.";
            }
            if (timeout < 0) {
                return "Timeout must be >= 0.";
            }
            try {
                ((TimeoutEdge) dataEdge).setTimeout(timeout);
            } catch (NumberFormatException ex) {
                return ex.getMessage();
            }
        }
        if (fields.has("points")) {
            JSONArray list = fields.optJSONArray("points");
            List<EdgePoint> points = parseEdgePoints(list);
            if (points == null || points.size() < 2) {
                return "Invalid edge control points.";
            }
            applyEdgePointState(dataEdge, points, workSpace);
        }
        if (fields.has("altStartMap")) {
            JSONArray list = fields.optJSONArray("altStartMap");
            if (list == null) {
                return "Alt start map must be a list.";
            }
            BasicNode targetNode = dataEdge.getTargetNode();
            if (targetNode == null && dataEdge.getTargetUnid() != null && active != null) {
                targetNode = active.getChildNodeById(dataEdge.getTargetUnid());
            }
            if (!(targetNode instanceof SuperNode)) {
                return "Alt start nodes require a super node target.";
            }
            SuperNode targetSuper = (SuperNode) targetNode;
            Map<Tuple<String, BasicNode>, Tuple<String, BasicNode>> altMap = new LinkedHashMap<>();
            Set<String> seen = new LinkedHashSet<>();
            for (int i = 0; i < list.length(); i++) {
                JSONObject entry = list.optJSONObject(i);
                if (entry == null) {
                    return "Alt start map entries must be objects.";
                }
                String startId = entry.optString("startId", "").trim();
                String altStartId = entry.optString("altStartId", "").trim();
                if (startId.isEmpty() || altStartId.isEmpty()) {
                    return "Alt start nodes require startId and altStartId.";
                }
                if (!seen.add(startId)) {
                    return "Duplicate alt start mapping for " + startId + ".";
                }
                BasicNode startNode = targetSuper.getChildNodeById(startId);
                if (startNode == null) {
                    return "Unknown start node: " + startId;
                }
                BasicNode altNode = targetSuper.getChildNodeById(altStartId);
                if (altNode == null) {
                    return "Unknown alt start node: " + altStartId;
                }
                altMap.put(new Tuple<>(startId, startNode), new Tuple<>(altStartId, altNode));
            }
            dataEdge.setAltMap(altMap);
        }
        return null;
    }

    // Phase 6: Headless version of applyEdgeUpdates (no WorkSpacePanel)
    private String applyEdgeUpdatesHeadless(AbstractEdge dataEdge, SuperNode active, JSONObject fields, EditorConfig config) {
        if (dataEdge == null || fields == null) {
            return null;
        }
        if (fields.has("condition")) {
            if (!(dataEdge instanceof GuargedEdge) && !(dataEdge instanceof InterruptEdge)) {
                return "Condition is not supported for this edge type.";
            }
            String input = fields.optString("condition", "").trim();
            if (input.isEmpty()) {
                return "Condition is required.";
            }
            Expression expr;
            try {
                Command parsed = GlueParser.run(input);
                if (!(parsed instanceof Expression)) {
                    return "Condition parse failed.";
                }
                expr = (Expression) parsed;
            } catch (Exception ex) {
                return "Condition parse failed.";
            }
            if (dataEdge instanceof GuargedEdge) {
                ((GuargedEdge) dataEdge).setCondition(expr);
            } else {
                ((InterruptEdge) dataEdge).setCondition(expr);
            }
        }
        if (fields.has("probability")) {
            if (!(dataEdge instanceof RandomEdge)) {
                return "Probability is not supported for this edge type.";
            }
            Object raw = fields.get("probability");
            int probability;
            try {
                probability = Integer.parseInt(String.valueOf(raw));
            } catch (NumberFormatException ex) {
                return "Probability must be a number.";
            }
            if (probability < 0 || probability > 100) {
                return "Probability must be between 0 and 100.";
            }
            BasicNode sourceNode = resolveEdgeSourceNode(dataEdge, active);
            if (sourceNode != null) {
                int sumOther = sumProbabilities(sourceNode, dataEdge);
                int total = sumOther + probability;
                if (total != 100) {
                    int remaining = Math.max(0, 100 - sumOther);
                    return "Total probability must be 100%. Remaining: " + remaining + ".";
                }
            }
            ((RandomEdge) dataEdge).setProbability(probability);
        }
        if (fields.has("timeoutExpr")) {
            if (!(dataEdge instanceof TimeoutEdge)) {
                return "Timeout is not supported for this edge type.";
            }
            String input = fields.optString("timeoutExpr", "").trim();
            TimeoutEdge timeoutEdge = (TimeoutEdge) dataEdge;
            if (input.isEmpty()) {
                timeoutEdge.setExpression(null);
            } else {
                Command parsed;
                try {
                    parsed = GlueParser.run(input);
                } catch (Exception ex) {
                    return "Timeout expression parse failed.";
                }
                if (!(parsed instanceof Expression)) {
                    return "Timeout expression parse failed.";
                }
                if (!(parsed instanceof SimpleVariable)) {
                    return "Timeout expression must be an integer variable.";
                }
                String varName = ((SimpleVariable) parsed).getName();
                if (!isIntVarDefined(active, varName)) {
                    return "Timeout expression must be an integer sceneflow variable.";
                }
                timeoutEdge.setExpression((Expression) parsed);
            }
        }
        if (fields.has("timeoutMs")) {
            if (!(dataEdge instanceof TimeoutEdge)) {
                return "Timeout is not supported for this edge type.";
            }
            Object raw = fields.get("timeoutMs");
            long timeout;
            try {
                timeout = Long.parseLong(String.valueOf(raw));
            } catch (NumberFormatException ex) {
                return "Timeout must be a number.";
            }
            if (timeout < 0) {
                return "Timeout must be >= 0.";
            }
            try {
                ((TimeoutEdge) dataEdge).setTimeout(timeout);
            } catch (NumberFormatException ex) {
                return ex.getMessage();
            }
        }
        if (fields.has("points")) {
            JSONArray list = fields.optJSONArray("points");
            List<EdgePoint> points = parseEdgePoints(list);
            if (points == null || points.size() < 2) {
                return "Invalid edge control points.";
            }
            // Log edge point updates for self-loop debugging
            String srcId = dataEdge.getSourceUnid();
            String tgtId = dataEdge.getTargetUnid();
            if (srcId != null && srcId.equals(tgtId) && points.size() >= 2) {
                EdgePoint startPt = points.get(0);
                EdgePoint endPt = points.get(points.size() - 1);
                System.out.println(">>> Edge.Update self-loop points: start=(" + startPt.getXPos() + "," + startPt.getYPos() +
                    ") startCtrl=(" + startPt.getCtrlXPos() + "," + startPt.getCtrlYPos() +
                    ") end=(" + endPt.getXPos() + "," + endPt.getYPos() +
                    ") endCtrl=(" + endPt.getCtrlXPos() + "," + endPt.getCtrlYPos() + ")");
            }
            applyEdgePointStateHeadless(dataEdge, points, config);
        }
        if (fields.has("altStartMap")) {
            JSONArray list = fields.optJSONArray("altStartMap");
            if (list == null) {
                return "Alt start map must be a list.";
            }
            BasicNode targetNode = dataEdge.getTargetNode();
            if (targetNode == null && dataEdge.getTargetUnid() != null && active != null) {
                targetNode = active.getChildNodeById(dataEdge.getTargetUnid());
            }
            if (!(targetNode instanceof SuperNode)) {
                return "Alt start nodes require a super node target.";
            }
            SuperNode targetSuper = (SuperNode) targetNode;
            Map<Tuple<String, BasicNode>, Tuple<String, BasicNode>> altMap = new LinkedHashMap<>();
            Set<String> seen = new LinkedHashSet<>();
            for (int i = 0; i < list.length(); i++) {
                JSONObject entry = list.optJSONObject(i);
                if (entry == null) {
                    return "Alt start map entries must be objects.";
                }
                String startId = entry.optString("startId", "").trim();
                String altStartId = entry.optString("altStartId", "").trim();
                if (startId.isEmpty() || altStartId.isEmpty()) {
                    return "Alt start nodes require startId and altStartId.";
                }
                if (!seen.add(startId)) {
                    return "Duplicate alt start mapping for " + startId + ".";
                }
                BasicNode startNode = targetSuper.getChildNodeById(startId);
                if (startNode == null) {
                    return "Unknown start node: " + startId;
                }
                BasicNode altNode = targetSuper.getChildNodeById(altStartId);
                if (altNode == null) {
                    return "Unknown alt start node: " + altStartId;
                }
                altMap.put(new Tuple<>(startId, startNode), new Tuple<>(altStartId, altNode));
            }
            dataEdge.setAltMap(altMap);
        }
        return null;
    }

    // Phase 6: Headless version of applyEdgePointState (no WorkSpacePanel)
    // Handles dock point snapping when edge endpoints are moved
    private void applyEdgePointStateHeadless(AbstractEdge dataEdge, List<EdgePoint> points, EditorConfig config) {
        if (dataEdge == null || points == null || points.size() < 2) {
            return;
        }

        EdgeGraphics graphics = dataEdge.getGraphics();
        if (graphics == null) {
            graphics = new EdgeGraphics();
            dataEdge.setGraphics(graphics);
        }
        EdgeArrow arrow = graphics.getConnection();
        if (arrow == null) {
            arrow = new EdgeArrow();
            graphics.setConnection(arrow);
        }

        // Get node dimensions
        int nodeWidth = config != null ? config.sNODEWIDTH : 90;
        int nodeHeight = config != null ? config.sNODEHEIGHT : 90;

        // Get source and target nodes
        BasicNode sourceNode = dataEdge.getSourceNode();
        BasicNode targetNode = dataEdge.getTargetNode();

        // Get current (old) edge points for dock point comparison
        List<EdgePoint> oldPoints = arrow.getPointList();
        EdgePoint oldStartPt = (oldPoints != null && !oldPoints.isEmpty()) ? oldPoints.get(0) : null;
        EdgePoint oldEndPt = (oldPoints != null && oldPoints.size() >= 2) ? oldPoints.get(oldPoints.size() - 1) : null;

        // Get new points from input
        EdgePoint newStartPt = points.get(0);
        EdgePoint newEndPt = points.get(points.size() - 1);

        // Process source node dock point (start of edge)
        if (sourceNode != null) {
            NodeGraphics srcGraphics = sourceNode.getGraphics();
            NodePosition srcPos = srcGraphics != null ? srcGraphics.getPosition() : null;
            double srcX = srcPos != null ? srcPos.getXPos() : 0;
            double srcY = srcPos != null ? srcPos.getYPos() : 0;
            boolean srcIsSuperNode = sourceNode instanceof SuperNode;
            String srcId = sourceNode.getId();

            // Find old dock point index
            int oldSrcDock = -1;
            if (oldStartPt != null) {
                oldSrcDock = findDockPointIndex(srcX, srcY, nodeWidth, nodeHeight, srcIsSuperNode,
                        oldStartPt.getXPos(), oldStartPt.getYPos());
            }

            // Check if endpoint position changed (not just control point)
            boolean srcEndpointMoved = oldStartPt == null ||
                    Math.abs(newStartPt.getXPos() - oldStartPt.getXPos()) > 5 ||
                    Math.abs(newStartPt.getYPos() - oldStartPt.getYPos()) > 5;

            if (srcEndpointMoved) {
                // Find nearest available dock point to new position
                int newSrcDock = findNearestAvailableDockPoint(srcId, srcX, srcY, nodeWidth, nodeHeight,
                        srcIsSuperNode, newStartPt.getXPos(), newStartPt.getYPos());

                if (newSrcDock >= 0 && newSrcDock != oldSrcDock) {
                    // Release old dock point
                    if (oldSrcDock >= 0) {
                        releaseDockPoint(srcId, oldSrcDock, true);
                    }
                    // Occupy new dock point
                    occupyDockPoint(srcId, newSrcDock, true);

                    // Snap endpoint to dock position, preserve control point offset
                    double[] dockPos = getDockPointPosition(srcX, srcY, nodeWidth, nodeHeight, srcIsSuperNode, newSrcDock);
                    int ctrlOffsetX = newStartPt.getCtrlXPos() - newStartPt.getXPos();
                    int ctrlOffsetY = newStartPt.getCtrlYPos() - newStartPt.getYPos();
                    newStartPt.setXPos((int) Math.round(dockPos[0]));
                    newStartPt.setYPos((int) Math.round(dockPos[1]));
                    newStartPt.setCtrlXPos((int) Math.round(dockPos[0]) + ctrlOffsetX);
                    newStartPt.setCtrlYPos((int) Math.round(dockPos[1]) + ctrlOffsetY);
                }
            }
        }

        // Process target node dock point (end of edge)
        if (targetNode != null) {
            NodeGraphics tgtGraphics = targetNode.getGraphics();
            NodePosition tgtPos = tgtGraphics != null ? tgtGraphics.getPosition() : null;
            double tgtX = tgtPos != null ? tgtPos.getXPos() : 0;
            double tgtY = tgtPos != null ? tgtPos.getYPos() : 0;
            boolean tgtIsSuperNode = targetNode instanceof SuperNode;
            String tgtId = targetNode.getId();

            // Find old dock point index
            int oldTgtDock = -1;
            if (oldEndPt != null) {
                oldTgtDock = findDockPointIndex(tgtX, tgtY, nodeWidth, nodeHeight, tgtIsSuperNode,
                        oldEndPt.getXPos(), oldEndPt.getYPos());
            }

            // Check if endpoint position changed (not just control point)
            boolean tgtEndpointMoved = oldEndPt == null ||
                    Math.abs(newEndPt.getXPos() - oldEndPt.getXPos()) > 5 ||
                    Math.abs(newEndPt.getYPos() - oldEndPt.getYPos()) > 5;

            if (tgtEndpointMoved) {
                // Find nearest available dock point to new position
                int newTgtDock = findNearestAvailableDockPoint(tgtId, tgtX, tgtY, nodeWidth, nodeHeight,
                        tgtIsSuperNode, newEndPt.getXPos(), newEndPt.getYPos());

                if (newTgtDock >= 0 && newTgtDock != oldTgtDock) {
                    // Release old dock point
                    if (oldTgtDock >= 0) {
                        releaseDockPoint(tgtId, oldTgtDock, false);
                    }
                    // Occupy new dock point
                    occupyDockPoint(tgtId, newTgtDock, false);

                    // Snap endpoint to dock position, preserve control point offset
                    double[] dockPos = getDockPointPosition(tgtX, tgtY, nodeWidth, nodeHeight, tgtIsSuperNode, newTgtDock);
                    int ctrlOffsetX = newEndPt.getCtrlXPos() - newEndPt.getXPos();
                    int ctrlOffsetY = newEndPt.getCtrlYPos() - newEndPt.getYPos();
                    newEndPt.setXPos((int) Math.round(dockPos[0]));
                    newEndPt.setYPos((int) Math.round(dockPos[1]));
                    newEndPt.setCtrlXPos((int) Math.round(dockPos[0]) + ctrlOffsetX);
                    newEndPt.setCtrlYPos((int) Math.round(dockPos[1]) + ctrlOffsetY);
                }
            }
        }

        // Copy points to new list
        ArrayList<EdgePoint> newPoints = new ArrayList<>();
        for (EdgePoint point : points) {
            if (point != null) {
                newPoints.add(point.getCopy());
            }
        }
        arrow.setPointList(newPoints);
    }

    private Point minNodePosition(List<BasicNode> nodes) {
        if (nodes == null || nodes.isEmpty()) {
            return null;
        }
        int minX = Integer.MAX_VALUE;
        int minY = Integer.MAX_VALUE;
        for (BasicNode node : nodes) {
            if (node == null) {
                continue;
            }
            NodeGraphics graphics = node.getGraphics();
            NodePosition pos = graphics != null ? graphics.getPosition() : null;
            int x = pos != null ? pos.getXPos() : 0;
            int y = pos != null ? pos.getYPos() : 0;
            if (x < minX) {
                minX = x;
            }
            if (y < minY) {
                minY = y;
            }
        }
        if (minX == Integer.MAX_VALUE || minY == Integer.MAX_VALUE) {
            return null;
        }
        return new Point(minX, minY);
    }

    private void collectNodeIdsRecursive(BasicNode node, Map<BasicNode, String> mapping) {
        if (node == null || mapping == null) {
            return;
        }
        mapping.put(node, node.getId());
        if (node instanceof SuperNode) {
            for (BasicNode child : ((SuperNode) node).getNodeAndSuperNodeList()) {
                collectNodeIdsRecursive(child, mapping);
            }
        }
    }

    private void remapAltStartMapsRecursive(BasicNode node, Map<String, String> idMap) {
        if (node == null || idMap == null) {
            return;
        }
        for (AbstractEdge edge : node.getEdgeList()) {
            if (edge == null || edge.getAltMap() == null || edge.getAltMap().isEmpty()) {
                continue;
            }
            Map<Tuple<String, BasicNode>, Tuple<String, BasicNode>> remapped = new LinkedHashMap<>();
            for (Map.Entry<Tuple<String, BasicNode>, Tuple<String, BasicNode>> entry : edge.getAltMap().entrySet()) {
                Tuple<String, BasicNode> start = entry.getKey();
                Tuple<String, BasicNode> alt = entry.getValue();
                String startId = start != null ? start.getFirst() : "";
                String altId = alt != null ? alt.getFirst() : "";
                String newStartId = idMap.getOrDefault(startId, startId);
                String newAltId = idMap.getOrDefault(altId, altId);
                Tuple<String, BasicNode> startCopy = new Tuple<>(newStartId, start != null ? start.getSecond() : null);
                Tuple<String, BasicNode> altCopy = new Tuple<>(newAltId, alt != null ? alt.getSecond() : null);
                remapped.put(startCopy, altCopy);
            }
            edge.setAltMap(remapped);
        }
        if (node instanceof SuperNode) {
            for (BasicNode child : ((SuperNode) node).getNodeAndSuperNodeList()) {
                remapAltStartMapsRecursive(child, idMap);
            }
        }
    }

    private void normalizeSuperNodesRecursive(BasicNode node) {
        if (!(node instanceof SuperNode)) {
            return;
        }
        SuperNode superNode = (SuperNode) node;
        superNode.establishTargetNodes();
        superNode.establishStartNodes();
        superNode.establishAltStartNodes();
        for (SuperNode child : superNode.getSuperNodeList()) {
            normalizeSuperNodesRecursive(child);
        }
    }

    private void offsetNodeGraphics(BasicNode node, int dx, int dy) {
        if (node == null) {
            return;
        }
        NodeGraphics graphics = node.getGraphics();
        if (graphics == null) {
            graphics = new NodeGraphics();
            node.setGraphics(graphics);
        }
        NodePosition pos = graphics.getPosition();
        int x = pos != null ? pos.getXPos() : 0;
        int y = pos != null ? pos.getYPos() : 0;
        graphics.setPosition(x + dx, y + dy);
    }

    private void offsetEdgeGraphics(AbstractEdge edge, int dx, int dy) {
        if (edge == null) {
            return;
        }
        EdgeGraphics graphics = edge.getGraphics();
        if (graphics == null) {
            return;
        }
        EdgeArrow arrow = graphics.getConnection();
        if (arrow == null || arrow.getPointList() == null) {
            return;
        }
        for (EdgePoint point : arrow.getPointList()) {
            if (point == null) {
                continue;
            }
            int x = point.getXPos();
            int y = point.getYPos();
            int cx = point.getCtrlXPos();
            int cy = point.getCtrlYPos();
            if (x != Integer.MIN_VALUE) {
                point.setXPos(x + dx);
            }
            if (y != Integer.MIN_VALUE) {
                point.setYPos(y + dy);
            }
            if (cx != Integer.MIN_VALUE) {
                point.setCtrlXPos(cx + dx);
            }
            if (cy != Integer.MIN_VALUE) {
                point.setCtrlYPos(cy + dy);
            }
        }
    }

    private void applyStartMap(WorkSpacePanel workSpace, SuperNode superNode, Map<String, BasicNode> desired) {
        if (workSpace == null || superNode == null) {
            return;
        }
        for (Node node : workSpace.getNodes()) {
            if (node == null) {
                continue;
            }
            BasicNode dataNode = node.getDataNode();
            if (dataNode == null || dataNode.getParentNode() != superNode) {
                continue;
            }
            boolean shouldStart = desired != null && desired.containsKey(dataNode.getId());
            updateStartFlag(dataNode, node, shouldStart);
        }
    }

    private boolean normalizeEdgeForCopy(AbstractEdge edge, String sourceId, Set<String> allowedTargets) {
        if (edge == null) {
            return false;
        }
        String targetId = edge.getTargetUnid();
        if (targetId == null || targetId.isBlank()) {
            BasicNode targetNode = edge.getTargetNode();
            if (targetNode != null) {
                targetId = targetNode.getId();
                edge.setTargetUnid(targetId);
            }
        }
        edge.setSourceUnid(sourceId == null ? "" : sourceId);
        if (targetId == null || targetId.isBlank()) {
            return false;
        }
        return allowedTargets == null || allowedTargets.contains(targetId);
    }

    private <T extends AbstractEdge> ArrayList<T> filterEdgesForCopy(
            List<T> edges,
            String sourceId,
            Set<String> allowedTargets) {
        ArrayList<T> filtered = new ArrayList<>();
        if (edges == null) {
            return filtered;
        }
        for (T edge : edges) {
            if (normalizeEdgeForCopy(edge, sourceId, allowedTargets)) {
                filtered.add(edge);
            }
        }
        return filtered;
    }

    private void normalizeEdgeIdsRecursive(BasicNode node, Set<String> allowedTargets) {
        if (node == null) {
            return;
        }
        String sourceId = node.getId();
        normalizeEdgeList(node.getCEdgeList(), sourceId, allowedTargets);
        normalizeEdgeList(node.getPEdgeList(), sourceId, allowedTargets);
        normalizeEdgeList(node.getFEdgeList(), sourceId, allowedTargets);
        normalizeEdgeList(node.getIEdgeList(), sourceId, allowedTargets);
        if (node.hasDEdge()) {
            if (!normalizeEdgeForCopy(node.getDedge(), sourceId, allowedTargets)) {
                node.removeDEdge();
            }
        }
        if (node instanceof SuperNode) {
            for (BasicNode child : ((SuperNode) node).getNodeAndSuperNodeList()) {
                normalizeEdgeIdsRecursive(child, allowedTargets);
            }
        }
    }

    private <T extends AbstractEdge> void normalizeEdgeList(
            List<T> edges,
            String sourceId,
            Set<String> allowedTargets) {
        if (edges == null) {
            return;
        }
        Iterator<T> iterator = edges.iterator();
        while (iterator.hasNext()) {
            T edge = iterator.next();
            if (!normalizeEdgeForCopy(edge, sourceId, allowedTargets)) {
                iterator.remove();
            }
        }
    }

    private CommentBadge resolveCommentById(SuperNode superNode, String commentId) {
        if (superNode == null || commentId == null) {
            return null;
        }
        List<CommentBadge> list = superNode.getCommentList();
        if (list == null || list.isEmpty()) {
            return null;
        }
        String normalized = commentId.trim();
        if (normalized.startsWith("C")) {
            normalized = normalized.substring(1);
        }
        int index;
        try {
            index = Integer.parseInt(normalized);
        } catch (NumberFormatException ex) {
            return null;
        }
        if (index < 0 || index >= list.size()) {
            return null;
        }
        return list.get(index);
    }

    private Comment findCommentComponent(WorkSpacePanel workSpace, CommentBadge badge) {
        if (workSpace == null || badge == null) {
            return null;
        }
        for (Component comp : workSpace.getComponents()) {
            if (comp instanceof Comment) {
                Comment comment = (Comment) comp;
                if (badge.equals(comment.getData())) {
                    return comment;
                }
            }
        }
        return null;
    }

    private AbstractEdge resolveEdgeById(SuperNode superNode, String edgeId) {
        if (superNode == null || edgeId == null) {
            return null;
        }
        String normalized = edgeId.trim();
        if (normalized.startsWith("E")) {
            normalized = normalized.substring(1);
        }
        int index;
        try {
            index = Integer.parseInt(normalized);
        } catch (NumberFormatException ex) {
            return null;
        }
        if (index < 0) {
            return null;
        }
        int edgeIndex = 0;
        for (BasicNode node : superNode.getNodeAndSuperNodeList()) {
            for (AbstractEdge edge : node.getEdgeList()) {
                if (edgeIndex == index) {
                    return edge;
                }
                edgeIndex++;
            }
        }
        return null;
    }

    private AbstractEdge resolveEdgeByNodes(SuperNode superNode, String sourceId, String targetId) {
        if (superNode == null || sourceId == null || targetId == null) {
            return null;
        }
        for (BasicNode node : superNode.getNodeAndSuperNodeList()) {
            for (AbstractEdge edge : node.getEdgeList()) {
                String source = edge.getSourceUnid();
                if (source == null || source.isBlank()) {
                    source = edge.getSourceNode() != null ? edge.getSourceNode().getId() : "";
                }
                String target = edge.getTargetUnid();
                if (target == null || target.isBlank()) {
                    target = edge.getTargetNode() != null ? edge.getTargetNode().getId() : "";
                }
                if (sourceId.equals(source) && targetId.equals(target)) {
                    return edge;
                }
            }
        }
        return null;
    }

    private RandomEdge resolvePEdgeForSource(
            SuperNode superNode,
            BasicNode sourceNode,
            String edgeId,
            String targetId) {
        if (sourceNode == null) {
            return null;
        }
        if (edgeId != null && !edgeId.isBlank()) {
            AbstractEdge resolved = resolveEdgeById(superNode, edgeId);
            if (resolved instanceof RandomEdge) {
                RandomEdge pedge = (RandomEdge) resolved;
                BasicNode edgeSource = resolveEdgeSourceNode(pedge, superNode);
                if (edgeSource != null && edgeSource.equals(sourceNode)) {
                    return pedge;
                }
            }
        }
        if (targetId != null && !targetId.isBlank()) {
            for (RandomEdge edge : sourceNode.getPEdgeList()) {
                if (edge == null) {
                    continue;
                }
                String target = edge.getTargetUnid();
                if ((target == null || target.isBlank()) && edge.getTargetNode() != null) {
                    target = edge.getTargetNode().getId();
                }
                if (targetId.equals(target)) {
                    return edge;
                }
            }
        }
        return null;
    }

    private Edge findGuiEdgeByData(WorkSpacePanel workSpace, AbstractEdge dataEdge) {
        if (workSpace == null || dataEdge == null) {
            return null;
        }
        for (Edge edge : workSpace.getEdges()) {
            if (edge.getDataEdge() == dataEdge) {
                return edge;
            }
        }
        return null;
    }

    private boolean requiresAuth(String path) {
        // Public endpoints that don't require authentication
        if ((API_PREFIX + "/token").equals(path)) {
            return false;
        }
        if ((API_PREFIX + "/info").equals(path)) {
            return false;
        }
        // Landing page endpoints - should be accessible without auth
        if ((API_PREFIX + "/preferences").equals(path)) {
            return false;
        }
        if ((API_PREFIX + "/projects/recent").equals(path)) {
            return false;
        }
        if ((API_PREFIX + "/projects").equals(path)) {
            return false;
        }
        if ((API_PREFIX + "/projects/tutorials").equals(path)) {
            return false;
        }
        if ((API_PREFIX + "/projects/samples").equals(path)) {
            return false;
        }
        // Project-specific endpoints - allow all /api/v1/projects/{id}/* paths
        // These are needed for the Web UI to work without authentication
        if (path.startsWith(API_PREFIX + "/projects/") && !path.equals(API_PREFIX + "/projects/")) {
            return false;
        }
        return path.startsWith("/api/") || path.startsWith("/ws");
    }

    private boolean isAuthorized(Context ctx) {
        String token = extractBearerToken(ctx.header("Authorization"));
        if (token == null || token.isEmpty()) {
            token = ctx.queryParam("token");
        }
        return tokenMatches(token);
    }

    private boolean isAuthorized(WsConnectContext ctx) {
        String token = ctx.queryParam("token");
        return tokenMatches(token);
    }

    private boolean tokenMatches(String token) {
        return mToken == null || mToken.isEmpty() || mToken.equals(token);
    }

    private boolean isLocalRequest(Context ctx) {
        String ip = ctx.ip();
        if (ip == null || ip.isBlank()) {
            return false;
        }
        if (ip.startsWith("127.") || ip.equals("::1") || ip.equals("0:0:0:0:0:0:0:1")) {
            return true;
        }
        return ip.startsWith("::ffff:127.");
    }

    private String extractBearerToken(String header) {
        if (header == null) {
            return null;
        }
        String prefix = "bearer ";
        if (header.toLowerCase(Locale.ROOT).startsWith(prefix)) {
            return header.substring(prefix.length()).trim();
        }
        return null;
    }

    private String resolveToken() {
        String token = System.getProperty("vsm.web.token");
        if (token == null || token.isBlank()) {
            token = System.getenv("VSM_WEB_TOKEN");
        }
        if (token == null || token.isBlank()) {
            token = generateToken();
        }
        return token;
    }

    private String generateToken() {
        byte[] bytes = new byte[16];
        mRandom.nextBytes(bytes);
        StringBuilder sb = new StringBuilder();
        for (byte b : bytes) {
            sb.append(String.format("%02x", b));
        }
        return sb.toString();
    }

    private void logStartup() {
        String host = "127.0.0.1";
        String lanHost = null;
        if (mAllowExternal) {
            try {
                lanHost = InetAddress.getLocalHost().getHostAddress();
            } catch (Exception ignored) {
            }
        }
        mLogger.message("Web UI server started on http://" + host + ":" + mPort + "/");
        if (lanHost != null && !lanHost.isBlank()) {
            mLogger.message("Web UI LAN access enabled at http://" + lanHost + ":" + mPort + "/");
        }
        if (mToken != null && !mToken.isEmpty()) {
            mLogger.message("Web UI token: " + mToken);
        }
    }

    private <T> T callOnEdt(Supplier<T> supplier) {
        if (SwingUtilities.isEventDispatchThread()) {
            return supplier.get();
        }
        AtomicReference<T> result = new AtomicReference<>();
        AtomicReference<RuntimeException> error = new AtomicReference<>();
        try {
            SwingUtilities.invokeAndWait(() -> {
                try {
                    result.set(supplier.get());
                } catch (RuntimeException ex) {
                    error.set(ex);
                }
            });
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        if (error.get() != null) {
            throw error.get();
        }
        return result.get();
    }
}
