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
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgeArrow;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgeGraphics;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgePoint;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.tpl.Tuple;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

/**
 * Handles SceneFlow.Edge.Add/Create/Update/Delete commands.
 */
public final class EdgeCrudCommandService {

    public interface Context {
        RunTimeProject runtimeProject(String projectId);

        JSONObject mutateAndSnapshotLegacy(String projectId, String operation, JSONObject params, Consumer<String> broadcaster);

        JSONObject errorResponse(String code, String message);

        SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId);

        BasicNode resolveNodeById(SuperNode root, String nodeId);

        AbstractEdge resolveEdgeById(SuperNode root, String edgeId);

        Expression parseExpressionOrNull(String text);

        int getEditorConfigInt(String projectId, String key, int fallback);

        void initializeEdgeDockPoints(AbstractEdge edge, int nodeWidth, int nodeHeight);

        void normalizeEdge(AbstractEdge edge, int nodeWidth, int nodeHeight);

        void releaseEdgeDockPoints(AbstractEdge edge, int nodeWidth, int nodeHeight);

        JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SuperNode snapshotTarget, SceneFlow sceneFlow);

        JSONObject buildSceneFlowResponse(JSONObject snapshot);

        void broadcastSceneFlowSnapshot(Consumer<String> broadcaster, String projectId, JSONObject snapshot);

        void recordHistory(String projectId, String action);

        void recordCommand(String projectId, String action, JSONObject params);
    }

    public JSONObject dispatch(final String method,
                               final JSONObject params,
                               final Consumer<String> broadcaster,
                               final Context context) {
        switch (method) {
            case "SceneFlow.Edge.Add":
            case "SceneFlow.Edge.Create":
                return createEdgeForProject(params, broadcaster, context);
            case "SceneFlow.Edge.Update":
                return updateEdgeForProject(params, broadcaster, context);
            case "SceneFlow.Edge.Delete":
                return deleteEdgeForProject(params, broadcaster, context);
            default:
                return context.errorResponse("BAD_REQUEST", "Unsupported edge CRUD command: " + method);
        }
    }

    private JSONObject createEdgeForProject(final JSONObject params,
                                            final Consumer<String> broadcaster,
                                            final Context context) {
        String pid = params.optString("projectId", "");
        RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.mutateAndSnapshotLegacy(pid, "add", params, broadcaster);
        }

        SceneFlow sceneFlow = project.getSceneFlow();
        String superNodeId = params.optString("superNodeId", null);
        SuperNode snapshotTarget = context.resolveSuperNode(sceneFlow, superNodeId);
        SuperNode activeSuperNode = snapshotTarget != null ? snapshotTarget : sceneFlow;

        String sourceId = params.optString("sourceId", params.optString("source", ""));
        String targetId = params.optString("targetId", params.optString("target", ""));
        if (sourceId.isBlank() || targetId.isBlank()) {
            return context.errorResponse("BAD_REQUEST", "Missing sourceId or targetId");
        }

        BasicNode sourceNode = context.resolveNodeById(activeSuperNode, sourceId);
        BasicNode targetNode = context.resolveNodeById(activeSuperNode, targetId);
        if (sourceNode == null || targetNode == null) {
            return context.errorResponse("NODE_NOT_FOUND", "Source or target node not found");
        }

        String edgeType = params.optString("edgeType", params.optString("type", "EEDGE")).trim().toUpperCase();
        String edgeConstraintError = validateEdgeCreateConstraints(sourceNode, edgeType);
        if (edgeConstraintError != null) {
            return context.errorResponse("EDGE_NOT_ALLOWED", edgeConstraintError);
        }

        AbstractEdge edge;
        switch (edgeType) {
            case "CEDGE":
                edge = new GuargedEdge();
                ((GuargedEdge) edge).setCondition(context.parseExpressionOrNull(
                        sourceNode.getCEdgeList() == null || sourceNode.getCEdgeList().isEmpty() ? "true" : "false"
                ));
                sourceNode.addCEdge((GuargedEdge) edge);
                break;
            case "IEDGE":
                edge = new InterruptEdge();
                ((InterruptEdge) edge).setCondition(context.parseExpressionOrNull(
                        sourceNode.getIEdgeList() == null || sourceNode.getIEdgeList().isEmpty() ? "true" : "false"
                ));
                sourceNode.addIEdge((InterruptEdge) edge);
                break;
            case "PEDGE":
                RandomEdge redge = new RandomEdge();
                redge.setProbability((sourceNode.getPEdgeList() == null || sourceNode.getPEdgeList().isEmpty()) ? 100 : 0);
                edge = redge;
                sourceNode.addPEdge((RandomEdge) edge);
                break;
            case "FEDGE":
                edge = new ForkingEdge();
                sourceNode.addFEdge((ForkingEdge) edge);
                break;
            case "TEDGE":
                TimeoutEdge ted = new TimeoutEdge();
                try {
                    ted.setTimeout(1000);
                } catch (NumberFormatException ignore) {
                    // Keep default timeout on parse failure.
                }
                edge = ted;
                sourceNode.setDedge(edge);
                break;
            case "EEDGE":
            default:
                edge = new EpsilonEdge();
                sourceNode.setDedge(edge);
                break;
        }

        edge.setSourceNode(sourceNode);
        edge.setTargetNode(targetNode);
        edge.setSourceUnid(sourceNode.getId());
        edge.setTargetUnid(targetNode.getId());
        edge.setGraphics(new EdgeGraphics());

        int nodeWidth = context.getEditorConfigInt(pid, "node_width", 90);
        int nodeHeight = context.getEditorConfigInt(pid, "node_height", nodeWidth);
        context.initializeEdgeDockPoints(edge, nodeWidth, nodeHeight);
        context.normalizeEdge(edge, nodeWidth, nodeHeight);

        JSONObject snapshot = context.createSceneFlowSnapshot(project, pid, snapshotTarget, sceneFlow);
        JSONObject response = context.buildSceneFlowResponse(snapshot);
        context.broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
        context.recordHistory(pid, "SceneFlow.Edge.Create");
        context.recordCommand(pid, "SceneFlow.Edge.Create", params);
        return response;
    }

    private JSONObject updateEdgeForProject(final JSONObject params,
                                            final Consumer<String> broadcaster,
                                            final Context context) {
        String pid = params.optString("projectId", "");
        RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.mutateAndSnapshotLegacy(pid, "update", params, broadcaster);
        }

        SceneFlow sceneFlow = project.getSceneFlow();
        String superNodeId = params.optString("superNodeId", null);
        SuperNode snapshotTarget = context.resolveSuperNode(sceneFlow, superNodeId);
        SuperNode activeSuperNode = snapshotTarget != null ? snapshotTarget : sceneFlow;
        String edgeId = params.optString("edgeId", "");
        if (edgeId.isBlank()) {
            return context.errorResponse("BAD_REQUEST", "Missing edgeId");
        }
        AbstractEdge edge = context.resolveEdgeById(activeSuperNode, edgeId);
        if (edge == null) {
            return context.errorResponse("EDGE_NOT_FOUND", "Edge not found: " + edgeId);
        }

        JSONObject fields = params.optJSONObject("fields");
        if (fields == null) {
            fields = new JSONObject();
        }

        if (fields.has("points")) {
            JSONArray points = fields.optJSONArray("points");
            if (points != null) {
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
                ArrayList<EdgePoint> pointList = new ArrayList<>();
                for (int i = 0; i < points.length(); i++) {
                    JSONObject pt = points.optJSONObject(i);
                    if (pt == null) {
                        continue;
                    }
                    int x = safeRound(pt.has("x") ? pt.optDouble("x") : null, 0);
                    int y = safeRound(pt.has("y") ? pt.optDouble("y") : null, 0);
                    int cx = safeRound(pt.has("cx") ? pt.optDouble("cx") : null, x);
                    int cy = safeRound(pt.has("cy") ? pt.optDouble("cy") : null, y);
                    pointList.add(new EdgePoint(x, cx, y, cy));
                }
                arrow.setPointList(pointList);
            }
        }

        if (fields.has("condition")) {
            String conditionText = fields.optString("condition", "").trim();
            if (edge instanceof GuargedEdge) {
                ((GuargedEdge) edge).setCondition(context.parseExpressionOrNull(conditionText));
            } else if (edge instanceof InterruptEdge) {
                ((InterruptEdge) edge).setCondition(context.parseExpressionOrNull(conditionText));
            }
        }
        if (fields.has("timeoutMs") || fields.has("timeoutExpr") || fields.has("timeoutMinMs") || fields.has("timeoutMaxMs")) {
            if (edge instanceof TimeoutEdge) {
                TimeoutEdge te = (TimeoutEdge) edge;
                if (fields.has("timeoutMs")) {
                    try {
                        te.setTimeout(fields.optLong("timeoutMs", 0));
                        te.setExpression(null);
                        te.clearTimeoutRange();
                    } catch (NumberFormatException ignore) {
                        // ignore invalid timeout
                    }
                } else if (fields.has("timeoutMinMs") || fields.has("timeoutMaxMs")) {
                    long min = fields.optLong("timeoutMinMs", Long.MIN_VALUE);
                    long max = fields.optLong("timeoutMaxMs", Long.MIN_VALUE);
                    if (min >= 0 && max >= min) {
                        try {
                            te.setTimeoutRange(min, max);
                            te.setTimeout(min);
                            te.setExpression(null);
                        } catch (NumberFormatException ignore) {
                            // ignore invalid timeout range
                        }
                    }
                } else if (fields.has("timeoutExpr")) {
                    String exprText = fields.optString("timeoutExpr", "").trim();
                    te.clearTimeoutRange();
                    te.setExpression(context.parseExpressionOrNull(exprText));
                }
            }
        }
        if (fields.has("altStartMap") && (edge instanceof GuargedEdge || edge instanceof InterruptEdge)) {
            JSONArray entries = fields.optJSONArray("altStartMap");
            if (entries != null) {
                edge.getAltMap().clear();
                for (int i = 0; i < entries.length(); i++) {
                    JSONObject entry = entries.optJSONObject(i);
                    if (entry == null) {
                        continue;
                    }
                    String startId = entry.optString("startId", "").trim();
                    String altStartId = entry.optString("altStartId", "").trim();
                    if (startId.isEmpty() || altStartId.isEmpty()) {
                        continue;
                    }
                    BasicNode startNode = context.resolveNodeById(activeSuperNode, startId);
                    BasicNode altNode = context.resolveNodeById(activeSuperNode, altStartId);
                    if (startNode == null || altNode == null) {
                        continue;
                    }
                    Tuple<String, BasicNode> startTuple = new Tuple<>(startId, startNode);
                    Tuple<String, BasicNode> altTuple = new Tuple<>(altStartId, altNode);
                    edge.getAltMap().put(startTuple, altTuple);
                }
            }
        }

        JSONObject snapshot = context.createSceneFlowSnapshot(project, pid, snapshotTarget, sceneFlow);
        JSONObject response = context.buildSceneFlowResponse(snapshot);
        context.broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
        context.recordHistory(pid, "SceneFlow.Edge.Update");
        context.recordCommand(pid, "SceneFlow.Edge.Update", params);
        return response;
    }

    private JSONObject deleteEdgeForProject(final JSONObject params,
                                            final Consumer<String> broadcaster,
                                            final Context context) {
        String pid = params.optString("projectId", "");
        RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.mutateAndSnapshotLegacy(pid, "delete", params, broadcaster);
        }

        SceneFlow sceneFlow = project.getSceneFlow();
        String superNodeId = params.optString("superNodeId", null);
        SuperNode snapshotTarget = context.resolveSuperNode(sceneFlow, superNodeId);
        SuperNode activeSuperNode = snapshotTarget != null ? snapshotTarget : sceneFlow;
        String edgeId = params.optString("edgeId", "");
        if (edgeId.isBlank()) {
            return context.errorResponse("BAD_REQUEST", "Missing edgeId");
        }
        AbstractEdge dataEdge = context.resolveEdgeById(activeSuperNode, edgeId);
        if (dataEdge == null) {
            return context.errorResponse("EDGE_NOT_FOUND", "Edge not found: " + edgeId);
        }

        BasicNode sourceNode = dataEdge.getSourceNode();
        if (sourceNode != null) {
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

        int nodeWidth = context.getEditorConfigInt(pid, "node_width", 90);
        int nodeHeight = context.getEditorConfigInt(pid, "node_height", nodeWidth);
        context.releaseEdgeDockPoints(dataEdge, nodeWidth, nodeHeight);

        JSONObject snapshot = context.createSceneFlowSnapshot(project, pid, snapshotTarget, sceneFlow);
        JSONObject response = context.buildSceneFlowResponse(snapshot);
        context.broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
        context.recordHistory(pid, "SceneFlow.Edge.Delete");
        context.recordCommand(pid, "SceneFlow.Edge.Delete", params);
        return response;
    }

    private String validateEdgeCreateConstraints(final BasicNode sourceNode, final String edgeType) {
        if (sourceNode == null) {
            return "Source node not found";
        }
        String type = edgeType == null ? "" : edgeType.trim().toUpperCase();
        boolean hasC = sourceNode.getCEdgeList() != null && !sourceNode.getCEdgeList().isEmpty();
        boolean hasP = sourceNode.getPEdgeList() != null && !sourceNode.getPEdgeList().isEmpty();
        boolean hasI = sourceNode.getIEdgeList() != null && !sourceNode.getIEdgeList().isEmpty();
        boolean hasF = sourceNode.getFEdgeList() != null && !sourceNode.getFEdgeList().isEmpty();
        AbstractEdge dEdge = sourceNode.getDedge();
        boolean hasE = dEdge instanceof EpsilonEdge;
        boolean hasT = dEdge instanceof TimeoutEdge;
        boolean hasD = dEdge != null;
        boolean hasSelfLoopT = hasT
                && sourceNode.getId() != null
                && sourceNode.getId().equals(dEdge.getTargetUnid());

        if (hasP) {
            return "PEDGE".equals(type) ? null : "Only probabilistic edges are allowed on this node";
        }
        if (hasI) {
            if ("IEDGE".equals(type) || "TEDGE".equals(type)) {
                return null;
            }
            return "Only interrupt edges are allowed on this node (plus one timeout edge)";
        }
        if (hasF) {
            return "FEDGE".equals(type) ? null : "Only fork edges are allowed on this node";
        }

        if (hasC) {
            if ("CEDGE".equals(type)) {
                return null;
            }
            if ("EEDGE".equals(type) || "TEDGE".equals(type)) {
                return hasD ? "Only one default/timeout edge is allowed on this node" : null;
            }
            if ("IEDGE".equals(type) && hasSelfLoopT) {
                return null;
            }
            return "Only conditional edges are allowed (plus one epsilon or timeout edge)";
        }

        if (hasD) {
            if ("CEDGE".equals(type)) {
                return null;
            }
            if ("IEDGE".equals(type) && hasSelfLoopT) {
                return null;
            }
            if (hasE) {
                return "Only conditional edges can be combined with an epsilon edge";
            }
            if (hasT) {
                return "Only conditional edges can be combined with a timeout edge";
            }
            return "Only conditional edges can be combined with the default edge";
        }

        return null;
    }

    private int safeRound(final Double value, final int fallback) {
        return value == null || value.isNaN() || value.isInfinite() ? fallback : (int) Math.round(value);
    }
}
