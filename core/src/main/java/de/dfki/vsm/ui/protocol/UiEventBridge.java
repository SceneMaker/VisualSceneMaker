package de.dfki.vsm.ui.protocol;

import de.dfki.vsm.event.EventListener;
import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.event.event.EdgeExecutedEvent;
import de.dfki.vsm.event.event.EdgeEditEvent;
import de.dfki.vsm.event.event.EdgeSelectedEvent;
import de.dfki.vsm.event.event.FunctionCreatedEvent;
import de.dfki.vsm.event.event.FunctionModifiedEvent;
import de.dfki.vsm.event.event.FunctionRemovedEvent;
import de.dfki.vsm.event.event.NodeExecutedEvent;
import de.dfki.vsm.event.event.NodeStartedEvent;
import de.dfki.vsm.event.event.NodeTerminatedEvent;
import de.dfki.vsm.event.event.NodeSelectedEvent;
import de.dfki.vsm.event.event.ProjectChangedEvent;
import de.dfki.vsm.event.event.SceneExecutedEvent;
import de.dfki.vsm.event.event.SceneDoneEvent;
import de.dfki.vsm.event.event.SceneStoppedEvent;
import de.dfki.vsm.event.event.TimeoutEdgeStartedEvent;
import de.dfki.vsm.event.event.TurnExecutedEvent;
import de.dfki.vsm.event.event.TurnDoneEvent;
import de.dfki.vsm.event.event.VariableChangedEvent;
import de.dfki.vsm.model.scenescript.SceneObject;
import de.dfki.vsm.model.scenescript.SceneTurn;
import de.dfki.vsm.event.event.WorkSpaceSelectedEvent;
import de.dfki.vsm.model.sceneflow.chart.BasicNode;
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
import de.dfki.vsm.util.tpl.Tuple;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

public final class UiEventBridge implements EventListener {
    private final UiEventSink mSink;

    public UiEventBridge(final UiEventSink sink) {
        mSink = sink;
    }

    @Override
    public void update(final EventObject event) {
        if (event == null || mSink == null || !mSink.isActive()) {
            return;
        }
        if (event instanceof VariableChangedEvent) {
            VariableChangedEvent variableEvent = (VariableChangedEvent) event;
            Tuple<String, String> pair = variableEvent.getVarValue();
            if (pair == null || pair.getFirst() == null || pair.getFirst().isBlank()) {
                return;
            }
            mSink.emitLazy(() -> UiEvent.create(UiChannel.VARS, "vars.updated",
                    variablePayload(pair.getFirst(), pair.getSecond())));
            return;
        }
        if (event instanceof ProjectChangedEvent) {
            emitProjectDirty(event, null);
            return;
        }
        if (event instanceof FunctionCreatedEvent
                || event instanceof FunctionModifiedEvent
                || event instanceof FunctionRemovedEvent) {
            emitProjectDirty(event, Arrays.asList("sceneflow"));
            return;
        }
        if (event instanceof EdgeEditEvent) {
            AbstractEdge edge = ((EdgeEditEvent) event).getEdge();
            if (edge != null) {
                emitEdgeUpdated(edge);
            }
            return;
        }
        if (event instanceof NodeSelectedEvent) {
            BasicNode node = ((NodeSelectedEvent) event).getNode();
            if (node != null && node.getId() != null && !node.getId().isBlank()) {
                emitSelection(Collections.singletonList(node.getId()), Collections.emptyList(), Collections.emptyList());
            }
            return;
        }
        if (event instanceof EdgeSelectedEvent) {
            AbstractEdge edge = ((EdgeSelectedEvent) event).getEdge();
            String edgeId = resolveEdgeId(edge);
            if (edgeId != null && !edgeId.isBlank()) {
                emitSelection(Collections.emptyList(), Collections.singletonList(edgeId), Collections.emptyList());
            }
            return;
        }
        if (event instanceof WorkSpaceSelectedEvent) {
            emitSelection(Collections.emptyList(), Collections.emptyList(), Collections.emptyList());
            return;
        }
        if (event instanceof NodeStartedEvent) {
            BasicNode node = ((NodeStartedEvent) event).getNode();
            if (node == null) {
                return;
            }
            mSink.emitLazy(() -> UiEvent.create(UiChannel.RUNTIME, "runtime.nodeActive", nodePayload(node)));
            return;
        }
        if (event instanceof NodeExecutedEvent || event instanceof NodeTerminatedEvent) {
            BasicNode node = event instanceof NodeExecutedEvent
                    ? ((NodeExecutedEvent) event).getNode()
                    : ((NodeTerminatedEvent) event).getNode();
            if (node == null) {
                return;
            }
            mSink.emitLazy(() -> UiEvent.create(UiChannel.RUNTIME, "runtime.nodeStopped", nodePayload(node)));
            return;
        }
        if (event instanceof SceneExecutedEvent) {
            SceneObject scene = ((SceneExecutedEvent) event).getScene();
            if (scene == null) return;
            mSink.emitLazy(() -> UiEvent.create(UiChannel.RUNTIME, "runtime.scene.playing",
                    scenePayload(scene)));
            return;
        }
        if (event instanceof SceneDoneEvent) {
            SceneObject scene = ((SceneDoneEvent) event).getScene();
            if (scene == null) return;
            mSink.emitLazy(() -> UiEvent.create(UiChannel.RUNTIME, "runtime.scene.done",
                    scenePayload(scene)));
            return;
        }
        if (event instanceof TurnExecutedEvent) {
            SceneTurn turn = ((TurnExecutedEvent) event).getTurn();
            if (turn == null) return;
            mSink.emitLazy(() -> UiEvent.create(UiChannel.RUNTIME, "runtime.scene.turn",
                    turnPayload(turn)));
            return;
        }
        if (event instanceof TurnDoneEvent) {
            SceneTurn turn = ((TurnDoneEvent) event).getTurn();
            if (turn == null) return;
            mSink.emitLazy(() -> UiEvent.create(UiChannel.RUNTIME, "runtime.scene.turnDone",
                    turnPayload(turn)));
            return;
        }
        if (event instanceof SceneStoppedEvent) {
            Map<String, Object> payload = new LinkedHashMap<>();
            payload.put("status", "stopped");
            mSink.emitLazy(() -> UiEvent.create(UiChannel.RUNTIME, "runtime.state", payload));
            return;
        }
        if (event instanceof EdgeExecutedEvent) {
            AbstractEdge edge = ((EdgeExecutedEvent) event).getEdge();
            if (edge == null) {
                return;
            }
            mSink.emitLazy(() -> UiEvent.create(UiChannel.RUNTIME, "runtime.edgeActive", edgePayload(edge)));
            return;
        }
        if (event instanceof TimeoutEdgeStartedEvent) {
            TimeoutEdgeStartedEvent timeoutEvent = (TimeoutEdgeStartedEvent) event;
            TimeoutEdge edge = timeoutEvent.getEdge();
            if (edge == null) {
                return;
            }
            mSink.emitLazy(() -> UiEvent.create(UiChannel.RUNTIME, "runtime.timeoutProgress",
                    timeoutPayload(edge, timeoutEvent.getTimeoutMs(), timeoutEvent.getStartedAt())));
        }
    }

    private void emitProjectDirty(final EventObject event, final List<String> areasHint) {
        List<String> areas = areasHint != null && !areasHint.isEmpty()
                ? new ArrayList<>(areasHint)
                : deriveProjectAreas(event);
        Map<String, Object> payload = new LinkedHashMap<>();
        payload.put("dirty", true);
        payload.put("areas", areas);
        mSink.emitLazy(() -> UiEvent.create(UiChannel.PROJECT, "project.dirty", payload));
    }

    private void emitEdgeUpdated(final AbstractEdge edge) {
        Map<String, Object> payload = new LinkedHashMap<>();
        payload.put("edge", edgeObject(edge));
        mSink.emitLazy(() -> UiEvent.create(UiChannel.SCENEFLOW, "sceneflow.edgeUpdated", payload));
    }

    private void emitSelection(final List<String> nodes, final List<String> edges, final List<String> comments) {
        Map<String, Object> selection = new LinkedHashMap<>();
        selection.put("nodes", nodes);
        selection.put("edges", edges);
        selection.put("comments", comments);
        Map<String, Object> payload = new LinkedHashMap<>();
        payload.put("selection", selection);
        mSink.emitLazy(() -> UiEvent.create(UiChannel.SCENEFLOW, "sceneflow.selection", payload));
    }

    private List<String> deriveProjectAreas(final EventObject event) {
        String sourceName = "";
        if (event != null && event.getSource() != null) {
            sourceName = event.getSource().getClass().getName().toLowerCase(Locale.ROOT);
        }
        List<String> areas = new ArrayList<>();
        if (sourceName.contains("script")) {
            areas.add("script");
        }
        if (sourceName.contains("config") || sourceName.contains("optionsdialog") || sourceName.contains("preferences")) {
            areas.add("config");
        }
        if (sourceName.contains("sceneflow")
                || sourceName.contains("workspace")
                || sourceName.contains("node")
                || sourceName.contains("edge")
                || sourceName.contains("cmdbadge")) {
            areas.add("sceneflow");
        }
        if (areas.isEmpty()) {
            areas.add("sceneflow");
            areas.add("script");
            areas.add("config");
        }
        return areas;
    }

    private Map<String, Object> variablePayload(final String name, final String value) {
        Map<String, Object> payload = new LinkedHashMap<>();
        payload.put("name", name);
        payload.put("value", value == null ? "" : value);
        return payload;
    }

    private Map<String, Object> nodePayload(final BasicNode node) {
        Map<String, Object> payload = new LinkedHashMap<>();
        payload.put("nodeId", node.getId());
        SuperNode parent = node.getParentNode();
        if (parent != null) {
            payload.put("parentId", parent.getId());
        }
        return payload;
    }

    private Map<String, Object> edgePayload(final AbstractEdge edge) {
        Map<String, Object> payload = new LinkedHashMap<>();
        String sourceId = edge.getSourceUnid();
        if (sourceId == null || sourceId.isBlank()) {
            sourceId = edge.getSourceNode() != null ? edge.getSourceNode().getId() : "";
        }
        String targetId = edge.getTargetUnid();
        if (targetId == null || targetId.isBlank()) {
            targetId = edge.getTargetNode() != null ? edge.getTargetNode().getId() : "";
        }
        if (sourceId != null && !sourceId.isBlank()) {
            payload.put("sourceId", sourceId);
        }
        if (targetId != null && !targetId.isBlank()) {
            payload.put("targetId", targetId);
        }
        if (edge.getSourceNode() != null && edge.getSourceNode().getParentNode() != null) {
            payload.put("sourceParentId", edge.getSourceNode().getParentNode().getId());
        }
        if (edge.getTargetNode() != null && edge.getTargetNode().getParentNode() != null) {
            payload.put("targetParentId", edge.getTargetNode().getParentNode().getId());
        }
        payload.put("edgeType", edgeType(edge));
        return payload;
    }

    private Map<String, Object> edgeObject(final AbstractEdge edge) {
        Map<String, Object> json = new LinkedHashMap<>();
        String edgeId = resolveEdgeId(edge);
        if (edgeId != null && !edgeId.isBlank()) {
            json.put("id", edgeId);
        }
        json.put("type", edgeType(edge));
        String sourceId = edge.getSourceUnid();
        if (sourceId == null || sourceId.isBlank()) {
            sourceId = edge.getSourceNode() != null ? edge.getSourceNode().getId() : "";
        }
        String targetId = edge.getTargetUnid();
        if (targetId == null || targetId.isBlank()) {
            targetId = edge.getTargetNode() != null ? edge.getTargetNode().getId() : "";
        }
        if (sourceId != null && !sourceId.isBlank()) {
            json.put("sourceId", sourceId);
        }
        if (targetId != null && !targetId.isBlank()) {
            json.put("targetId", targetId);
        }
        List<Map<String, Object>> points = edgeControlPoints(edge);
        if (!points.isEmpty()) {
            json.put("controlPoints", points);
        }
        if (edge instanceof GuargedEdge) {
            Expression condition = ((GuargedEdge) edge).getCondition();
            if (condition != null) {
                json.put("label", condition.getConcreteSyntax());
            }
        }
        if (edge instanceof InterruptEdge) {
            Expression condition = ((InterruptEdge) edge).getCondition();
            if (condition != null) {
                json.put("label", condition.getConcreteSyntax());
            }
        }
        if (edge instanceof RandomEdge) {
            json.put("probability", ((RandomEdge) edge).getProbability());
        }
        if (edge instanceof TimeoutEdge) {
            TimeoutEdge timeoutEdge = (TimeoutEdge) edge;
            if (timeoutEdge.getExpression() != null) {
                json.put("timeoutExpr", timeoutEdge.getExpression().getConcreteSyntax());
            } else if (timeoutEdge.getTimeout() != Long.MIN_VALUE) {
                json.put("timeoutExpr", Long.toString(timeoutEdge.getTimeout()));
            }
        }
        return json;
    }

    private List<Map<String, Object>> edgeControlPoints(final AbstractEdge edge) {
        List<Map<String, Object>> points = new ArrayList<>();
        EdgeGraphics graphics = edge.getGraphics();
        if (graphics == null) {
            return points;
        }
        EdgeArrow arrow = graphics.getConnection();
        if (arrow == null) {
            return points;
        }
        for (EdgePoint point : arrow.getPointList()) {
            Map<String, Object> entry = new LinkedHashMap<>();
            entry.put("x", point.getXPos());
            entry.put("y", point.getYPos());
            entry.put("cx", point.getCtrlXPos());
            entry.put("cy", point.getCtrlYPos());
            points.add(entry);
        }
        return points;
    }

    private Map<String, Object> timeoutPayload(final TimeoutEdge edge, final long timeoutMs, final long startedAt) {
        Map<String, Object> payload = new LinkedHashMap<>(edgePayload(edge));
        payload.put("timeoutMs", timeoutMs);
        payload.put("elapsedMs", 0L);
        payload.put("ratio", 0.0d);
        payload.put("startedAt", startedAt);
        return payload;
    }

    private String edgeType(final AbstractEdge edge) {
        if (edge instanceof EpsilonEdge) {
            return "epsilon";
        }
        if (edge instanceof GuargedEdge) {
            return "conditional";
        }
        if (edge instanceof RandomEdge) {
            return "probabilistic";
        }
        if (edge instanceof InterruptEdge) {
            return "interruptive";
        }
        if (edge instanceof TimeoutEdge) {
            return "timeout";
        }
        if (edge instanceof ForkingEdge) {
            return "fork";
        }
        return "unknown";
    }

    private SuperNode resolveEdgeOwner(final AbstractEdge edge) {
        if (edge == null) {
            return null;
        }
        BasicNode source = edge.getSourceNode();
        if (source != null && source.getParentNode() != null) {
            return source.getParentNode();
        }
        BasicNode target = edge.getTargetNode();
        if (target != null && target.getParentNode() != null) {
            return target.getParentNode();
        }
        return null;
    }

    private String resolveEdgeId(final AbstractEdge edge) {
        SuperNode owner = resolveEdgeOwner(edge);
        if (edge == null || owner == null) {
            return "";
        }
        int index = 0;
        for (BasicNode node : owner.getNodeAndSuperNodeList()) {
            for (AbstractEdge candidate : node.getEdgeList()) {
                if (candidate == edge) {
                    return "E" + index;
                }
                index += 1;
            }
        }
        return "";
    }

    private Map<String, Object> scenePayload(final SceneObject scene) {
        Map<String, Object> payload = new LinkedHashMap<>();
        payload.put("sceneName", scene.getName());
        payload.put("language", scene.getLanguage());
        payload.put("lower", scene.getLower());
        payload.put("upper", scene.getUpper());
        return payload;
    }

    private Map<String, Object> turnPayload(final SceneTurn turn) {
        Map<String, Object> payload = new LinkedHashMap<>();
        payload.put("speaker", turn.getSpeaker());
        payload.put("lower", turn.getLower());
        payload.put("upper", turn.getUpper());
        return payload;
    }
}
