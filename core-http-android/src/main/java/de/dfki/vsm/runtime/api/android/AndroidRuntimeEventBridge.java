package de.dfki.vsm.runtime.api.android;

import de.dfki.vsm.event.EventDispatcher;
import de.dfki.vsm.event.EventListener;
import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.event.event.*;
import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.EpsilonEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.ForkingEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.RandomEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge;
import de.dfki.vsm.model.scenescript.SceneObject;
import de.dfki.vsm.model.scenescript.SceneTurn;
import de.dfki.vsm.runtime.interpreter.event.TerminationEvent;
import de.dfki.vsm.util.tpl.Tuple;
import org.json.JSONObject;

import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * Bridges core runtime events into WebSocket event envelopes for Android-hosted clients.
 */
public final class AndroidRuntimeEventBridge implements EventListener {

    private final Supplier<String> projectIdSupplier;
    private final Supplier<Consumer<String>> broadcasterSupplier;

    public AndroidRuntimeEventBridge(final Supplier<String> projectIdSupplier,
                                     final Supplier<Consumer<String>> broadcasterSupplier) {
        this.projectIdSupplier = Objects.requireNonNull(projectIdSupplier, "projectIdSupplier");
        this.broadcasterSupplier = Objects.requireNonNull(broadcasterSupplier, "broadcasterSupplier");
    }

    public void start() {
        EventDispatcher.getInstance().register(this);
    }

    public void stop() {
        EventDispatcher.getInstance().remove(this);
    }

    @Override
    public void update(final EventObject event) {
        if (event == null) {
            return;
        }
        Consumer<String> broadcaster = broadcasterSupplier.get();
        if (broadcaster == null) {
            return;
        }

        String projectId = projectIdSupplier.get();
        JSONObject payload = new JSONObject();
        if (projectId != null && !projectId.isBlank()) {
            payload.put("projectId", projectId);
        }

        if (event instanceof VariableChangedEvent) {
            Tuple<String, String> pair = ((VariableChangedEvent) event).getVarValue();
            if (pair == null || pair.getFirst() == null || pair.getFirst().isBlank()) {
                return;
            }
            payload.put("name", pair.getFirst());
            payload.put("value", pair.getSecond() == null ? "" : pair.getSecond());
            emitEvent(broadcaster, "vars", "vars.updated", payload);
            return;
        }

        if (event instanceof NodeStartedEvent) {
            BasicNode node = ((NodeStartedEvent) event).getNode();
            if (node == null) return;
            payload.put("nodeId", node.getId());
            if (node.getParentNode() != null) {
                payload.put("parentId", node.getParentNode().getId());
            }
            emitEvent(broadcaster, "runtime", "runtime.nodeActive", payload);
            return;
        }

        if (event instanceof NodeExecutedEvent || event instanceof NodeTerminatedEvent) {
            BasicNode node = event instanceof NodeExecutedEvent
                    ? ((NodeExecutedEvent) event).getNode()
                    : ((NodeTerminatedEvent) event).getNode();
            if (node == null) return;
            payload.put("nodeId", node.getId());
            if (node.getParentNode() != null) {
                payload.put("parentId", node.getParentNode().getId());
            }
            emitEvent(broadcaster, "runtime", "runtime.nodeStopped", payload);
            return;
        }

        if (event instanceof EdgeExecutedEvent) {
            AbstractEdge edge = ((EdgeExecutedEvent) event).getEdge();
            if (edge == null) return;
            payload.put("edgeId", AndroidSceneFlowSnapshotBuilder.edgeRuntimeId(edge));
            String sourceId = edge.getSourceUnid() == null ? "" : edge.getSourceUnid();
            if (sourceId == null || sourceId.isBlank()) {
                sourceId = edge.getSourceNode() != null ? edge.getSourceNode().getId() : "";
            }
            String targetId = edge.getTargetUnid() == null ? "" : edge.getTargetUnid();
            if (targetId == null || targetId.isBlank()) {
                targetId = edge.getTargetNode() != null ? edge.getTargetNode().getId() : "";
            }
            payload.put("sourceId", sourceId);
            payload.put("targetId", targetId);
            String edgeType = getEdgeTypeCode(edge);
            payload.put("edgeType", getEdgeTypeLowercase(edge));
            payload.put("edgeTypeCode", edgeType);
            payload.put("id", payload.optString("edgeId", ""));
            if (edge instanceof TimeoutEdge) {
                long timeoutMs = ((TimeoutEdge) edge).getTimeout();
                if (timeoutMs > 0L) {
                    payload.put("timeoutMs", timeoutMs);
                }
            }
            emitEvent(broadcaster, "runtime", "runtime.edgeActive", payload);
            return;
        }

        if (event instanceof TimeoutEdgeStartedEvent) {
            TimeoutEdgeStartedEvent te = (TimeoutEdgeStartedEvent) event;
            TimeoutEdge edge = te.getEdge();
            if (edge == null) return;
            payload.put("edgeId", AndroidSceneFlowSnapshotBuilder.edgeRuntimeId(edge));
            String sourceId = edge.getSourceUnid() == null ? "" : edge.getSourceUnid();
            if (sourceId == null || sourceId.isBlank()) {
                sourceId = edge.getSourceNode() != null ? edge.getSourceNode().getId() : "";
            }
            String targetId = edge.getTargetUnid() == null ? "" : edge.getTargetUnid();
            if (targetId == null || targetId.isBlank()) {
                targetId = edge.getTargetNode() != null ? edge.getTargetNode().getId() : "";
            }
            payload.put("sourceId", sourceId);
            payload.put("targetId", targetId);
            payload.put("edgeType", "timeout");
            payload.put("edgeTypeCode", "TEDGE");
            payload.put("id", payload.optString("edgeId", ""));
            payload.put("timeoutMs", te.getTimeoutMs());
            payload.put("elapsedMs", 0L);
            payload.put("ratio", 0.0);
            // Also emit runtime.edgeActive for timeout edges so clients using the generic
            // edge highlight path visualize T-edges consistently.
            emitEvent(broadcaster, "runtime", "runtime.edgeActive", payload);
            emitEvent(broadcaster, "runtime", "runtime.timeoutProgress", payload);
            return;
        }

        if (event instanceof SceneExecutedEvent) {
            SceneObject scene = ((SceneExecutedEvent) event).getScene();
            if (scene == null) return;
            payload.put("sceneName", scene.getName());
            payload.put("language", scene.getLanguage());
            payload.put("lower", scene.getLower());
            payload.put("upper", scene.getUpper());
            emitEvent(broadcaster, "runtime", "runtime.scene.playing", payload);
            return;
        }

        if (event instanceof SceneDoneEvent) {
            SceneObject scene = ((SceneDoneEvent) event).getScene();
            if (scene == null) return;
            payload.put("sceneName", scene.getName());
            payload.put("language", scene.getLanguage());
            payload.put("lower", scene.getLower());
            payload.put("upper", scene.getUpper());
            emitEvent(broadcaster, "runtime", "runtime.scene.done", payload);
            return;
        }

        if (event instanceof TurnExecutedEvent) {
            SceneTurn turn = ((TurnExecutedEvent) event).getTurn();
            if (turn == null) return;
            payload.put("speaker", turn.getSpeaker());
            payload.put("lower", turn.getLower());
            payload.put("upper", turn.getUpper());
            emitEvent(broadcaster, "runtime", "runtime.scene.turn", payload);
            return;
        }

        if (event instanceof TurnDoneEvent) {
            SceneTurn turn = ((TurnDoneEvent) event).getTurn();
            if (turn == null) return;
            payload.put("speaker", turn.getSpeaker());
            payload.put("lower", turn.getLower());
            payload.put("upper", turn.getUpper());
            emitEvent(broadcaster, "runtime", "runtime.scene.turnDone", payload);
            return;
        }

        if (event instanceof SceneStoppedEvent || event instanceof TerminationEvent) {
            payload.put("state", "stopped");
            payload.put("status", "stopped");
            emitEvent(broadcaster, "runtime", "runtime.state", payload);
        }
    }

    private void emitEvent(final Consumer<String> broadcaster,
                           final String channel,
                           final String event,
                           final JSONObject payload) {
        JSONObject message = new JSONObject();
        message.put("type", "event");
        message.put("ts", System.currentTimeMillis());
        message.put("channel", channel);
        message.put("event", event);
        message.put("payload", payload == null ? new JSONObject() : payload);
        broadcaster.accept(message.toString());
    }

    private String getEdgeTypeCode(final AbstractEdge edge) {
        if (edge instanceof GuargedEdge) return "CEDGE";
        if (edge instanceof RandomEdge) return "PEDGE";
        if (edge instanceof InterruptEdge) return "IEDGE";
        if (edge instanceof ForkingEdge) return "FEDGE";
        if (edge instanceof TimeoutEdge) return "TEDGE";
        if (edge instanceof EpsilonEdge) return "EEDGE";
        return "EEDGE";
    }

    private String getEdgeTypeLowercase(final AbstractEdge edge) {
        if (edge instanceof EpsilonEdge) return "epsilon";
        if (edge instanceof GuargedEdge) return "conditional";
        if (edge instanceof RandomEdge) return "probabilistic";
        if (edge instanceof InterruptEdge) return "interruptive";
        if (edge instanceof TimeoutEdge) return "timeout";
        if (edge instanceof ForkingEdge) return "fork";
        return "unknown";
    }
}
