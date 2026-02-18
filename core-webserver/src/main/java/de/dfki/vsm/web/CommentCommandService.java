package de.dfki.vsm.web;

import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.badge.CommentBadge;
import de.dfki.vsm.model.sceneflow.chart.graphics.comment.CommentBoundary;
import de.dfki.vsm.model.sceneflow.chart.graphics.comment.CommentGraphics;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.json.JSONObject;

import java.util.function.Consumer;

/**
 * Handles SceneFlow.Comment.* commands.
 */
public final class CommentCommandService {

    public interface Context {
        RunTimeProject runtimeProject(String projectId);

        JSONObject mutateAndSnapshotLegacy(String projectId, String operation, JSONObject params, Consumer<String> broadcaster);

        JSONObject errorResponse(String code, String message);

        SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId);

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
            case "SceneFlow.Comment.Add":
            case "SceneFlow.Comment.Create":
                return createCommentForProject(params, broadcaster, context);
            case "SceneFlow.Comment.Update":
                return updateCommentForProject(params, broadcaster, context);
            case "SceneFlow.Comment.Delete":
                return deleteCommentForProject(params, broadcaster, context);
            default:
                return context.errorResponse("BAD_REQUEST", "Unsupported comment command: " + method);
        }
    }

    private JSONObject createCommentForProject(final JSONObject params,
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

        CommentBadge comment = new CommentBadge();
        comment.setParentNode(activeSuperNode);
        CommentBoundary rect = new CommentBoundary(
                safeRound(params.has("x") ? params.optDouble("x") : null, 0),
                safeRound(params.has("y") ? params.optDouble("y") : null, 0),
                200,
                120
        );
        comment.setGraphics(new CommentGraphics(rect));
        comment.setHTMLText(params.optString("text", ""));
        activeSuperNode.getCommentList().add(comment);

        JSONObject snapshot = context.createSceneFlowSnapshot(project, pid, snapshotTarget, sceneFlow);
        JSONObject response = context.buildSceneFlowResponse(snapshot);
        String commentId = "C" + Math.max(0, activeSuperNode.getCommentList().size() - 1);
        response.put("commentId", commentId);
        context.broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
        context.recordHistory(pid, "SceneFlow.Comment.Create");
        context.recordCommand(pid, "SceneFlow.Comment.Create", params);
        return response;
    }

    private JSONObject updateCommentForProject(final JSONObject params,
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
        String commentId = params.optString("commentId", "");
        if (commentId.isBlank()) {
            return context.errorResponse("BAD_REQUEST", "Missing commentId");
        }

        CommentBadge comment = resolveCommentById(activeSuperNode, commentId);
        if (comment == null) {
            return context.errorResponse("COMMENT_NOT_FOUND", "Comment not found: " + commentId);
        }

        if (params.has("text")) {
            comment.setHTMLText(params.optString("text", ""));
        }
        CommentGraphics cg = comment.getGraphics();
        if (cg == null) {
            cg = new CommentGraphics();
            comment.setGraphics(cg);
        }
        CommentBoundary boundary = cg.getRectangle();
        if (boundary == null) {
            boundary = new CommentBoundary();
            cg.setRectangle(boundary);
        }
        if (params.has("x")) {
            boundary.setXPos(safeRound(params.optDouble("x"), boundary.getXPos()));
        }
        if (params.has("y")) {
            boundary.setYPos(safeRound(params.optDouble("y"), boundary.getYPos()));
        }
        if (params.has("width")) {
            boundary.setWidth(safeRound(params.optDouble("width"), boundary.getWidth()));
        }
        if (params.has("height")) {
            boundary.setHeight(safeRound(params.optDouble("height"), boundary.getHeight()));
        }
        JSONObject rect = params.optJSONObject("rect");
        if (rect != null) {
            boundary.setXPos(safeRound(rect.has("x") ? rect.optDouble("x") : null, boundary.getXPos()));
            boundary.setYPos(safeRound(rect.has("y") ? rect.optDouble("y") : null, boundary.getYPos()));
            boundary.setWidth(safeRound(rect.has("w") ? rect.optDouble("w") : null, boundary.getWidth()));
            boundary.setHeight(safeRound(rect.has("h") ? rect.optDouble("h") : null, boundary.getHeight()));
        }

        JSONObject snapshot = context.createSceneFlowSnapshot(project, pid, snapshotTarget, sceneFlow);
        JSONObject response = context.buildSceneFlowResponse(snapshot);
        context.broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
        context.recordHistory(pid, "SceneFlow.Comment.Update");
        context.recordCommand(pid, "SceneFlow.Comment.Update", params);
        return response;
    }

    private JSONObject deleteCommentForProject(final JSONObject params,
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
        String commentId = params.optString("commentId", "");
        if (commentId.isBlank()) {
            return context.errorResponse("BAD_REQUEST", "Missing commentId");
        }

        CommentBadge comment = resolveCommentById(activeSuperNode, commentId);
        if (comment == null) {
            return context.errorResponse("COMMENT_NOT_FOUND", "Comment not found: " + commentId);
        }
        activeSuperNode.getCommentList().remove(comment);

        JSONObject snapshot = context.createSceneFlowSnapshot(project, pid, snapshotTarget, sceneFlow);
        JSONObject response = context.buildSceneFlowResponse(snapshot);
        context.broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
        context.recordHistory(pid, "SceneFlow.Comment.Delete");
        context.recordCommand(pid, "SceneFlow.Comment.Delete", params);
        return response;
    }

    private CommentBadge resolveCommentById(final SuperNode superNode, final String commentId) {
        if (superNode == null || commentId == null) {
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
        if (index < 0 || index >= superNode.getCommentList().size()) {
            return null;
        }
        return superNode.getCommentList().get(index);
    }

    private int safeRound(final Double value, final int fallback) {
        if (value == null || Double.isNaN(value) || Double.isInfinite(value)) {
            return fallback;
        }
        return (int) Math.round(value);
    }
}
