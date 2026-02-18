package de.dfki.vsm.web;

import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.json.JSONObject;

import java.util.List;
import java.util.function.Consumer;

/**
 * Handles SceneFlow.Node.Cmd.* commands.
 */
public final class NodeCmdCommandService {

    public interface Context {
        RunTimeProject runtimeProject(String projectId);

        JSONObject errorResponse(String code, String message);

        BasicNode findNodeRecursive(SuperNode root, String nodeId);

        SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId);

        Command parseCommandText(String text, StringBuilder error);

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
            case "SceneFlow.Node.Cmd.Add":
                return handleAdd(params, broadcaster, context);
            case "SceneFlow.Node.Cmd.Update":
                return handleUpdate(params, broadcaster, context);
            case "SceneFlow.Node.Cmd.Delete":
                return handleDelete(params, broadcaster, context);
            case "SceneFlow.Node.Cmd.Move":
                return handleMove(params, broadcaster, context);
            default:
                return context.errorResponse("BAD_REQUEST", "Unsupported cmd command: " + method);
        }
    }

    private JSONObject handleAdd(final JSONObject params,
                                 final Consumer<String> broadcaster,
                                 final Context context) {
        final String pid = params.optString("projectId", "");
        final String nodeId = params.optString("nodeId", "");
        final String superNodeId = params.optString("superNodeId", null);
        final JSONObject commandJson = params.optJSONObject("command");
        final int index = params.has("index") ? params.optInt("index", -1) : -1;

        final RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }
        if (commandJson == null) {
            return context.errorResponse("BAD_REQUEST", "Missing command");
        }

        final SceneFlow sceneFlow = project.getSceneFlow();
        final BasicNode dataNode = nodeId.isBlank() ? sceneFlow : context.findNodeRecursive(sceneFlow, nodeId);
        if (dataNode == null) {
            return context.errorResponse("NODE_NOT_FOUND", "Node not found: " + nodeId);
        }

        final StringBuilder error = new StringBuilder();
        final Command command = context.parseCommandText(commandJson.optString("text", ""), error);
        if (command == null) {
            return context.errorResponse("COMMAND_INVALID", error.length() > 0 ? error.toString() : "Invalid command");
        }

        final List<Command> list = dataNode.getCmdList();
        final int insertIndex = index < 0 || index > list.size() ? list.size() : index;
        list.add(insertIndex, command);

        return snapshotAndRecord(context, project, pid, superNodeId, sceneFlow,
                "SceneFlow.Node.Cmd.Add", params, broadcaster);
    }

    private JSONObject handleUpdate(final JSONObject params,
                                    final Consumer<String> broadcaster,
                                    final Context context) {
        final String pid = params.optString("projectId", "");
        final String nodeId = params.optString("nodeId", "");
        final String superNodeId = params.optString("superNodeId", null);
        final JSONObject commandJson = params.optJSONObject("command");
        final int index = params.optInt("index", -1);

        final RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }
        if (commandJson == null || index < 0) {
            return context.errorResponse("BAD_REQUEST", "Missing command or index");
        }

        final SceneFlow sceneFlow = project.getSceneFlow();
        final BasicNode dataNode = nodeId.isBlank() ? sceneFlow : context.findNodeRecursive(sceneFlow, nodeId);
        if (dataNode == null) {
            return context.errorResponse("NODE_NOT_FOUND", "Node not found: " + nodeId);
        }

        final List<Command> list = dataNode.getCmdList();
        if (index >= list.size()) {
            return context.errorResponse("COMMAND_NOT_FOUND", "Command not found at index: " + index);
        }

        final StringBuilder error = new StringBuilder();
        final Command command = context.parseCommandText(commandJson.optString("text", ""), error);
        if (command == null) {
            return context.errorResponse("COMMAND_INVALID", error.length() > 0 ? error.toString() : "Invalid command");
        }

        list.set(index, command);

        return snapshotAndRecord(context, project, pid, superNodeId, sceneFlow,
                "SceneFlow.Node.Cmd.Update", params, broadcaster);
    }

    private JSONObject handleDelete(final JSONObject params,
                                    final Consumer<String> broadcaster,
                                    final Context context) {
        final String pid = params.optString("projectId", "");
        final String nodeId = params.optString("nodeId", "");
        final String superNodeId = params.optString("superNodeId", null);
        final int index = params.optInt("index", -1);

        final RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }
        if (index < 0) {
            return context.errorResponse("BAD_REQUEST", "Missing index");
        }

        final SceneFlow sceneFlow = project.getSceneFlow();
        final BasicNode dataNode = nodeId.isBlank() ? sceneFlow : context.findNodeRecursive(sceneFlow, nodeId);
        if (dataNode == null) {
            return context.errorResponse("NODE_NOT_FOUND", "Node not found: " + nodeId);
        }

        final List<Command> list = dataNode.getCmdList();
        if (index >= list.size()) {
            return context.errorResponse("COMMAND_NOT_FOUND", "Command not found at index: " + index);
        }

        list.remove(index);

        return snapshotAndRecord(context, project, pid, superNodeId, sceneFlow,
                "SceneFlow.Node.Cmd.Delete", params, broadcaster);
    }

    private JSONObject handleMove(final JSONObject params,
                                  final Consumer<String> broadcaster,
                                  final Context context) {
        final String pid = params.optString("projectId", "");
        final String nodeId = params.optString("nodeId", "");
        final String superNodeId = params.optString("superNodeId", null);
        final int from = params.optInt("from", -1);
        final int to = params.optInt("to", -1);

        final RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }
        if (from < 0 || to < 0) {
            return context.errorResponse("BAD_REQUEST", "Missing from or to index");
        }

        final SceneFlow sceneFlow = project.getSceneFlow();
        final BasicNode dataNode = nodeId.isBlank() ? sceneFlow : context.findNodeRecursive(sceneFlow, nodeId);
        if (dataNode == null) {
            return context.errorResponse("NODE_NOT_FOUND", "Node not found: " + nodeId);
        }

        final List<Command> list = dataNode.getCmdList();
        if (from >= list.size() || to >= list.size()) {
            return context.errorResponse("COMMAND_NOT_FOUND", "Invalid index");
        }

        if (from != to) {
            final Command entry = list.remove(from);
            list.add(to, entry);
        }

        return snapshotAndRecord(context, project, pid, superNodeId, sceneFlow,
                "SceneFlow.Node.Cmd.Move", params, broadcaster);
    }

    private JSONObject snapshotAndRecord(final Context context,
                                         final RunTimeProject project,
                                         final String projectId,
                                         final String superNodeId,
                                         final SceneFlow sceneFlow,
                                         final String command,
                                         final JSONObject params,
                                         final Consumer<String> broadcaster) {
        final SuperNode snapshotTarget = context.resolveSuperNode(sceneFlow, superNodeId);
        final JSONObject snapshot = context.createSceneFlowSnapshot(project, projectId, snapshotTarget, sceneFlow);
        final JSONObject response = context.buildSceneFlowResponse(snapshot);
        context.broadcastSceneFlowSnapshot(broadcaster, projectId, snapshot);
        context.recordHistory(projectId, command);
        context.recordCommand(projectId, command, params);
        return response;
    }
}
