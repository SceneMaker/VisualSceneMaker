package de.dfki.vsm.web;

import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.json.JSONObject;

import java.util.List;
import java.util.function.Consumer;

/**
 * Handles SceneFlow.Node.VarDef.* commands.
 */
public final class NodeVarDefCommandService {

    public interface Context {
        RunTimeProject runtimeProject(String projectId);

        JSONObject errorResponse(String code, String message);

        BasicNode findNodeRecursive(SuperNode root, String nodeId);

        SuperNode resolveSuperNode(SceneFlow sceneFlow, String superNodeId);

        VariableDefinition parseVarDef(JSONObject source, BasicNode node, StringBuilder error);

        JSONObject createSceneFlowSnapshot(RunTimeProject project, String projectId, SuperNode snapshotTarget, SceneFlow sceneFlow);

        JSONObject buildSceneFlowResponse(JSONObject snapshot);

        void broadcastSceneFlowSnapshot(Consumer<String> broadcaster, String projectId, JSONObject snapshot);

        int renameVariableReferences(SuperNode root, String oldName, String newName);

        void recordHistory(String projectId, String action);

        void recordCommand(String projectId, String action, JSONObject params);
    }

    public JSONObject dispatch(final String method,
                               final JSONObject params,
                               final Consumer<String> broadcaster,
                               final Context context) {
        switch (method) {
            case "SceneFlow.Node.VarDef.Add":
                return handleAdd(params, broadcaster, context);
            case "SceneFlow.Node.VarDef.Update":
                return handleUpdate(params, broadcaster, context);
            case "SceneFlow.Node.VarDef.Delete":
                return handleDelete(params, broadcaster, context);
            case "SceneFlow.Node.VarDef.Move":
                return handleMove(params, broadcaster, context);
            default:
                return context.errorResponse("BAD_REQUEST", "Unsupported var-def command: " + method);
        }
    }

    private JSONObject handleAdd(final JSONObject params,
                                 final Consumer<String> broadcaster,
                                 final Context context) {
        final String pid = params.optString("projectId", "");
        final String nodeId = params.optString("nodeId", "");
        final String superNodeId = params.optString("superNodeId", null);
        final JSONObject varDefJson = params.optJSONObject("varDef");
        final int index = params.has("index") ? params.optInt("index", -1) : -1;

        final RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }
        if (varDefJson == null) {
            return context.errorResponse("BAD_REQUEST", "Missing varDef");
        }

        final SceneFlow sceneFlow = project.getSceneFlow();
        final BasicNode dataNode = nodeId.isBlank() ? sceneFlow : context.findNodeRecursive(sceneFlow, nodeId);
        if (dataNode == null) {
            return context.errorResponse("NODE_NOT_FOUND", "Node not found: " + nodeId);
        }

        final StringBuilder error = new StringBuilder();
        final VariableDefinition varDef = context.parseVarDef(varDefJson, dataNode, error);
        if (varDef == null) {
            return context.errorResponse("VARDEF_INVALID", error.length() > 0 ? error.toString() : "Invalid variable definition");
        }

        final List<VariableDefinition> list = dataNode.getVarDefList();
        // A duplicate name here would only surface later, as Environment.create() throwing
        // "Variable already defined" the first time Runtime.Play processes the Declare list —
        // silently aborting the interpreter thread before any node ever runs. Reject it now
        // instead, at the point that would introduce it.
        final boolean duplicate = list.stream()
                .anyMatch(existing -> existing.getName() != null && existing.getName().equals(varDef.getName()));
        if (duplicate) {
            return context.errorResponse("VARDEF_DUPLICATE",
                    "A variable named '" + varDef.getName() + "' already exists on this node");
        }
        final int insertIndex = index < 0 || index > list.size() ? list.size() : index;
        list.add(insertIndex, varDef);

        return snapshotAndRecord(context, project, pid, superNodeId, sceneFlow,
                "SceneFlow.Node.VarDef.Add", params, broadcaster);
    }

    private JSONObject handleUpdate(final JSONObject params,
                                    final Consumer<String> broadcaster,
                                    final Context context) {
        final String pid = params.optString("projectId", "");
        final String nodeId = params.optString("nodeId", "");
        final String superNodeId = params.optString("superNodeId", null);
        final JSONObject varDefJson = params.optJSONObject("varDef");
        final int index = params.optInt("index", -1);

        final RunTimeProject project = context.runtimeProject(pid);
        if (project == null) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }
        if (varDefJson == null || index < 0) {
            return context.errorResponse("BAD_REQUEST", "Missing varDef or index");
        }

        final SceneFlow sceneFlow = project.getSceneFlow();
        final BasicNode dataNode = nodeId.isBlank() ? sceneFlow : context.findNodeRecursive(sceneFlow, nodeId);
        if (dataNode == null) {
            return context.errorResponse("NODE_NOT_FOUND", "Node not found: " + nodeId);
        }

        final List<VariableDefinition> list = dataNode.getVarDefList();
        if (index >= list.size()) {
            return context.errorResponse("VARDEF_NOT_FOUND", "Variable definition not found at index: " + index);
        }
        final VariableDefinition current = list.get(index);
        final String oldName = current != null ? current.getName() : "";

        final StringBuilder error = new StringBuilder();
        final VariableDefinition varDef = context.parseVarDef(varDefJson, dataNode, error);
        if (varDef == null) {
            return context.errorResponse("VARDEF_INVALID", error.length() > 0 ? error.toString() : "Invalid variable definition");
        }

        list.set(index, varDef);
        int renamedReferences = 0;
        final String newName = varDef.getName();
        if (oldName != null && !oldName.isBlank() && newName != null && !newName.isBlank() && !oldName.equals(newName)) {
            renamedReferences = context.renameVariableReferences(sceneFlow, oldName, newName);
        }

        final JSONObject response = snapshotAndRecord(context, project, pid, superNodeId, sceneFlow,
                "SceneFlow.Node.VarDef.Update", params, broadcaster);
        response.put("scriptChanged", renamedReferences > 0);
        response.put("renamedReferences", renamedReferences);
        return response;
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

        final List<VariableDefinition> list = dataNode.getVarDefList();
        if (index >= list.size()) {
            return context.errorResponse("VARDEF_NOT_FOUND", "Variable definition not found at index: " + index);
        }

        list.remove(index);

        return snapshotAndRecord(context, project, pid, superNodeId, sceneFlow,
                "SceneFlow.Node.VarDef.Delete", params, broadcaster);
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

        final List<VariableDefinition> list = dataNode.getVarDefList();
        if (from >= list.size() || to >= list.size()) {
            return context.errorResponse("VARDEF_NOT_FOUND", "Invalid index");
        }

        if (from != to) {
            final VariableDefinition entry = list.remove(from);
            list.add(to, entry);
        }

        return snapshotAndRecord(context, project, pid, superNodeId, sceneFlow,
                "SceneFlow.Node.VarDef.Move", params, broadcaster);
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
