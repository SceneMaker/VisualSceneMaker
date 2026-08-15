package de.dfki.vsm.sceneflow.ir;

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
import de.dfki.vsm.model.sceneflow.chart.graphics.node.NodeGraphics;
import de.dfki.vsm.model.sceneflow.glue.GlueParser;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public final class SceneFlowIrPatchApplier {

    private static final Set<String> SUPPORTED_EDGE_TYPES =
            Set.of("EEDGE", "CEDGE", "PEDGE", "TEDGE", "FEDGE", "IEDGE");

    public SceneFlow apply(final JSONObject ir, final SceneFlow baseFlow) throws SceneFlowIrCompileException {
        if (ir == null) {
            throw new SceneFlowIrCompileException("IR document is null.");
        }
        if (baseFlow == null) {
            throw new SceneFlowIrCompileException("Base SceneFlow model is null.");
        }
        final JSONArray operations = ir.optJSONArray("operations");
        if (operations == null || operations.isEmpty()) {
            throw new SceneFlowIrCompileException("IR operations list is missing or empty.");
        }

        final SceneFlow working = baseFlow.getCopy();
        final GraphContext context = GraphContext.from(working);

        for (int i = 0; i < operations.length(); i++) {
            final JSONObject op = operations.optJSONObject(i);
            if (op == null) {
                throw new SceneFlowIrCompileException("Operation at index " + i + " is not an object.");
            }
            applyOperation(op, i, context);
        }

        return working;
    }

    private void applyOperation(final JSONObject op, final int index, final GraphContext context)
            throws SceneFlowIrCompileException {
        final String kind = op.optString("op", "").trim();
        if (kind.isEmpty()) {
            throw new SceneFlowIrCompileException("Operation " + index + " is missing 'op'.");
        }
        switch (kind) {
            case "create_supernode":
                applyCreateSuperNode(op, index, context);
                break;
            case "create_node":
                applyCreateNode(op, index, context);
                break;
            case "update_node":
                applyUpdateNode(op, index, context);
                break;
            case "delete_node":
                applyDeleteNode(op, index, context);
                break;
            case "create_edge":
                applyCreateEdge(op, index, context);
                break;
            case "update_edge":
                applyUpdateEdge(op, index, context);
                break;
            case "delete_edge":
                applyDeleteEdge(op, index, context);
                break;
            case "add_node_command":
                applyAddNodeCommand(op, index, context);
                break;
            case "update_node_command":
                applyUpdateNodeCommand(op, index, context);
                break;
            case "delete_node_command":
                applyDeleteNodeCommand(op, index, context);
                break;
            case "add_variable_definition":
                applyAddVarDef(op, index, context);
                break;
            case "update_variable_definition":
                applyUpdateVarDef(op, index, context);
                break;
            case "delete_variable_definition":
                applyDeleteVarDef(op, index, context);
                break;
            default:
                throw new SceneFlowIrCompileException("Unsupported operation at index " + index + ": " + kind);
        }
    }

    private void applyCreateSuperNode(final JSONObject op, final int index, final GraphContext context)
            throws SceneFlowIrCompileException {
        final String parentId = requiredString(op, "parentSuperNodeId", index);
        final String id = requiredString(op, "superNodeId", index);
        final String name = requiredString(op, "name", index);
        if (context.nodeById.containsKey(id)) {
            throw new SceneFlowIrCompileException("Operation " + index + ": supernode id already exists: " + id);
        }
        final BasicNode parentNode = "SceneFlow".equals(parentId) ? context.root : context.nodeById.get(parentId);
        if (!(parentNode instanceof SuperNode)) {
            throw new SceneFlowIrCompileException("Operation " + index + ": parent supernode not found: " + parentId);
        }
        final SuperNode parent = (SuperNode) parentNode;
        final SuperNode node = new SuperNode();
        node.setId(id);
        node.setName(name);
        node.setComment(op.optString("comment", ""));
        node.setParentNode(parent);
        applyPosition(op.optJSONObject("position"), node);
        parent.addSuperNode(node);
        if (op.optBoolean("isStartNode", false)) {
            parent.addStartNode(node);
        }
        context.addNode(node, parent);
    }

    private void applyCreateNode(final JSONObject op, final int index, final GraphContext context)
            throws SceneFlowIrCompileException {
        final String parentId = requiredString(op, "parentSuperNodeId", index);
        final String id = requiredString(op, "nodeId", index);
        final String name = requiredString(op, "name", index);
        if (context.nodeById.containsKey(id)) {
            throw new SceneFlowIrCompileException("Operation " + index + ": node id already exists: " + id);
        }
        final BasicNode parentNode = "SceneFlow".equals(parentId) ? context.root : context.nodeById.get(parentId);
        if (!(parentNode instanceof SuperNode)) {
            throw new SceneFlowIrCompileException("Operation " + index + ": parent supernode not found: " + parentId);
        }
        final SuperNode parent = (SuperNode) parentNode;
        final BasicNode node = new BasicNode();
        node.setId(id);
        node.setName(name);
        node.setComment(op.optString("comment", ""));
        node.setHistoryNodeFlag(op.optBoolean("isHistoryNode", false));
        node.setParentNode(parent);
        applyPosition(op.optJSONObject("position"), node);
        parent.addNode(node);
        if (op.optBoolean("isStartNode", false)) {
            parent.addStartNode(node);
        }
        context.addNode(node, parent);
    }

    private void applyUpdateNode(final JSONObject op, final int index, final GraphContext context)
            throws SceneFlowIrCompileException {
        final String id = requiredString(op, "nodeId", index);
        final BasicNode node = context.requireNode(id, index);
        if (op.has("name")) {
            node.setName(op.optString("name", node.getName()));
        }
        if (op.has("comment")) {
            node.setComment(op.optString("comment", node.getComment()));
        }
        if (op.has("position")) {
            applyPosition(op.optJSONObject("position"), node);
        }
        if (op.has("isHistoryNode")) {
            node.setHistoryNodeFlag(op.optBoolean("isHistoryNode", false));
        }
        if (op.has("isStartNode")) {
            final SuperNode parent = context.parentByNodeId.get(id);
            if (parent != null) {
                final boolean start = op.optBoolean("isStartNode", false);
                if (start) {
                    parent.addStartNode(node);
                } else {
                    parent.removeStartNode(node);
                }
            }
        }
    }

    private void applyDeleteNode(final JSONObject op, final int index, final GraphContext context)
            throws SceneFlowIrCompileException {
        final String id = requiredString(op, "nodeId", index);
        if (id.equals(context.root.getId())) {
            throw new SceneFlowIrCompileException("Operation " + index + ": cannot delete SceneFlow root node.");
        }
        final BasicNode node = context.requireNode(id, index);
        final SuperNode parent = context.parentByNodeId.get(id);
        if (parent == null) {
            throw new SceneFlowIrCompileException("Operation " + index + ": node has no parent: " + id);
        }

        final Set<String> removedIds = new HashSet<>();
        collectNodeIds(node, removedIds);
        context.removeEdgesReferencing(removedIds);

        if (node instanceof SuperNode) {
            parent.removeSuperNode((SuperNode) node);
        } else {
            parent.removeNode(node);
        }
        parent.removeStartNode(node);
        context.removeNodes(removedIds);
    }

    private void applyCreateEdge(final JSONObject op, final int index, final GraphContext context)
            throws SceneFlowIrCompileException {
        final String edgeId = requiredString(op, "edgeId", index);
        if (context.edgeById.containsKey(edgeId)) {
            throw new SceneFlowIrCompileException("Operation " + index + ": edge id already exists in patch context: " + edgeId);
        }
        final String edgeType = requiredString(op, "edgeType", index);
        if (!SUPPORTED_EDGE_TYPES.contains(edgeType)) {
            throw new SceneFlowIrCompileException("Operation " + index + ": unsupported edge type: " + edgeType);
        }
        final BasicNode source = context.requireNode(requiredString(op, "sourceNodeId", index), index);
        final BasicNode target = context.requireNode(requiredString(op, "targetNodeId", index), index);
        final JSONObject payload = op.optJSONObject("payload");

        final List<Command> commands = parseCommandsFromPayload(payload, index);
        final Map altMap = new HashMap<>();
        final AbstractEdge edge;
        switch (edgeType) {
            case "EEDGE":
                edge = new EpsilonEdge(target.getId(), source.getId(), target, source, null, commands, altMap);
                attachDefaultEdge(source, edge, index);
                break;
            case "TEDGE":
                final int timeoutMs = payload != null ? payload.optInt("timeoutMs", -1) : -1;
                if (timeoutMs < 0) {
                    throw new SceneFlowIrCompileException("Operation " + index + ": TEDGE requires payload.timeoutMs >= 0.");
                }
                edge = new TimeoutEdge(target.getId(), source.getId(), target, source, null, commands, altMap, timeoutMs);
                attachDefaultEdge(source, edge, index);
                break;
            case "CEDGE":
                edge = new GuargedEdge(target.getId(), source.getId(), target, source, null, commands, altMap,
                        parseCondition(payload, index));
                source.addCEdge((GuargedEdge) edge);
                break;
            case "IEDGE":
                edge = new InterruptEdge(target.getId(), source.getId(), target, source, null, commands, altMap,
                        parseCondition(payload, index));
                source.addIEdge((InterruptEdge) edge);
                break;
            case "PEDGE":
                final int probability = payload != null ? payload.optInt("probability", -1) : -1;
                if (probability < 0 || probability > 100) {
                    throw new SceneFlowIrCompileException("Operation " + index + ": PEDGE requires payload.probability in [0,100].");
                }
                edge = new RandomEdge(target.getId(), source.getId(), target, source, null, commands, altMap, probability);
                source.addPEdge((RandomEdge) edge);
                break;
            case "FEDGE":
                edge = new ForkingEdge(target.getId(), source.getId(), target, source, null, commands, altMap);
                source.addFEdge((ForkingEdge) edge);
                break;
            default:
                throw new SceneFlowIrCompileException("Operation " + index + ": unsupported edge type: " + edgeType);
        }

        context.edgeById.put(edgeId, new EdgeRef(source, edge));
    }

    private void applyUpdateEdge(final JSONObject op, final int index, final GraphContext context)
            throws SceneFlowIrCompileException {
        final String edgeId = requiredString(op, "edgeId", index);
        final EdgeRef ref = context.edgeById.get(edgeId);
        if (ref == null) {
            throw new SceneFlowIrCompileException("Operation " + index + ": unknown edge id in patch context: " + edgeId);
        }
        if (op.has("targetNodeId")) {
            final BasicNode target = context.requireNode(op.optString("targetNodeId", ""), index);
            ref.edge.setTargetUnid(target.getId());
            ref.edge.setTargetNode(target);
        }
        final JSONObject payload = op.optJSONObject("payload");
        if (payload == null) {
            return;
        }

        if (payload.has("commands")) {
            ref.edge.setCmdList(parseCommandsFromPayload(payload, index));
        }
        if (ref.edge instanceof TimeoutEdge && payload.has("timeoutMs")) {
            final int timeoutMs = payload.optInt("timeoutMs", -1);
            if (timeoutMs < 0) {
                throw new SceneFlowIrCompileException("Operation " + index + ": timeoutMs must be >= 0.");
            }
            ((TimeoutEdge) ref.edge).setTimeout(timeoutMs);
        }
        if (ref.edge instanceof RandomEdge && payload.has("probability")) {
            final int probability = payload.optInt("probability", -1);
            if (probability < 0 || probability > 100) {
                throw new SceneFlowIrCompileException("Operation " + index + ": probability must be in [0,100].");
            }
            ((RandomEdge) ref.edge).setProbability(probability);
        }
        if ((ref.edge instanceof GuargedEdge || ref.edge instanceof InterruptEdge) && payload.has("conditionText")) {
            final Expression condition = parseCondition(payload, index);
            if (ref.edge instanceof GuargedEdge) {
                ((GuargedEdge) ref.edge).setCondition(condition);
            } else {
                ((InterruptEdge) ref.edge).setCondition(condition);
            }
        }
    }

    private void applyDeleteEdge(final JSONObject op, final int index, final GraphContext context)
            throws SceneFlowIrCompileException {
        final String edgeId = requiredString(op, "edgeId", index);
        final EdgeRef ref = context.edgeById.remove(edgeId);
        if (ref == null) {
            throw new SceneFlowIrCompileException("Operation " + index + ": unknown edge id in patch context: " + edgeId);
        }
        final BasicNode source = ref.source;
        if (ref.edge instanceof EpsilonEdge || ref.edge instanceof TimeoutEdge) {
            if (source.getDedge() == ref.edge) {
                source.removeDEdge();
            }
            return;
        }
        if (ref.edge instanceof GuargedEdge) {
            source.removeCEdge((GuargedEdge) ref.edge);
            return;
        }
        if (ref.edge instanceof InterruptEdge) {
            source.removeIEdge((InterruptEdge) ref.edge);
            return;
        }
        if (ref.edge instanceof RandomEdge) {
            source.removePEdge((RandomEdge) ref.edge);
            return;
        }
        if (ref.edge instanceof ForkingEdge) {
            source.removeFEdge((ForkingEdge) ref.edge);
        }
    }

    private void applyAddNodeCommand(final JSONObject op, final int index, final GraphContext context)
            throws SceneFlowIrCompileException {
        final BasicNode node = context.requireNode(requiredString(op, "nodeId", index), index);
        final Command command = parseCommand(op.optString("commandText", ""), index, "commandText");
        if (op.has("index")) {
            final int cmdIndex = op.optInt("index", -1);
            if (cmdIndex < 0 || cmdIndex > node.getSizeOfCmdList()) {
                throw new SceneFlowIrCompileException("Operation " + index + ": command index out of range: " + cmdIndex);
            }
            node.getCmdList().add(cmdIndex, command);
            return;
        }
        node.addCmd(command);
    }

    private void applyUpdateNodeCommand(final JSONObject op, final int index, final GraphContext context)
            throws SceneFlowIrCompileException {
        final BasicNode node = context.requireNode(requiredString(op, "nodeId", index), index);
        final int cmdIndex = op.optInt("index", -1);
        if (cmdIndex < 0 || cmdIndex >= node.getSizeOfCmdList()) {
            throw new SceneFlowIrCompileException("Operation " + index + ": command index out of range: " + cmdIndex);
        }
        final Command command = parseCommand(op.optString("commandText", ""), index, "commandText");
        node.setCmdAt(command, cmdIndex);
    }

    private void applyDeleteNodeCommand(final JSONObject op, final int index, final GraphContext context)
            throws SceneFlowIrCompileException {
        final BasicNode node = context.requireNode(requiredString(op, "nodeId", index), index);
        final int cmdIndex = op.optInt("index", -1);
        if (cmdIndex < 0 || cmdIndex >= node.getSizeOfCmdList()) {
            throw new SceneFlowIrCompileException("Operation " + index + ": command index out of range: " + cmdIndex);
        }
        node.removeCmdAt(cmdIndex);
    }

    private void applyAddVarDef(final JSONObject op, final int index, final GraphContext context)
            throws SceneFlowIrCompileException {
        final BasicNode owner = context.requireNode(requiredString(op, "ownerNodeId", index), index);
        final VariableDefinition varDef = parseVarDef(op.optJSONObject("varDef"), index);
        if (op.has("index")) {
            final int varIndex = op.optInt("index", -1);
            if (varIndex < 0 || varIndex > owner.getVarDefList().size()) {
                throw new SceneFlowIrCompileException("Operation " + index + ": varDef index out of range: " + varIndex);
            }
            owner.getVarDefList().add(varIndex, varDef);
            return;
        }
        owner.addVarDef(varDef);
    }

    private void applyUpdateVarDef(final JSONObject op, final int index, final GraphContext context)
            throws SceneFlowIrCompileException {
        final BasicNode owner = context.requireNode(requiredString(op, "ownerNodeId", index), index);
        final int varIndex = op.optInt("index", -1);
        if (varIndex < 0 || varIndex >= owner.getVarDefList().size()) {
            throw new SceneFlowIrCompileException("Operation " + index + ": varDef index out of range: " + varIndex);
        }
        owner.setVarDefAt(parseVarDef(op.optJSONObject("varDef"), index), varIndex);
    }

    private void applyDeleteVarDef(final JSONObject op, final int index, final GraphContext context)
            throws SceneFlowIrCompileException {
        final BasicNode owner = context.requireNode(requiredString(op, "ownerNodeId", index), index);
        final int varIndex = op.optInt("index", -1);
        if (varIndex < 0 || varIndex >= owner.getVarDefList().size()) {
            throw new SceneFlowIrCompileException("Operation " + index + ": varDef index out of range: " + varIndex);
        }
        owner.removeVarDefAt(varIndex);
    }

    private void applyPosition(final JSONObject position, final BasicNode node) {
        if (position == null) {
            return;
        }
        final int x = (int) Math.round(position.optDouble("x", Integer.MIN_VALUE));
        final int y = (int) Math.round(position.optDouble("y", Integer.MIN_VALUE));
        if (node.getGraphics() == null) {
            node.setGraphics(new NodeGraphics(x, y));
            return;
        }
        node.getGraphics().setPosition(x, y);
    }

    private void attachDefaultEdge(final BasicNode source, final AbstractEdge edge, final int index)
            throws SceneFlowIrCompileException {
        if (source.getDedge() != null) {
            throw new SceneFlowIrCompileException(
                    "Operation " + index + ": source node already has a default edge (EEDGE/TEDGE).");
        }
        source.setDedge(edge);
    }

    private VariableDefinition parseVarDef(final JSONObject varDefJson, final int index) throws SceneFlowIrCompileException {
        if (varDefJson == null) {
            throw new SceneFlowIrCompileException("Operation " + index + ": varDef object is required.");
        }
        final String name = varDefJson.optString("name", "").trim();
        final String type = varDefJson.optString("type", "").trim();
        if (name.isEmpty() || type.isEmpty()) {
            throw new SceneFlowIrCompileException("Operation " + index + ": varDef.name and varDef.type are required.");
        }
        final String runtimeType;
        if ("Event".equals(type)) {
            final String elementType = varDefJson.optString("eventElementType", "*").trim();
            final int capacity = varDefJson.optInt("eventCapacity", 10);
            runtimeType = "Event(" + (elementType.isEmpty() ? "*" : elementType) + ", " + Math.max(0, capacity) + ")";
        } else {
            runtimeType = type;
        }
        final Expression initExpression;
        final String expressionText = varDefJson.optString("expression", "").trim();
        if (expressionText.isEmpty()) {
            initExpression = null;
        } else {
            final Command parsed = runGlueParser(expressionText, index, "varDef.expression");
            if (!(parsed instanceof Expression)) {
                throw new SceneFlowIrCompileException("Operation " + index + ": varDef.expression is not an expression.");
            }
            initExpression = (Expression) parsed;
        }
        return new VariableDefinition(name, runtimeType, initExpression);
    }

    private Expression parseCondition(final JSONObject payload, final int index) throws SceneFlowIrCompileException {
        if (payload == null) {
            throw new SceneFlowIrCompileException("Operation " + index + ": payload is required for condition edge.");
        }
        final String conditionText = payload.optString("conditionText", "").trim();
        if (conditionText.isEmpty()) {
            throw new SceneFlowIrCompileException("Operation " + index + ": payload.conditionText is required.");
        }
        final Command parsed = runGlueParser(conditionText, index, "payload.conditionText");
        if (!(parsed instanceof Expression)) {
            throw new SceneFlowIrCompileException("Operation " + index + ": conditionText does not parse to an expression.");
        }
        return (Expression) parsed;
    }

    private List<Command> parseCommandsFromPayload(final JSONObject payload, final int index)
            throws SceneFlowIrCompileException {
        final List<Command> commands = new ArrayList<>();
        if (payload == null || !payload.has("commands")) {
            return commands;
        }
        final JSONArray commandTexts = payload.optJSONArray("commands");
        if (commandTexts == null) {
            return commands;
        }
        for (int i = 0; i < commandTexts.length(); i++) {
            final String commandText = commandTexts.optString(i, "").trim();
            if (commandText.isEmpty()) {
                continue;
            }
            commands.add(parseCommand(commandText, index, "payload.commands[" + i + "]"));
        }
        return commands;
    }

    private Command parseCommand(final String text, final int index, final String field) throws SceneFlowIrCompileException {
        if (text == null || text.trim().isEmpty()) {
            throw new SceneFlowIrCompileException("Operation " + index + ": " + field + " is empty.");
        }
        final Command command = runGlueParser(text, index, field);
        if (command == null) {
            throw new SceneFlowIrCompileException("Operation " + index + ": cannot parse command: " + text);
        }
        return command;
    }

    private Command runGlueParser(final String text, final int index, final String field)
            throws SceneFlowIrCompileException {
        try {
            return GlueParser.run(text);
        } catch (Exception exc) {
            throw new SceneFlowIrCompileException(
                    "Operation " + index + ": parser failure for " + field + " = " + text, exc);
        }
    }

    private String requiredString(final JSONObject obj, final String field, final int index)
            throws SceneFlowIrCompileException {
        final String value = obj.optString(field, "").trim();
        if (value.isEmpty()) {
            throw new SceneFlowIrCompileException("Operation " + index + ": missing field '" + field + "'.");
        }
        return value;
    }

    private void collectNodeIds(final BasicNode node, final Set<String> out) {
        out.add(node.getId());
        if (!(node instanceof SuperNode)) {
            return;
        }
        final SuperNode superNode = (SuperNode) node;
        for (BasicNode child : superNode.getNodeList()) {
            collectNodeIds(child, out);
        }
        for (SuperNode child : superNode.getSuperNodeList()) {
            collectNodeIds(child, out);
        }
    }

    private static final class EdgeRef {
        private final BasicNode source;
        private final AbstractEdge edge;

        private EdgeRef(final BasicNode source, final AbstractEdge edge) {
            this.source = source;
            this.edge = edge;
        }
    }

    private static final class GraphContext {
        private final SceneFlow root;
        private final Map<String, BasicNode> nodeById;
        private final Map<String, SuperNode> parentByNodeId;
        private final Map<String, EdgeRef> edgeById;

        private GraphContext(
                final SceneFlow root,
                final Map<String, BasicNode> nodeById,
                final Map<String, SuperNode> parentByNodeId,
                final Map<String, EdgeRef> edgeById) {
            this.root = root;
            this.nodeById = nodeById;
            this.parentByNodeId = parentByNodeId;
            this.edgeById = edgeById;
        }

        private static GraphContext from(final SceneFlow flow) {
            final Map<String, BasicNode> nodeById = new HashMap<>();
            final Map<String, SuperNode> parentByNodeId = new HashMap<>();
            final Map<String, EdgeRef> edgeById = new HashMap<>();
            nodeById.put(flow.getId(), flow);
            indexRecursive(flow, flow, nodeById, parentByNodeId, edgeById);
            return new GraphContext(flow, nodeById, parentByNodeId, edgeById);
        }

        private static void indexRecursive(
                final SuperNode parent,
                final SceneFlow root,
                final Map<String, BasicNode> nodeById,
                final Map<String, SuperNode> parentByNodeId,
                final Map<String, EdgeRef> edgeById) {
            for (BasicNode node : parent.getNodeList()) {
                nodeById.put(node.getId(), node);
                parentByNodeId.put(node.getId(), parent);
                indexEdges(node, edgeById);
            }
            for (SuperNode node : parent.getSuperNodeList()) {
                nodeById.put(node.getId(), node);
                parentByNodeId.put(node.getId(), parent);
                indexEdges(node, edgeById);
                indexRecursive(node, root, nodeById, parentByNodeId, edgeById);
            }
        }

        private static void indexEdges(final BasicNode source, final Map<String, EdgeRef> edgeById) {
            final List<AbstractEdge> edges = source.getEdgeList();
            for (int i = 0; i < edges.size(); i++) {
                final String syntheticId = "__existing__" + source.getId() + "__" + i;
                edgeById.put(syntheticId, new EdgeRef(source, edges.get(i)));
            }
        }

        private BasicNode requireNode(final String nodeId, final int index) throws SceneFlowIrCompileException {
            BasicNode node = nodeById.get(nodeId);
            if (node == null && "SceneFlow".equals(nodeId)) {
                node = root;
            }
            if (node == null) {
                throw new SceneFlowIrCompileException("Operation " + index + ": unknown node id: " + nodeId);
            }
            return node;
        }

        private void addNode(final BasicNode node, final SuperNode parent) {
            nodeById.put(node.getId(), node);
            parentByNodeId.put(node.getId(), parent);
        }

        private void removeNodes(final Set<String> removedIds) {
            for (String id : removedIds) {
                nodeById.remove(id);
                parentByNodeId.remove(id);
            }
        }

        private void removeEdgesReferencing(final Set<String> removedIds) {
            final List<String> edgeIds = new ArrayList<>(edgeById.keySet());
            for (String edgeId : edgeIds) {
                final EdgeRef ref = edgeById.get(edgeId);
                if (ref == null) {
                    continue;
                }
                final boolean remove = removedIds.contains(ref.source.getId())
                        || removedIds.contains(ref.edge.getTargetUnid());
                if (remove) {
                    detach(ref.source, ref.edge);
                    edgeById.remove(edgeId);
                }
            }
        }

        private void detach(final BasicNode source, final AbstractEdge edge) {
            if (edge instanceof EpsilonEdge || edge instanceof TimeoutEdge) {
                if (source.getDedge() == edge) {
                    source.removeDEdge();
                }
            } else if (edge instanceof GuargedEdge) {
                source.removeCEdge((GuargedEdge) edge);
            } else if (edge instanceof InterruptEdge) {
                source.removeIEdge((InterruptEdge) edge);
            } else if (edge instanceof RandomEdge) {
                source.removePEdge((RandomEdge) edge);
            } else if (edge instanceof ForkingEdge) {
                source.removeFEdge((ForkingEdge) edge);
            }
        }
    }
}
