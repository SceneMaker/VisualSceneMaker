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
import de.dfki.vsm.model.sceneflow.chart.graphics.node.NodePosition;
import de.dfki.vsm.model.sceneflow.glue.command.Assignment;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.model.sceneflow.glue.command.definition.DataTypeDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.CallingExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.ConstructExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.ParenExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.TernaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.UnaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.ContainsList;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.StringLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.record.ArrayExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.record.StructExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.variable.ArrayVariable;
import de.dfki.vsm.model.sceneflow.glue.command.expression.variable.MemberVariable;
import de.dfki.vsm.model.sceneflow.glue.command.expression.variable.SimpleVariable;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.PlayActionActivity;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.PlayDialogAction;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.PlayScenesActivity;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.StopActionActivity;
import de.dfki.vsm.model.scenescript.SceneScript;
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
        final String timeoutExpr;
        final long timeoutMin;
        final long timeoutMax;

        public ClipboardEdgeData(String sourceId, String targetId, String edgeType,
                                 String condition, int probability, long timeout,
                                 String timeoutExpr, long timeoutMin, long timeoutMax) {
            this.sourceId = sourceId;
            this.targetId = targetId;
            this.edgeType = edgeType;
            this.condition = condition;
            this.probability = probability;
            this.timeout = timeout;
            this.timeoutExpr = timeoutExpr;
            this.timeoutMin = timeoutMin;
            this.timeoutMax = timeoutMax;
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

        Set<String> clipboardStartNodeIds(String projectId);
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
        Set<String> clipboardStartNodeIds = context.clipboardStartNodeIds(pid);
        clipboardNodes.clear();
        clipboardEdges.clear();
        clipboardStartNodeIds.clear();

        for (String nodeId : nodeIdSet) {
            BasicNode node = context.findNodeRecursive(sceneFlow, nodeId);
            if (node != null) {
                clipboardNodes.add(node.getCopy());
                SuperNode parent = node.getParentNode();
                if (parent != null && parent.getStartNodeMap().containsKey(node.getId())) {
                    clipboardStartNodeIds.add(node.getId());
                }
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
                String timeoutExpr = dEdge instanceof TimeoutEdge && ((TimeoutEdge) dEdge).getExpression() != null
                        ? ((TimeoutEdge) dEdge).getExpression().getConcreteSyntax()
                        : null;
                long timeoutMin = dEdge instanceof TimeoutEdge ? ((TimeoutEdge) dEdge).getTimeoutMin() : Long.MIN_VALUE;
                long timeoutMax = dEdge instanceof TimeoutEdge ? ((TimeoutEdge) dEdge).getTimeoutMax() : Long.MIN_VALUE;
                clipboardEdges.add(new ClipboardEdgeData(sourceId, dEdge.getTargetUnid(),
                        edgeType, null, 0, timeout, timeoutExpr, timeoutMin, timeoutMax));
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
                    condition = cond != null ? cond.getConcreteSyntax() : "true";
                } else if (edge instanceof InterruptEdge) {
                    Expression cond = ((InterruptEdge) edge).getCondition();
                    condition = cond != null ? cond.getConcreteSyntax() : "true";
                } else if (edge instanceof RandomEdge) {
                    probability = ((RandomEdge) edge).getProbability();
                }
                clipboardEdges.add(new ClipboardEdgeData(sourceId, targetId, edgeType, condition, probability, 0,
                        null, Long.MIN_VALUE, Long.MIN_VALUE));
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

        String sourceProjectId = params.optString("sourceProjectId", "");
        if (sourceProjectId.isBlank()) {
            sourceProjectId = pid;
        }
        if (!sourceProjectId.equals(pid) && context.runtimeProject(sourceProjectId) == null) {
            return context.errorResponse("SOURCE_PROJECT_NOT_FOUND",
                    "Clipboard source project is no longer open");
        }

        List<BasicNode> clipboardNodes = context.clipboardNodes(sourceProjectId);
        List<ClipboardEdgeData> clipboardEdges = context.clipboardEdges(sourceProjectId);
        Set<String> clipboardStartNodeIds = context.clipboardStartNodeIds(sourceProjectId);
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
        Set<String> deepCopiedSuperSourceIds = new HashSet<>();
        List<SuperNode> pastedSuperNodes = new ArrayList<>();
        List<String> newNodeIds = new ArrayList<>();

        for (BasicNode clipboardNode : clipboardNodes) {
            String oldId = clipboardNode.getId();
            boolean isSuperNode = clipboardNode instanceof SuperNode;
            if (isSuperNode) {
                SuperNode superCopy = ((SuperNode) clipboardNode).getCopy();
                String newId = context.allocateNodeId(pid, true, usedIds);
                usedIds.add(newId);
                idMapping.put(oldId, newId);
                deepCopiedSuperSourceIds.add(oldId);
                superCopy.setId(newId);

                NodeGraphics oldGraphics = superCopy.getGraphics();
                int x = (oldGraphics != null && oldGraphics.getPosition() != null ? oldGraphics.getPosition().getXPos() : 0) + dx;
                int y = (oldGraphics != null && oldGraphics.getPosition() != null ? oldGraphics.getPosition().getYPos() : 0) + dy;
                int[] settled = snapAndSettlePosition(
                        x, y, nodeWidth, nodeHeight, gridX, gridY, originX, originY, occupiedPositions, collisionThreshold
                );
                superCopy.setGraphics(new NodeGraphics(settled[0], settled[1]));
                occupiedPositions.add(settled);

                superCopy.setParentNode(activeSuperNode);
                activeSuperNode.addSuperNode(superCopy);
                if (clipboardStartNodeIds.contains(oldId)) {
                    activeSuperNode.addStartNode(superCopy);
                }
                newNodeIds.add(newId);

                Map<String, String> subtreeIdMap = new HashMap<>();
                assignFreshIdsRecursively(superCopy, pid, context, usedIds, subtreeIdMap, false);
                idMapping.putAll(subtreeIdMap);
                pastedSuperNodes.add(superCopy);
            } else {
                String newId = context.allocateNodeId(pid, false, usedIds);
                usedIds.add(newId);
                idMapping.put(oldId, newId);

                BasicNode newNode = new BasicNode();
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
                int[] settled = snapAndSettlePosition(
                        x, y, nodeWidth, nodeHeight, gridX, gridY, originX, originY, occupiedPositions, collisionThreshold
                );
                newNode.setGraphics(new NodeGraphics(settled[0], settled[1]));
                occupiedPositions.add(settled);

                newNode.setParentNode(activeSuperNode);
                activeSuperNode.addNode(newNode);
                if (clipboardStartNodeIds.contains(oldId)) {
                    activeSuperNode.addStartNode(newNode);
                }
                newNodeIds.add(newId);
            }
        }

        List<String> droppedEdges = new ArrayList<>();
        for (SuperNode pastedSuper : pastedSuperNodes) {
            // Resolve remapped targets within the full active paste scope so edges from
            // a pasted supernode to sibling pasted nodes are preserved.
            reconcileSuperNodeSubtree(activeSuperNode, pastedSuper, idMapping, context, droppedEdges);
        }

        for (ClipboardEdgeData ce : clipboardEdges) {
            if (deepCopiedSuperSourceIds.contains(ce.sourceId)) {
                // Supernode copies already carry their own outgoing edges after remap.
                // Replaying clipboard edges here would duplicate them.
                continue;
            }
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

        List<BasicNode> pastedNodesFlat = new ArrayList<>();
        for (String newId : newNodeIds) {
            BasicNode pastedNode = context.resolveNodeById(activeSuperNode, newId);
            if (pastedNode == null) {
                continue;
            }
            if (pastedNode instanceof SuperNode) {
                context.collectNodes((SuperNode) pastedNode, pastedNodesFlat);
            } else {
                pastedNodesFlat.add(pastedNode);
            }
        }
        List<String> referenceWarnings = collectMissingReferenceWarnings(
                pastedNodesFlat, project.getSceneScript(), activeSuperNode);

        context.markDirty(pid);
        JSONObject snapshot = context.createSceneFlowSnapshot(project, pid, snapshotTarget, sceneFlow);
        JSONObject response = context.buildSceneFlowResponse(snapshot);
        response.put("nodeIds", new JSONArray(newNodeIds));
        if (!droppedEdges.isEmpty() || !referenceWarnings.isEmpty()) {
            JSONArray warnings = new JSONArray();
            if (!droppedEdges.isEmpty()) {
                warnings.put("Paste: " + droppedEdges.size() + " edge(s) could not be reconnected "
                        + "because their target nodes were not part of the selection. "
                        + "Redraw them manually.");
                for (String desc : droppedEdges) {
                    warnings.put(desc);
                }
            }
            for (String desc : referenceWarnings) {
                warnings.put(desc);
            }
            response.put("warnings", warnings);
        }
        context.broadcastSceneFlowSnapshot(broadcaster, pid, snapshot);
        context.recordHistory(pid, "SceneFlow.Selection.Paste");
        return response;
    }

    /**
     * Best-effort, non-blocking check for pasted content that only made sense in the source
     * project: scenes played by name and variables read/written by name are copied by value/text
     * (see {@link #collectEdgesForClipboard}/{@code copySelectionForProject}), so they silently do
     * nothing (scenes) or would fail at runtime (variables) unless the target project happens to
     * declare a matching name. Agent/device calls are deliberately not checked here: the command
     * model has no structural field naming the called agent (see {@code PlayActionActivity}/
     * {@code StopActionActivity}, whose payload is a free-form {@link Expression}), so any such
     * check would be a fragile text heuristic rather than a real reference check.
     */
    private List<String> collectMissingReferenceWarnings(final List<BasicNode> pastedNodes,
                                                          final SceneScript sceneScript,
                                                          final SuperNode pasteLocation) {
        List<String> warnings = new ArrayList<>();

        Set<String> reportedScenes = new HashSet<>();
        for (BasicNode node : pastedNodes) {
            for (Command cmd : node.getCmdList()) {
                if (!(cmd instanceof PlayScenesActivity)) {
                    continue;
                }
                Expression argument = ((PlayScenesActivity) cmd).getArgument();
                if (!(argument instanceof StringLiteral)) {
                    continue;
                }
                String sceneName = ((StringLiteral) argument).getValue();
                if (sceneName == null || sceneName.isBlank() || !reportedScenes.add(sceneName)) {
                    continue;
                }
                if (sceneScript == null || sceneScript.getSceneGroup(sceneName) == null) {
                    warnings.add("Pasted command plays scene '" + sceneName
                            + "', which the target project does not define.");
                }
            }
        }

        // Variables visible at the paste location: the target's own scope chain (walking up to
        // the SceneFlow root, which is itself a node with a global var-def list) plus whatever
        // node-local variables travelled along with the pasted nodes themselves.
        Set<String> visibleVarNames = new HashSet<>();
        for (BasicNode ancestor = pasteLocation; ancestor != null; ancestor = ancestor.getParentNode()) {
            for (VariableDefinition def : ancestor.getVarDefList()) {
                visibleVarNames.add(def.getName());
            }
        }
        for (BasicNode node : pastedNodes) {
            for (VariableDefinition def : node.getVarDefList()) {
                visibleVarNames.add(def.getName());
            }
        }

        Set<String> reportedVars = new HashSet<>();
        for (BasicNode node : pastedNodes) {
            for (Command cmd : node.getCmdList()) {
                List<Expression> expressions = new ArrayList<>();
                collectTopLevelExpressions(cmd, expressions);
                Set<String> referencedNames = new HashSet<>();
                for (Expression expr : expressions) {
                    collectVariableReferences(expr, referencedNames);
                }
                for (String name : referencedNames) {
                    if (!visibleVarNames.contains(name) && reportedVars.add(name)) {
                        warnings.add("Pasted command references variable '" + name
                                + "', which is not declared in the target project.");
                    }
                }
            }
        }

        return warnings;
    }

    /** Collects the expressions a single {@link Command} exposes directly, without recursing
     *  into their sub-expressions (that's {@link #collectVariableReferences}'s job). */
    private void collectTopLevelExpressions(final Command cmd, final List<Expression> out) {
        if (cmd instanceof Assignment) {
            Assignment assignment = (Assignment) cmd;
            if (assignment.getLeftExpression() != null) {
                out.add(assignment.getLeftExpression());
            }
            if (assignment.getInitExpression() != null) {
                out.add(assignment.getInitExpression());
            }
        } else if (cmd instanceof PlayActionActivity) {
            PlayActionActivity activity = (PlayActionActivity) cmd;
            if (activity.getCommand() != null) {
                out.add(activity.getCommand());
            }
            out.addAll(activity.getArgList());
        } else if (cmd instanceof StopActionActivity) {
            StopActionActivity activity = (StopActionActivity) cmd;
            if (activity.getCommand() != null) {
                out.add(activity.getCommand());
            }
            out.addAll(activity.getArgList());
        } else if (cmd instanceof PlayScenesActivity) {
            PlayScenesActivity activity = (PlayScenesActivity) cmd;
            if (activity.getArgument() != null) {
                out.add(activity.getArgument());
            }
            out.addAll(activity.getArgList());
        } else if (cmd instanceof PlayDialogAction) {
            PlayDialogAction activity = (PlayDialogAction) cmd;
            if (activity.getArg() != null) {
                out.add(activity.getArg());
            }
            out.addAll(activity.getArgList());
        } else if (cmd instanceof Expression) {
            out.add((Expression) cmd);
        }
    }

    /** Recursively collects referenced variable names without evaluating the expression.
     *  Mirrors {@code de.dfki.vsm.runtime.interpreter.GuardDependencyExtractor}, except calls
     *  ({@link CallingExpression}/{@link ConstructExpression}) are descended into here instead of
     *  treated as opaque, since we're after every name mentioned rather than re-evaluation triggers. */
    private void collectVariableReferences(final Expression expr, final Set<String> out) {
        if (expr == null) {
            return;
        }
        if (expr instanceof SimpleVariable) {
            out.add(((SimpleVariable) expr).getName());
        } else if (expr instanceof MemberVariable) {
            out.add(((MemberVariable) expr).getName());
        } else if (expr instanceof ArrayVariable) {
            out.add(((ArrayVariable) expr).getName());
            collectVariableReferences(((ArrayVariable) expr).getExpression(), out);
        } else if (expr instanceof BinaryExpression) {
            collectVariableReferences(((BinaryExpression) expr).getLeftExp(), out);
            collectVariableReferences(((BinaryExpression) expr).getRightExp(), out);
        } else if (expr instanceof UnaryExpression) {
            collectVariableReferences(((UnaryExpression) expr).getExp(), out);
        } else if (expr instanceof TernaryExpression) {
            collectVariableReferences(((TernaryExpression) expr).getCondition(), out);
            collectVariableReferences(((TernaryExpression) expr).getThenExp(), out);
            collectVariableReferences(((TernaryExpression) expr).getElseExp(), out);
        } else if (expr instanceof ParenExpression) {
            collectVariableReferences(((ParenExpression) expr).getExp(), out);
        } else if (expr instanceof ArrayExpression) {
            for (Expression e : ((ArrayExpression) expr).getExpList()) {
                collectVariableReferences(e, out);
            }
        } else if (expr instanceof StructExpression) {
            for (Assignment a : ((StructExpression) expr).getExpList()) {
                collectVariableReferences(a.getInitExpression(), out);
            }
        } else if (expr instanceof ContainsList) {
            collectVariableReferences(((ContainsList) expr).getListExp(), out);
            collectVariableReferences(((ContainsList) expr).getItemExp(), out);
        } else if (expr instanceof CallingExpression) {
            for (Expression e : ((CallingExpression) expr).getArgList()) {
                collectVariableReferences(e, out);
            }
        } else if (expr instanceof ConstructExpression) {
            for (Expression e : ((ConstructExpression) expr).getArgList()) {
                collectVariableReferences(e, out);
            }
        }
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
                if (ce.timeoutExpr != null && !ce.timeoutExpr.isBlank()) {
                    tedge.setExpression(context.parseExpressionOrNull(ce.timeoutExpr));
                }
                if (ce.timeoutMin >= 0 && ce.timeoutMax >= ce.timeoutMin) {
                    tedge.setTimeoutRange(ce.timeoutMin, ce.timeoutMax);
                }
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

    private int[] snapAndSettlePosition(final int x,
                                        final int y,
                                        final int nodeWidth,
                                        final int nodeHeight,
                                        final int gridX,
                                        final int gridY,
                                        final double originX,
                                        final double originY,
                                        final List<int[]> occupiedPositions,
                                        final int collisionThreshold) {
        double centerX = x + nodeWidth / 2.0;
        double centerY = y + nodeHeight / 2.0;
        double snappedCenterX = originX + Math.round((centerX - originX) / gridX) * gridX;
        double snappedCenterY = originY + Math.round((centerY - originY) / gridY) * gridY;
        int snappedX = Math.max(1, (int) Math.round(snappedCenterX - nodeWidth / 2.0));
        int snappedY = Math.max(1, (int) Math.round(snappedCenterY - nodeHeight / 2.0));

        int attempts = 0;
        while (isPositionOccupied(snappedX, snappedY, occupiedPositions, collisionThreshold) && attempts < 100) {
            attempts++;
            snappedX += gridX;
            if (attempts % 5 == 0) {
                snappedX -= 5 * gridX;
                snappedY += gridY;
            }
        }
        return new int[]{snappedX, snappedY};
    }

    private void assignFreshIdsRecursively(final SuperNode root,
                                           final String projectId,
                                           final Context context,
                                           final Set<String> usedIds,
                                           final Map<String, String> idMapping,
                                           final boolean includeRoot) {
        if (includeRoot) {
            String oldId = root.getId();
            String newId = context.allocateNodeId(projectId, true, usedIds);
            usedIds.add(newId);
            idMapping.put(oldId, newId);
            root.setId(newId);
        }
        for (BasicNode child : root.getNodeList()) {
            String oldId = child.getId();
            boolean superNode = child instanceof SuperNode;
            String newId = context.allocateNodeId(projectId, superNode, usedIds);
            usedIds.add(newId);
            idMapping.put(oldId, newId);
            child.setId(newId);
            child.setParentNode(root);
        }
        for (SuperNode child : root.getSuperNodeList()) {
            child.setParentNode(root);
            assignFreshIdsRecursively(child, projectId, context, usedIds, idMapping, true);
        }
    }

    private void reconcileSuperNodeSubtree(final SuperNode root,
                                           final SuperNode current,
                                           final Map<String, String> idMapping,
                                           final Context context,
                                           final List<String> droppedEdges) {
        reconcileNodeEdges(root, current, idMapping, context, droppedEdges);
        List<BasicNode> nodes = current.getNodeAndSuperNodeList();
        for (BasicNode node : nodes) {
            if (node instanceof SuperNode) {
                reconcileStartNodeMap((SuperNode) node, root, idMapping, context);
                reconcileSuperNodeSubtree(root, (SuperNode) node, idMapping, context, droppedEdges);
            } else {
                reconcileNodeEdges(root, node, idMapping, context, droppedEdges);
            }
        }
        reconcileStartNodeMap(current, root, idMapping, context);
    }

    private void reconcileNodeEdges(final SuperNode root,
                                    final BasicNode source,
                                    final Map<String, String> idMapping,
                                    final Context context,
                                    final List<String> droppedEdges) {
        List<GuargedEdge> cedges = new ArrayList<>();
        for (GuargedEdge edge : source.getCEdgeList()) {
            if (remapEdge(root, source, edge, idMapping, context)) {
                cedges.add(edge);
            } else {
                droppedEdges.add("CEdge from '" + source.getName() + "' \u2192 " + edge.getTargetUnid());
            }
        }
        source.removeAllCEdges();
        for (GuargedEdge edge : cedges) {
            source.addCEdge(edge);
        }

        List<InterruptEdge> iedges = new ArrayList<>();
        for (InterruptEdge edge : source.getIEdgeList()) {
            if (remapEdge(root, source, edge, idMapping, context)) {
                iedges.add(edge);
            } else {
                droppedEdges.add("IEdge from '" + source.getName() + "' \u2192 " + edge.getTargetUnid()
                        + " (target not in selection)");
            }
        }
        source.removeAllIEdges();
        for (InterruptEdge edge : iedges) {
            source.addIEdge(edge);
        }

        List<RandomEdge> pedges = new ArrayList<>();
        for (RandomEdge edge : source.getPEdgeList()) {
            if (remapEdge(root, source, edge, idMapping, context)) {
                pedges.add(edge);
            } else {
                droppedEdges.add("PEdge from '" + source.getName() + "' \u2192 " + edge.getTargetUnid());
            }
        }
        source.removeAllPEdges();
        for (RandomEdge edge : pedges) {
            source.addPEdge(edge);
        }

        List<ForkingEdge> fedges = new ArrayList<>();
        for (ForkingEdge edge : source.getFEdgeList()) {
            if (remapEdge(root, source, edge, idMapping, context)) {
                fedges.add(edge);
            } else {
                droppedEdges.add("FEdge from '" + source.getName() + "' \u2192 " + edge.getTargetUnid());
            }
        }
        source.removeAllFEdges();
        for (ForkingEdge edge : fedges) {
            source.addFEdge(edge);
        }

        AbstractEdge dedge = source.getDedge();
        if (dedge != null && !remapEdge(root, source, dedge, idMapping, context)) {
            droppedEdges.add("DEdge from '" + source.getName() + "' \u2192 " + dedge.getTargetUnid());
            source.setDedge(null);
        }
    }

    private boolean remapEdge(final SuperNode root,
                              final BasicNode source,
                              final AbstractEdge edge,
                              final Map<String, String> idMapping,
                              final Context context) {
        if (edge == null) {
            return false;
        }
        String remappedTargetId = idMapping.get(edge.getTargetUnid());
        if ((remappedTargetId == null || remappedTargetId.isBlank()) && edge.getTargetNode() != null) {
            String referencedTargetId = edge.getTargetNode().getId();
            if (referencedTargetId != null && !referencedTargetId.isBlank()) {
                String mappedFromReferencedId = idMapping.get(referencedTargetId);
                if (mappedFromReferencedId != null && !mappedFromReferencedId.isBlank()) {
                    remappedTargetId = mappedFromReferencedId;
                } else if (idMapping.containsValue(referencedTargetId)) {
                    remappedTargetId = referencedTargetId;
                }
            }
        }
        if (remappedTargetId == null || remappedTargetId.isBlank()) {
            return false;
        }
        BasicNode target = context.resolveNodeById(root, remappedTargetId);
        if (target == null) {
            return false;
        }
        edge.setSourceNode(source);
        edge.setTargetNode(target);
        edge.setSourceUnid(source.getId());
        edge.setTargetUnid(remappedTargetId);
        if (edge.getGraphics() == null) {
            edge.setGraphics(new EdgeGraphics());
        }
        return true;
    }

    private void reconcileStartNodeMap(final SuperNode superNode,
                                       final SuperNode subtreeRoot,
                                       final Map<String, String> idMapping,
                                       final Context context) {
        HashMap<String, BasicNode> rebuilt = new HashMap<>();
        for (Map.Entry<String, BasicNode> entry : superNode.getStartNodeMap().entrySet()) {
            String remappedId = idMapping.get(entry.getKey());
            if ((remappedId == null || remappedId.isBlank()) && entry.getValue() != null) {
                String referencedId = entry.getValue().getId();
                String mappedFromReferencedId = idMapping.get(referencedId);
                if (mappedFromReferencedId != null && !mappedFromReferencedId.isBlank()) {
                    remappedId = mappedFromReferencedId;
                } else if (referencedId != null && idMapping.containsValue(referencedId)) {
                    remappedId = referencedId;
                }
            }
            if (remappedId == null || remappedId.isBlank()) {
                continue;
            }
            BasicNode target = context.resolveNodeById(subtreeRoot, remappedId);
            if (target != null) {
                rebuilt.put(remappedId, target);
            }
        }
        superNode.setStartNodeMap(rebuilt);
        BasicNode history = superNode.getHistoryNode();
        if (history == null) {
            return;
        }
        String historyId = history.getId();
        String remappedHistoryId = idMapping.get(historyId);
        if ((remappedHistoryId == null || remappedHistoryId.isBlank()) && historyId != null && idMapping.containsValue(historyId)) {
            remappedHistoryId = historyId;
        }
        BasicNode mappedHistory = context.resolveNodeById(subtreeRoot,
                remappedHistoryId != null && !remappedHistoryId.isBlank() ? remappedHistoryId : historyId);
        superNode.setHistoryNode(mappedHistory);
    }
}
