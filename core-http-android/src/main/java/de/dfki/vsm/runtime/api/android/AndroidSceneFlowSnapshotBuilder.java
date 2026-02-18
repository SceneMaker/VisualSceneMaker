package de.dfki.vsm.runtime.api.android;

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
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.model.sceneflow.glue.command.definition.DataTypeDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.datatype.ListTypeDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.datatype.MemberDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.datatype.StructTypeDefinition;
import de.dfki.vsm.util.tpl.Tuple;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Android-side SceneFlow snapshot serializer matching desktop Web UI snapshot fields.
 */
public final class AndroidSceneFlowSnapshotBuilder {

    private AndroidSceneFlowSnapshotBuilder() {
    }

    public static JSONObject createSnapshot(final String projectId,
                                            final SuperNode superNode,
                                            final SceneFlow sceneFlow,
                                            final int nodeWidth,
                                            final int nodeHeight,
                                            final JSONObject undoState) {
        JSONObject snapshot = new JSONObject();
        snapshot.put("projectId", projectId);
        snapshot.put("superNodeId", superNode.getId() != null ? superNode.getId() : "");
        snapshot.put("revision", superNode.hashCode());

        if (undoState != null) {
            snapshot.put("undoState", undoState);
        }

        JSONObject superNodeJson = new JSONObject();
        superNodeJson.put("id", superNode.getId() != null ? superNode.getId() : "");
        superNodeJson.put("name", superNode.getName() != null ? superNode.getName() : "SceneFlow");
        superNodeJson.put("flavour", superNode.getFlavour() != null ? superNode.getFlavour().name() : "None");
        snapshot.put("superNode", superNodeJson);

        JSONArray path = new JSONArray();
        JSONArray pathNodes = new JSONArray();
        List<SuperNode> pathList = findPathToSuperNode(sceneFlow, superNode.getId());
        if (pathList == null || pathList.isEmpty()) {
            pathList = new ArrayList<>();
            pathList.add(superNode);
        }
        for (SuperNode node : pathList) {
            String nodeName = node.getName();
            if (nodeName == null || nodeName.isBlank()) {
                nodeName = "SceneFlow";
            }
            String nodeId = node.getId();
            if (nodeId == null || nodeId.isBlank()) {
                nodeId = "__root__";
            }
            path.put(nodeName);
            JSONObject pathEntry = new JSONObject();
            pathEntry.put("id", nodeId);
            pathEntry.put("name", nodeName);
            pathEntry.put("isRoot", node.getParentNode() == null);
            pathNodes.put(pathEntry);
        }
        snapshot.put("path", path);
        snapshot.put("pathNodes", pathNodes);

        Set<String> altStartIds = collectAltStartIds(superNode);
        JSONObject superNodeData = nodeToJson(superNode, superNode, altStartIds, nodeWidth, nodeHeight);
        superNodeData.put("isStart", superNode.getParentNode() == null ||
                (superNode.getParentNode() != null && superNode.getParentNode().getStartNodeMap().containsKey(superNode.getId())));
        superNodeData.put("isRoot", superNode.getParentNode() == null);
        snapshot.put("superNodeData", superNodeData);

        JSONArray nodes = new JSONArray();
        for (BasicNode node : superNode.getNodeAndSuperNodeList()) {
            nodes.put(nodeToJson(node, superNode, altStartIds, nodeWidth, nodeHeight));
        }
        snapshot.put("nodes", nodes);

        JSONArray edges = new JSONArray();
        for (BasicNode node : superNode.getNodeAndSuperNodeList()) {
            for (AbstractEdge edge : node.getEdgeList()) {
                edges.put(edgeToJson(edge));
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

    public static SuperNode resolveSuperNode(final SceneFlow sceneFlow, final String superNodeId) {
        if (superNodeId == null || superNodeId.isBlank() || "__root__".equals(superNodeId)) {
            return sceneFlow;
        }
        return findSuperNodeById(sceneFlow, superNodeId);
    }

    private static SuperNode findSuperNodeById(final SuperNode parent, final String id) {
        if (parent == null) {
            return null;
        }
        if (id.equals(parent.getId())) {
            return parent;
        }
        for (SuperNode child : parent.getSuperNodeList()) {
            SuperNode found = findSuperNodeById(child, id);
            if (found != null) {
                return found;
            }
        }
        return null;
    }

    private static JSONObject nodeToJson(final BasicNode node,
                                         final SuperNode superNode,
                                         final Set<String> altStartIds,
                                         final int nodeWidth,
                                         final int nodeHeight) {
        JSONObject json = new JSONObject();
        json.put("id", node.getId());
        json.put("type", (node instanceof SuperNode) ? "Super" : "Basic");
        json.put("name", node.getName() != null ? node.getName() : "");
        json.put("comment", node.getComment() != null ? node.getComment() : "");
        json.put("flavour", node.getFlavour() != null ? node.getFlavour().name() : "None");
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
        if (node.getGraphics() != null && node.getGraphics().getPosition() != null) {
            x = node.getGraphics().getPosition().getXPos();
            y = node.getGraphics().getPosition().getYPos();
        }
        graphics.put("x", x);
        graphics.put("y", y);
        json.put("graphics", graphics);

        JSONObject size = new JSONObject();
        size.put("w", nodeWidth);
        size.put("h", nodeHeight);
        json.put("size", size);

        json.put("typeDefs", typeDefsToJson(node.getTypeDefList()));
        json.put("varDefs", varDefsToJson(node.getVarDefList()));
        json.put("commands", commandsToJson(node.getCmdList()));
        return json;
    }

    private static JSONObject edgeToJson(final AbstractEdge edge) {
        JSONObject json = new JSONObject();
        json.put("id", edgeRuntimeId(edge));
        json.put("type", getEdgeType(edge));

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
        EdgeGraphics eg = edge.getGraphics();
        EdgeArrow arrow = eg != null ? eg.getConnection() : null;
        JSONArray points = new JSONArray();
        if (arrow != null) {
            for (EdgePoint point : arrow.getPointList()) {
                JSONObject p = new JSONObject();
                p.put("x", point.getXPos());
                p.put("y", point.getYPos());
                p.put("cx", point.getCtrlXPos());
                p.put("cy", point.getCtrlYPos());
                points.put(p);
            }
        }
        graphics.put("points", points);
        graphics.put("docked", points.length() >= 2);
        json.put("graphics", graphics);

        String conditionText = "";
        if (edge instanceof GuargedEdge) {
            GuargedEdge ge = (GuargedEdge) edge;
            if (ge.getCondition() != null) {
                conditionText = ge.getCondition().getConcreteSyntax();
            }
        } else if (edge instanceof InterruptEdge) {
            InterruptEdge ie = (InterruptEdge) edge;
            if (ie.getCondition() != null) {
                conditionText = ie.getCondition().getConcreteSyntax();
            }
        }
        json.put("condition", conditionText);

        if (edge instanceof RandomEdge) {
            json.put("probability", ((RandomEdge) edge).getProbability());
        }

        if (edge instanceof TimeoutEdge) {
            TimeoutEdge te = (TimeoutEdge) edge;
            json.put("timeoutMs", te.getTimeout());
            json.put("timeoutExpr", te.getExpression() != null ? te.getExpression().getConcreteSyntax() : "");
        }

        return json;
    }

    static String edgeRuntimeId(final AbstractEdge edge) {
        if (edge == null) {
            return "";
        }
        final String sourceId = normalizeEdgeNodeId(edge.getSourceUnid(), edge.getSourceNode() != null ? edge.getSourceNode().getId() : "");
        final String targetId = normalizeEdgeNodeId(edge.getTargetUnid(), edge.getTargetNode() != null ? edge.getTargetNode().getId() : "");
        final String type = getEdgeType(edge);

        int sameKindIndex = 0;
        boolean foundSelf = false;
        BasicNode sourceNode = edge.getSourceNode();
        if (sourceNode != null) {
            for (AbstractEdge candidate : sourceNode.getEdgeList()) {
                if (!sameEdgeKind(candidate, sourceId, targetId, type)) {
                    continue;
                }
                if (candidate == edge) {
                    foundSelf = true;
                    break;
                }
                sameKindIndex++;
            }
        }

        if (!foundSelf) {
            // Fallback when edge instance cannot be located in source node list.
            sameKindIndex = 0;
        }
        return "E_" + sanitizeId(sourceId) + "_" + sanitizeId(targetId) + "_" + type + "_" + sameKindIndex;
    }

    private static boolean sameEdgeKind(final AbstractEdge edge,
                                        final String sourceId,
                                        final String targetId,
                                        final String type) {
        if (edge == null) {
            return false;
        }
        final String source = normalizeEdgeNodeId(edge.getSourceUnid(), edge.getSourceNode() != null ? edge.getSourceNode().getId() : "");
        final String target = normalizeEdgeNodeId(edge.getTargetUnid(), edge.getTargetNode() != null ? edge.getTargetNode().getId() : "");
        final String edgeType = getEdgeType(edge);
        return sourceId.equals(source) && targetId.equals(target) && type.equals(edgeType);
    }

    private static String normalizeEdgeNodeId(final String unid, final String nodeId) {
        if (unid != null && !unid.isBlank()) {
            return unid;
        }
        return nodeId == null ? "" : nodeId;
    }

    private static String sanitizeId(final String value) {
        if (value == null || value.isBlank()) {
            return "_";
        }
        StringBuilder out = new StringBuilder(value.length());
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            if ((c >= 'a' && c <= 'z')
                    || (c >= 'A' && c <= 'Z')
                    || (c >= '0' && c <= '9')
                    || c == '_'
                    || c == '-') {
                out.append(c);
            } else {
                out.append('_');
            }
        }
        return out.toString();
    }

    private static JSONObject commentToJson(final CommentBadge comment, final int index) {
        JSONObject json = new JSONObject();
        json.put("id", "C" + index);
        json.put("text", comment.getHTMLText() != null ? comment.getHTMLText() : "");

        JSONObject rectJson = new JSONObject();
        CommentGraphics cg = comment.getGraphics();
        CommentBoundary rect = cg != null ? cg.getRectangle() : null;
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

    private static String getEdgeType(final AbstractEdge edge) {
        if (edge instanceof GuargedEdge) return "CEDGE";
        if (edge instanceof RandomEdge) return "PEDGE";
        if (edge instanceof InterruptEdge) return "IEDGE";
        if (edge instanceof ForkingEdge) return "FEDGE";
        if (edge instanceof TimeoutEdge) return "TEDGE";
        if (edge instanceof EpsilonEdge) return "EEDGE";
        return "EEDGE";
    }

    private static JSONArray varDefsToJson(final List<VariableDefinition> defs) {
        JSONArray list = new JSONArray();
        if (defs == null) return list;
        for (VariableDefinition def : defs) {
            if (def != null) {
                JSONObject json = new JSONObject();
                json.put("name", def.getName());
                json.put("type", def.getType());
                json.put("expression", def.getExp() != null ? def.getExp().getConcreteSyntax() : "");
                json.put("syntax", def.getConcreteSyntax());
                list.put(json);
            }
        }
        return list;
    }

    private static JSONArray typeDefsToJson(final List<DataTypeDefinition> defs) {
        JSONArray list = new JSONArray();
        if (defs == null) return list;
        for (DataTypeDefinition def : defs) {
            if (def != null) {
                JSONObject json = new JSONObject();
                json.put("name", def.getName());
                json.put("flavour", def.getFlavour() != null ? def.getFlavour().name() : "");
                json.put("syntax", def.getConcreteSyntax());
                if (def instanceof ListTypeDefinition) {
                    ListTypeDefinition listDef = (ListTypeDefinition) def;
                    if (listDef.getType() != null) {
                        json.put("elementType", listDef.getType());
                    }
                } else if (def instanceof StructTypeDefinition) {
                    StructTypeDefinition structDef = (StructTypeDefinition) def;
                    JSONArray members = new JSONArray();
                    if (structDef.getMemberList() != null) {
                        for (MemberDefinition member : structDef.getMemberList()) {
                            if (member == null) continue;
                            JSONObject memberJson = new JSONObject();
                            memberJson.put("name", member.getName());
                            memberJson.put("type", member.getType());
                            members.put(memberJson);
                        }
                    }
                    json.put("members", members);
                }
                list.put(json);
            }
        }
        return list;
    }

    private static JSONArray commandsToJson(final List<Command> commands) {
        JSONArray list = new JSONArray();
        if (commands == null) return list;
        for (Command cmd : commands) {
            if (cmd != null) {
                JSONObject json = new JSONObject();
                json.put("text", cmd.getConcreteSyntax());
                json.put("syntax", cmd.getConcreteSyntax());
                list.put(json);
            }
        }
        return list;
    }

    private static Set<String> collectAltStartIds(final SuperNode target) {
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

    private static List<SuperNode> findPathToSuperNode(final SuperNode root, final String targetId) {
        if (root == null) return null;
        List<SuperNode> path = new ArrayList<>();
        if (findPathRecursive(root, targetId, path)) {
            return path;
        }
        return null;
    }

    private static boolean findPathRecursive(final SuperNode current,
                                             final String targetId,
                                             final List<SuperNode> path) {
        path.add(current);
        String currentId = current.getId();
        if ((currentId != null && currentId.equals(targetId)) ||
                (currentId == null && targetId == null) ||
                ("__root__".equals(targetId) && current.getParentNode() == null)) {
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
}
