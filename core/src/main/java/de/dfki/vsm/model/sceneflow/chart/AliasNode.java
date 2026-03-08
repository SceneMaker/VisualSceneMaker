package de.dfki.vsm.model.sceneflow.chart;

import de.dfki.vsm.model.sceneflow.chart.edge.*;
import de.dfki.vsm.model.sceneflow.chart.graphics.node.NodeGraphics;
import de.dfki.vsm.util.cpy.CopyTool;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * A visual alias (reference copy) of a canonical SuperNode.
 *
 * <p>The alias carries its own position, ID, name and outgoing edges, but delegates
 * all <em>internal</em> structure — start nodes, child nodes, history node,
 * variable/type definitions and commands — to the referenced canonical SuperNode.
 *
 * <p>XML: {@code <AliasNode id="..." name="..." comment="..." refId="...">
 *   <Graphics>...</Graphics>  <!-- optional outgoing edges -->
 * </AliasNode>}
 *
 * <p>Only top-level SuperNodes (direct children of {@link SceneFlow}) are allowed
 * as canonical targets.  Resolution of {@link #mCanonicalNode} is performed after
 * the full tree is parsed by calling {@link #resolve(Map)}.
 */
public class AliasNode extends SuperNode {

    /** Persistent: ID of the canonical SuperNode this alias refers to. */
    private String mRefId = "";

    /** Resolved at load time.  Not serialised. */
    private transient SuperNode mCanonicalNode = null;

    public AliasNode() {
    }

    // ===== Ref-id accessors =====

    public String getRefId() {
        return mRefId;
    }

    public void setRefId(String refId) {
        mRefId = refId != null ? refId : "";
    }

    public SuperNode getCanonicalNode() {
        return mCanonicalNode;
    }

    /**
     * Wires up {@link #mCanonicalNode} from the top-level index built by
     * {@link SceneFlow#establishAliases()}.
     */
    public void resolve(Map<String, SuperNode> topLevelIndex) {
        mCanonicalNode = topLevelIndex.get(mRefId);
    }

    // ===== Delegation to canonical =====

    @Override
    public HashMap<String, BasicNode> getStartNodeMap() {
        return mCanonicalNode != null ? mCanonicalNode.getStartNodeMap() : new HashMap<>();
    }

    @Override
    public BasicNode getHistoryNode() {
        return mCanonicalNode != null ? mCanonicalNode.getHistoryNode() : null;
    }

    @Override
    public ArrayList<BasicNode> getNodeAndSuperNodeList() {
        return mCanonicalNode != null ? mCanonicalNode.getNodeAndSuperNodeList() : new ArrayList<>();
    }

    @Override
    public ArrayList<BasicNode> getNodeList() {
        return mCanonicalNode != null ? mCanonicalNode.getNodeList() : new ArrayList<>();
    }

    /**
     * Returns empty — the alias owns no sub-supernodes.
     * The canonical's children are accessible via {@link #getNodeAndSuperNodeList()}.
     * Returning empty here prevents parent traversals from double-visiting the
     * canonical's children.
     */
    @Override
    public ArrayList<SuperNode> getSuperNodeList() {
        return new ArrayList<>();
    }

    @Override
    public BasicNode getChildNodeById(String id) {
        return mCanonicalNode != null ? mCanonicalNode.getChildNodeById(id) : null;
    }

    // ===== Establishment =====

    /**
     * Only establishes edges owned by <em>this</em> alias node (outgoing transitions
     * to sibling nodes in the same parent).  Does NOT recurse into the canonical's
     * children — they are established when the canonical itself is processed.
     */
    @Override
    public void establishTargetNodes() {
        for (AbstractEdge edge : getEdgeList()) {
            if (mParentNode != null) {
                edge.setTargetNode(mParentNode.getChildNodeById(edge.getTargetUnid()));
            }
        }
    }

    /** No-op: canonical's start nodes are already established by the canonical itself. */
    @Override
    public void establishStartNodes() {
    }

    // ===== XML serialisation =====

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<AliasNode id=\"" + mNodeId + "\" name=\"" + mNodeName
                + "\" comment=\"" + mComment + "\" refId=\"" + mRefId + "\">").push();

        for (int i = 0; i < mCEdgeList.size(); i++) mCEdgeList.get(i).writeXML(out);
        if (mDEdge != null) mDEdge.writeXML(out);
        for (int i = 0; i < mPEdgeList.size(); i++) mPEdgeList.get(i).writeXML(out);
        for (int i = 0; i < mFEdgeList.size(); i++) mFEdgeList.get(i).writeXML(out);
        for (int i = 0; i < mIEdgeList.size(); i++) mIEdgeList.get(i).writeXML(out);

        if (mGraphics != null) mGraphics.writeXML(out);

        out.pop().println("</AliasNode>");
    }

    @Override
    public void parseXML(Element element) throws XMLParseError {
        mNodeId  = element.getAttribute("id");
        mNodeName = element.getAttribute("name");
        mComment  = element.getAttribute("comment");
        mRefId    = element.getAttribute("refId");

        final AliasNode self = this;

        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element el) throws XMLParseError {
                String tag = el.getTagName();
                switch (tag) {
                    case "Graphics":
                        mGraphics = new NodeGraphics();
                        mGraphics.parseXML(el);
                        break;
                    case "CEdge": {
                        GuargedEdge edge = new GuargedEdge();
                        edge.parseXML(el);
                        edge.setSourceNode(self);
                        edge.setSourceUnid(self.getId());
                        mCEdgeList.add(edge);
                        break;
                    }
                    case "PEdge": {
                        RandomEdge edge = new RandomEdge();
                        edge.parseXML(el);
                        edge.setSourceNode(self);
                        edge.setSourceUnid(self.getId());
                        mPEdgeList.add(edge);
                        break;
                    }
                    case "FEdge": {
                        ForkingEdge edge = new ForkingEdge();
                        edge.parseXML(el);
                        edge.setSourceNode(self);
                        edge.setSourceUnid(self.getId());
                        mFEdgeList.add(edge);
                        break;
                    }
                    case "IEdge": {
                        InterruptEdge edge = new InterruptEdge();
                        edge.parseXML(el);
                        edge.setSourceNode(self);
                        edge.setSourceUnid(self.getId());
                        mIEdgeList.add(edge);
                        break;
                    }
                    case "EEdge": {
                        EpsilonEdge edge = new EpsilonEdge();
                        edge.parseXML(el);
                        edge.setSourceNode(self);
                        edge.setSourceUnid(self.getId());
                        mDEdge = edge;
                        break;
                    }
                    case "TEdge": {
                        TimeoutEdge edge = new TimeoutEdge();
                        edge.parseXML(el);
                        edge.setSourceNode(self);
                        edge.setSourceUnid(self.getId());
                        mDEdge = edge;
                        break;
                    }
                    default:
                        throw new XMLParseError(null,
                                "Cannot parse element \"" + tag + "\" into AliasNode child");
                }
            }
        });
    }

    @Override
    public AliasNode getCopy() {
        return (AliasNode) CopyTool.copy(this);
    }
}
