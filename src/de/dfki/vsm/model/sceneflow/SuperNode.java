package de.dfki.vsm.model.sceneflow;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.model.sceneflow.definition.VarDef;
import de.dfki.vsm.model.sceneflow.definition.type.TypeDef;
import de.dfki.vsm.model.sceneflow.graphics.node.Graphics;
import de.dfki.vsm.util.cpy.CopyTool;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.tpl.TPLTuple;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

/**
 * @author Not me
 */
public class SuperNode extends Node {
    protected Vector<Comment>       mCommentList         = new Vector<Comment>();
    protected Vector<Node>          mNodeList            = new Vector<Node>();
    protected Vector<SuperNode>     mSuperNodeList       = new Vector<SuperNode>();
    protected HashMap<String, Node> mStartNodeMap        = new HashMap<String, Node>();
    protected Node                  mHistoryNode         = null;
    protected boolean               mHideLocalVarBadge   = false;
    protected boolean               mHideGlobalVarBadge  = false;
    protected VariableBadge         mLocalVariableBadge  = new VariableBadge("LocalVariableBadge");
    protected VariableBadge         mGlobalVariableBadge = new VariableBadge("GlobalVariableBadge");

    public SuperNode() {}

    public SuperNode(Node node) {
        mId            = node.mId;
        mName          = node.mName;
        mComment       = node.mComment;
        mExhaustive    = node.mExhaustive;
        mPreserving    = node.mPreserving;
        mTypeDefList   = node.mTypeDefList;
        mVarDefList    = node.mVarDefList;
        mCmdList       = node.mCmdList;
        mCEdgeList     = node.mCEdgeList;
        mPEdgeList     = node.mPEdgeList;
        mIEdgeList     = node.mIEdgeList;
        mFEdgeList     = node.mFEdgeList;
        mDEdge         = node.mDEdge;
        mGraphics      = node.mGraphics;
        mParentNode    = node.mParentNode;
        mIsHistoryNode = node.mIsHistoryNode;
    }

    public void addComment(Comment value) {
        mCommentList.add(value);
    }

    public void hideGlobalVarBadge(Boolean value) {
        mHideGlobalVarBadge = value;
    }

    public Boolean isGlobalVarBadgeHidden() {
        return mHideGlobalVarBadge;
    }

    public void hideLocalVarBadge(Boolean value) {
        mHideLocalVarBadge = value;
    }

    public Boolean isLocalVarBadgeHidden() {
        return mHideLocalVarBadge;
    }

    public void removeComment(Comment value) {
        mCommentList.remove(value);
    }

    public Vector<Comment> getCommentList() {
        return mCommentList;
    }

    public void setHistoryNode(Node value) {
        mHistoryNode = value;
    }

    public Node getHistoryNode() {
        return mHistoryNode;
    }

    public HashMap<String, Node> getStartNodeMap() {
        return mStartNodeMap;
    }

    public void setStartNodeMap(HashMap<String, Node> value) {
        mStartNodeMap = value;
    }

    public void addStartNode(Node node) {
        mStartNodeMap.put(node.getId(), node);
    }

    public void removeStartNode(Node node) {
        mStartNodeMap.remove(node.getId());
    }

    // TODO: this is not a deep copy
    public HashMap<String, Node> getCopyOfStartNodeMap() {
        HashMap<String, Node> copy = new HashMap<String, Node>();
        Iterator              it   = mStartNodeMap.entrySet().iterator();

        while (it.hasNext()) {
            Map.Entry pairs    = (Map.Entry) it.next();
            String    nodeId   = (String) pairs.getKey();
            Node      nodeData = (Node) pairs.getValue();

            copy.put(nodeId, nodeData);
        }

        return copy;
    }

    public void addSuperNode(SuperNode value) {
        mSuperNodeList.add(value);
    }

    public void removeSuperNode(SuperNode value) {
        mSuperNodeList.remove(value);
    }

    public SuperNode getSuperNodeAt(int index) {
        return mSuperNodeList.get(index);
    }

    public Vector<SuperNode> getSuperNodeList() {
        return mSuperNodeList;
    }

    public Vector<SuperNode> getCopyOfSuperNodeList() {
        Vector<SuperNode> copy = new Vector<SuperNode>();

        for (SuperNode node : mSuperNodeList) {
            copy.add(node.getCopy());
        }

        return copy;
    }

    public void addNode(Node value) {
        mNodeList.add(value);
    }

    public void removeNode(Node value) {
        mNodeList.remove(value);
    }

    public Node getNodeAt(int index) {
        return mNodeList.get(index);
    }

    public Vector<Node> getNodeList() {
        return mNodeList;
    }

    public Vector<Node> getCopyOfNodeList() {
        Vector<Node> copy = new Vector<Node>();

        for (Node node : mNodeList) {
            copy.add(node.getCopy());
        }

        return copy;
    }

    public Vector<Node> getNodeAndSuperNodeList() {
        Vector<Node> list = new Vector<Node>();

        for (Node n : mNodeList) {
            list.add(n);
        }

        for (SuperNode sn : mSuperNodeList) {
            list.add(sn);
        }

        return list;
    }

    public Vector<Node> getCopyOfNodeAndSuperNodeList() {
        Vector<Node> copy = new Vector<Node>();

        for (Node n : mNodeList) {
            copy.add(n.getCopy());
        }

        for (SuperNode sn : mSuperNodeList) {
            copy.add(sn.getCopy());
        }

        return copy;
    }

    public Node getChildNodeById(String id) {
        for (Node node : getNodeAndSuperNodeList()) {
            if (node.getId().equals(id)) {
                return node;
            }
        }

        return null;
    }

    public VariableBadge getLocalVariableBadge() {
        return mLocalVariableBadge;
    }

    public void setLocalVariableBadge(VariableBadge vb) {
        mLocalVariableBadge = vb;
    }

    public VariableBadge getGlobalVariableBadge() {
        return mGlobalVariableBadge;
    }

    public void setGlobalVariableBadge(VariableBadge vb) {
        mGlobalVariableBadge = vb;
    }

    @Override
    public void establishTargetNodes() {
        super.establishTargetNodes();

        for (Node node : getNodeAndSuperNodeList()) {
            node.establishTargetNodes();
        }
    }

    public void establishStartNodes() {
        for (String id : mStartNodeMap.keySet()) {
            mStartNodeMap.put(id, getChildNodeById(id));
        }

        for (SuperNode node : mSuperNodeList) {
            node.establishStartNodes();
        }
    }

    // TODO:
    public void establishAltStartNodes() {
        for (Node node : getNodeAndSuperNodeList()) {
            for (Edge edge : node.getEdgeList()) {
                if (edge.getTargetNode() instanceof SuperNode) {

                    // First establish the start nodes
                    for (TPLTuple<String, Node> startNodePair : edge.getAltStartNodeMap().keySet()) {
                        if (!startNodePair.getFirst().equals("")) {
                            Node n = ((SuperNode) edge.getTargetNode()).getChildNodeById(startNodePair.getFirst());

                            startNodePair.setSecond(n);
                        }
                    }

                    // Second establish the alternative nodes
                    for (TPLTuple<String, Node> altStartNodePair : edge.getAltStartNodeMap().values()) {
                        Node n = ((SuperNode) edge.getTargetNode()).getChildNodeById(altStartNodePair.getFirst());

                        altStartNodePair.setSecond(n);
                    }
                }
            }
        }
    }

    @Override
    public SuperNode getCopy() {
        return (SuperNode) CopyTool.copy(this);
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        String start = "";

        for (String id : mStartNodeMap.keySet()) {
            start += id + ";";
        }

        out.println("<SuperNode id=\"" + mId + "\" name=\"" + mName + "\" comment=\"" + mComment + "\" exhaustive=\""
                    + mExhaustive + "\" preserving=\"" + mPreserving + "\" hideLocalVar=\"" + mHideLocalVarBadge
                    + "\" hideGlobalVar=\"" + mHideGlobalVarBadge + "\" start=\"" + start + "\">").push();

        int i = 0;

        out.println("<Define>").push();

        for (i = 0; i < mTypeDefList.size(); i++) {
            mTypeDefList.get(i).writeXML(out);
        }

        out.pop().println("</Define>");
        out.println("<Declare>").push();

        for (i = 0; i < mVarDefList.size(); i++) {
            mVarDefList.get(i).writeXML(out);
        }

        out.pop().println("</Declare>");
        out.println("<Commands>").push();

        for (i = 0; i < mCmdList.size(); i++) {
            mCmdList.get(i).writeXML(out);
        }

        out.pop().println("</Commands>");

        for (i = 0; i < mCEdgeList.size(); i++) {
            mCEdgeList.get(i).writeXML(out);
        }

        if (mDEdge != null) {
            mDEdge.writeXML(out);
        }

        for (i = 0; i < mPEdgeList.size(); i++) {
            mPEdgeList.get(i).writeXML(out);
        }

        for (i = 0; i < mFEdgeList.size(); i++) {
            mFEdgeList.get(i).writeXML(out);
        }

        for (i = 0; i < mIEdgeList.size(); i++) {
            mIEdgeList.get(i).writeXML(out);
        }

        if (mGraphics != null) {
            mGraphics.writeXML(out);
        }

        if (mLocalVariableBadge != null) {
            mLocalVariableBadge.writeXML(out);
        }

        if (mGlobalVariableBadge != null) {
            mGlobalVariableBadge.writeXML(out);
        }

        for (i = 0; i < mCommentList.size(); i++) {
            mCommentList.get(i).writeXML(out);
        }

        for (i = 0; i < mNodeList.size(); i++) {
            mNodeList.get(i).writeXML(out);
        }

        for (i = 0; i < mSuperNodeList.size(); i++) {
            mSuperNodeList.get(i).writeXML(out);
        }

        out.pop().println("</SuperNode>");
    }

    @Override
    public void parseXML(Element element) throws XMLParseError {
        mId                 = element.getAttribute("id");
        mName               = element.getAttribute("name");
        mComment            = element.getAttribute("comment");
        mExhaustive         = Boolean.valueOf(element.getAttribute("exhaustive"));
        mPreserving         = Boolean.valueOf(element.getAttribute("preserving"));
        mHideLocalVarBadge  = Boolean.valueOf(element.getAttribute("hideLocalVar"));
        mHideGlobalVarBadge = Boolean.valueOf(element.getAttribute("hideGlobalVar"));

        String[] arr = element.getAttribute("start").split(";");

        for (String str : arr) {
            if (!str.isEmpty()) {
                mStartNodeMap.put(str, null);
            }
        }

        final SuperNode superNode = this;

        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                java.lang.String tag = element.getTagName();

                if (tag.equals("Define")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        public void run(Element element) throws XMLParseError {
                            mTypeDefList.add(TypeDef.parse(element));
                        }
                    });
                } else if (tag.equals("Declare")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        public void run(Element element) throws XMLParseError {
                            VarDef def = new VarDef();

                            def.parseXML(element);
                            mVarDefList.add(def);
                        }
                    });
                } else if (tag.equals("Commands")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        public void run(Element element) throws XMLParseError {
                            mCmdList.add(Command.parse(element));
                        }
                    });
                } else if (tag.equals("LocalVariableBadge")) {
                    VariableBadge varBadge = new VariableBadge("LocalVariableBadge");

                    varBadge.parseXML(element);
                    mLocalVariableBadge = varBadge;
                } else if (tag.equals("GlobalVariableBadge")) {
                    VariableBadge varBadge = new VariableBadge("GlobalVariableBadge");

                    varBadge.parseXML(element);
                    mGlobalVariableBadge = varBadge;
                } else if (tag.equals("VariableBadge")) {

                    // do nothing (left for old project's compatibility)
                } else if (tag.equals("Comment")) {
                    Comment comment = new Comment();

                    comment.parseXML(element);
                    comment.setParentNode(superNode);
                    mCommentList.add(comment);
                } else if (tag.equals("Node")) {
                    Node node = new Node();

                    node.parseXML(element);
                    node.setParentNode(superNode);
                    mNodeList.add(node);

                    if (node.isHistoryNode()) {
                        mHistoryNode = node;
                    }
                } else if (tag.equals("SuperNode")) {
                    SuperNode node = new SuperNode();

                    node.parseXML(element);
                    node.setParentNode(superNode);
                    mSuperNodeList.add(node);
                } else if (tag.equals("Graphics")) {
                    mGraphics = new Graphics();
                    mGraphics.parseXML(element);
                } else if (tag.equals("CEdge")) {
                    CEdge edge = new CEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(superNode);
                    edge.setSource(superNode.getId());
                    mCEdgeList.add(edge);
                } else if (tag.equals("PEdge")) {
                    PEdge edge = new PEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(superNode);
                    edge.setSource(superNode.getId());
                    mPEdgeList.add(edge);
                } else if (tag.equals("FEdge")) {
                    FEdge edge = new FEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(superNode);
                    edge.setSource(superNode.getId());
                    mFEdgeList.add(edge);
                } else if (tag.equals("IEdge")) {
                    IEdge edge = new IEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(superNode);
                    edge.setSource(superNode.getId());
                    mIEdgeList.add(edge);
                } else if (tag.equals("EEdge")) {
                    EEdge edge = new EEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(superNode);
                    edge.setSource(superNode.getId());
                    mDEdge = edge;
                } else if (tag.equals("TEdge")) {
                    TEdge edge = new TEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(superNode);
                    edge.setSource(superNode.getId());
                    mDEdge = edge;
                } else {
                    throw new XMLParseError(null,
                                            "Cannot parse the element with the tag \"" + tag
                                            + "\" into a supernode child!");
                }
            }
        });
    }

    public int getHashCode() {

        // Add hash of General Attributes
        int hashCode =  ((mName == null)? 0: mName.hashCode()) +
                        ((mComment == null)? 0 : mComment.hashCode()) + 
                        ((mGraphics == null)? 0: mGraphics.toString().hashCode()) +
                        ((mHistoryNode == null)? 0: mHistoryNode.hashCode()) + 
                        ((mLocalVariableBadge == null)? 0: mLocalVariableBadge.hashCode()) + 
                        ((mGlobalVariableBadge == null)? 0: mGlobalVariableBadge.hashCode()) + 
                        ((mHideLocalVarBadge == true)? 1: 0) + ((mHideGlobalVarBadge == true)? 1: 0);

        // Add hash of all commands inside SuperNode
        for (int cntCommand = 0; cntCommand < getSizeOfCmdList(); cntCommand++) {
            hashCode += mCmdList.get(cntCommand).hashCode();
        }

        // Add hash of all TypeDef inside SuperNode
        for (int cntType = 0; cntType < getSizeOfTypeDefList(); cntType++) {
            hashCode += mTypeDefList.get(cntType).hashCode() + mTypeDefList.get(cntType).getName().hashCode()
                        + mTypeDefList.get(cntType).toString().hashCode();
        }

        // Add hash of all VarDef inside SuperNode
        for (int cntVar = 0; cntVar < getVarDefList().size(); cntVar++) {
            hashCode += getVarDefList().get(cntVar).getName().hashCode()
                        + getVarDefList().get(cntVar).getType().hashCode()
                        + getVarDefList().get(cntVar).toString().hashCode();
        }

        // Add hash of all Nodes inside SuperNode
        for (int cntNode = 0; cntNode < mNodeList.size(); cntNode++) {
            hashCode += getNodeAt(cntNode).getHashCode();
        }

        // Epsilon and Time Edges
        for (int cntEdge = 0; cntEdge < getEdgeList().size(); cntEdge++) {
            hashCode += getEdgeList().get(cntEdge).hashCode() + getEdgeList().get(cntEdge).mGraphics.getHashCode();

            // TODO: find a way to parse the TEDGE mDEGE to take timeout into accout
        }

        // Add hash of all Conditional Edges
        for (int cntEdge = 0; cntEdge < getSizeOfCEdgeList(); cntEdge++) {
            hashCode += mCEdgeList.get(cntEdge).hashCode() + mCEdgeList.get(cntEdge).mGraphics.getHashCode()
                        + mCEdgeList.get(cntEdge).mCondition.hashCode() + mCEdgeList.get(cntEdge).mSource.hashCode()
                        + mCEdgeList.get(cntEdge).mTarget.hashCode();
        }

        // Add hash of all Probability Edges
        for (int cntEdge = 0; cntEdge < getSizeOfPEdgeList(); cntEdge++) {
            hashCode += mPEdgeList.get(cntEdge).hashCode() + mPEdgeList.get(cntEdge).mGraphics.getHashCode()
                        + mPEdgeList.get(cntEdge).getProbability() + mPEdgeList.get(cntEdge).mSource.hashCode()
                        + mPEdgeList.get(cntEdge).mTarget.hashCode();
        }

        // Add hash of all Fork Edges
        for (int cntEdge = 0; cntEdge < mFEdgeList.size(); cntEdge++) {
            hashCode += mFEdgeList.get(cntEdge).hashCode() + mFEdgeList.get(cntEdge).mGraphics.getHashCode()
                        + mFEdgeList.get(cntEdge).mSource.hashCode() + mFEdgeList.get(cntEdge).mTarget.hashCode();
        }

        // Add hash of all Interruptive Edges
        for (int cntEdge = 0; cntEdge < getSizeOfIEdgeList(); cntEdge++) {
            hashCode += mIEdgeList.get(cntEdge).hashCode() + mIEdgeList.get(cntEdge).mGraphics.getHashCode()
                        + mIEdgeList.get(cntEdge).mCondition.hashCode() + mIEdgeList.get(cntEdge).mSource.hashCode()
                        + mIEdgeList.get(cntEdge).mTarget.hashCode();
        }

        // Check existing SuperNodes inside of this SuperNode
        for (int cntSNode = 0; cntSNode < mSuperNodeList.size(); cntSNode++) {
            hashCode += getSuperNodeAt(cntSNode).getHashCode();
        }
        
        // Add hash of all comments on workspace
        for (int cntComment = 0; cntComment < getCommentList().size(); cntComment++) {          
            hashCode += mCommentList.get(cntComment).mGraphics.getRect().hashCode();
            //hashCode += mCommentList.get(cntComment).getHTMLText().hashCode();
        }

        return hashCode;
    }
}
