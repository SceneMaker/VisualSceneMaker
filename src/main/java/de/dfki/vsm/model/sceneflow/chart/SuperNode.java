package de.dfki.vsm.model.sceneflow.chart;

import de.dfki.vsm.model.sceneflow.chart.badge.CommentBadge;
import de.dfki.vsm.model.sceneflow.chart.badge.VariableBadge;
import de.dfki.vsm.model.sceneflow.chart.edge.RandomEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.ForkingEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.EpsilonEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.DataTypeDefinition;
import de.dfki.vsm.model.sceneflow.chart.graphics.node.NodeGraphics;
import de.dfki.vsm.util.cpy.CopyTool;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.tpl.TPLTuple;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.util.ArrayList;
import org.w3c.dom.Element;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * @author Gregor Mehlmann
 */
public class SuperNode extends BasicNode {

    protected ArrayList<CommentBadge> mCommentList = new ArrayList<CommentBadge>();
    protected ArrayList<BasicNode> mNodeList = new ArrayList<BasicNode>();
    protected ArrayList<SuperNode> mSuperNodeList = new ArrayList<SuperNode>();
    protected HashMap<String, BasicNode> mStartNodeMap = new HashMap<String, BasicNode>();
    protected BasicNode mHistoryNode = null;
    protected boolean mHideLocalVarBadge = false;
    protected boolean mHideGlobalVarBadge = false;
    protected VariableBadge mLocalVariableBadge = new VariableBadge("LocalVariableBadge");
    protected VariableBadge mGlobalVariableBadge = new VariableBadge("GlobalVariableBadge");

    public SuperNode() {
    }

    public SuperNode(final BasicNode node) {
        mNodeId = node.mNodeId;
        mNodeName = node.mNodeName;
        mComment = node.mComment;
        mTypeDefList = node.mTypeDefList;
        mVarDefList = node.mVarDefList;
        mCmdList = node.mCmdList;
        mCEdgeList = node.mCEdgeList;
        mPEdgeList = node.mPEdgeList;
        mIEdgeList = node.mIEdgeList;
        mFEdgeList = node.mFEdgeList;
        mDEdge = node.mDEdge;
        mGraphics = node.mGraphics;
        mParentNode = node.mParentNode;
        mIsHistoryNode = node.mIsHistoryNode;
    }

    public void addComment(CommentBadge value) {
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

    public void removeComment(CommentBadge value) {
        mCommentList.remove(value);
    }

    public ArrayList<CommentBadge> getCommentList() {
        return mCommentList;
    }

    public void setHistoryNode(BasicNode value) {
        mHistoryNode = value;
    }

    public BasicNode getHistoryNode() {
        return mHistoryNode;
    }

    public HashMap<String, BasicNode> getStartNodeMap() {
        return mStartNodeMap;
    }

    public void setStartNodeMap(HashMap<String, BasicNode> value) {
        mStartNodeMap = value;
    }

    public void addStartNode(BasicNode node) {
        mStartNodeMap.put(node.getId(), node);
    }

    public void removeStartNode(BasicNode node) {
        mStartNodeMap.remove(node.getId());
    }

    // TODO: this is not a deep copy
    public HashMap<String, BasicNode> getCopyOfStartNodeMap() {
        HashMap<String, BasicNode> copy = new HashMap<String, BasicNode>();
        Iterator it = mStartNodeMap.entrySet().iterator();

        while (it.hasNext()) {
            Map.Entry pairs = (Map.Entry) it.next();
            String nodeId = (String) pairs.getKey();
            BasicNode nodeData = (BasicNode) pairs.getValue();

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

    public ArrayList<SuperNode> getSuperNodeList() {
        return mSuperNodeList;
    }

    public ArrayList<SuperNode> getCopyOfSuperNodeList() {
        ArrayList<SuperNode> copy = new ArrayList<SuperNode>();

        for (SuperNode node : mSuperNodeList) {
            copy.add(node.getCopy());
        }

        return copy;
    }

    public void addNode(BasicNode value) {
        mNodeList.add(value);
    }

    public void removeNode(BasicNode value) {
        mNodeList.remove(value);
    }

    public BasicNode getNodeAt(int index) {
        return mNodeList.get(index);
    }

    public ArrayList<BasicNode> getNodeList() {
        return mNodeList;
    }

    public ArrayList<BasicNode> getCopyOfNodeList() {
        ArrayList<BasicNode> copy = new ArrayList<BasicNode>();

        for (BasicNode node : mNodeList) {
            copy.add(node.getCopy());
        }

        return copy;
    }

    public ArrayList<BasicNode> getNodeAndSuperNodeList() {
        ArrayList<BasicNode> list = new ArrayList<BasicNode>();

        for (BasicNode n : mNodeList) {
            list.add(n);
        }

        for (SuperNode sn : mSuperNodeList) {
            list.add(sn);
        }

        return list;
    }

    public ArrayList<BasicNode> getCopyOfNodeAndSuperNodeList() {
        ArrayList<BasicNode> copy = new ArrayList<BasicNode>();

        for (BasicNode n : mNodeList) {
            copy.add(n.getCopy());
        }

        for (SuperNode sn : mSuperNodeList) {
            copy.add(sn.getCopy());
        }

        return copy;
    }

    public BasicNode getChildNodeById(String id) {
        for (BasicNode node : getNodeAndSuperNodeList()) {
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

        for (BasicNode node : getNodeAndSuperNodeList()) {
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
        for (BasicNode node : getNodeAndSuperNodeList()) {
            for (AbstractEdge edge : node.getEdgeList()) {
                if (edge.getTargetNode() instanceof SuperNode) {

                    // First establish the start nodes
                    for (TPLTuple<String, BasicNode> startNodePair : edge.getAltMap().keySet()) {
                        if (!startNodePair.getFirst().equals("")) {
                            BasicNode n = ((SuperNode) edge.getTargetNode()).getChildNodeById(startNodePair.getFirst());

                            startNodePair.setSecond(n);
                        }
                    }

                    // Second establish the alternative nodes
                    for (TPLTuple<String, BasicNode> altStartNodePair : edge.getAltMap().values()) {
                        BasicNode n = ((SuperNode) edge.getTargetNode()).getChildNodeById(altStartNodePair.getFirst());

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

        out.println("<SuperNode id=\"" + mNodeId + "\" name=\"" + mNodeName + "\" comment=\"" + mComment + "\" hideLocalVar=\"" + mHideLocalVarBadge
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
        mNodeId = element.getAttribute("id");
        mNodeName = element.getAttribute("name");
        mComment = element.getAttribute("comment");
        mHideLocalVarBadge = Boolean.valueOf(element.getAttribute("hideLocalVar"));
        mHideGlobalVarBadge = Boolean.valueOf(element.getAttribute("hideGlobalVar"));

        String[] arr = element.getAttribute("start").split(";");

        for (String str : arr) {
            if (!str.isEmpty() && !str.equals("null")) {
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
                            mTypeDefList.add(DataTypeDefinition.parse(element));
                        }
                    });
                } else if (tag.equals("Declare")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        public void run(Element element) throws XMLParseError {
                            VariableDefinition def = new VariableDefinition();
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
                    CommentBadge comment = new CommentBadge();

                    comment.parseXML(element);
                    comment.setParentNode(superNode);
                    mCommentList.add(comment);
                } else if (tag.equals("Node")) {
                    BasicNode node = new BasicNode();

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
                    mGraphics = new NodeGraphics();
                    mGraphics.parseXML(element);
                } else if (tag.equals("CEdge")) {
                    GuargedEdge edge = new GuargedEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(superNode);
                    edge.setSourceUnid(superNode.getId());
                    mCEdgeList.add(edge);
                } else if (tag.equals("PEdge")) {
                    RandomEdge edge = new RandomEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(superNode);
                    edge.setSourceUnid(superNode.getId());
                    mPEdgeList.add(edge);
                } else if (tag.equals("FEdge")) {
                    ForkingEdge edge = new ForkingEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(superNode);
                    edge.setSourceUnid(superNode.getId());
                    mFEdgeList.add(edge);
                } else if (tag.equals("IEdge")) {
                    InterruptEdge edge = new InterruptEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(superNode);
                    edge.setSourceUnid(superNode.getId());
                    mIEdgeList.add(edge);
                } else if (tag.equals("EEdge")) {
                    EpsilonEdge edge = new EpsilonEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(superNode);
                    edge.setSourceUnid(superNode.getId());
                    mDEdge = edge;
                } else if (tag.equals("TEdge")) {
                    TimeoutEdge edge = new TimeoutEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(superNode);
                    edge.setSourceUnid(superNode.getId());
                    mDEdge = edge;
                } else {
                    throw new XMLParseError(null,
                            "Cannot parse the element with the tag \"" + tag
                            + "\" into a supernode child!");
                }
            }
        });
    }

    @Override
    public int getHashCode() {

        // Add hash of General Attributes
        int hashCode = ((mNodeName == null) ? 0 : mNodeName.hashCode())
                + ((mComment == null) ? 0 : mComment.hashCode())
                + ((mGraphics == null) ? 0 : mGraphics.toString().hashCode())
                + ((mHistoryNode == null) ? 0 : mHistoryNode.hashCode())
                + ((mLocalVariableBadge == null) ? 0 : mLocalVariableBadge.hashCode())
                + ((mGlobalVariableBadge == null) ? 0 : mGlobalVariableBadge.hashCode())
                + ((mHideLocalVarBadge == true) ? 1 : 0) + ((mHideGlobalVarBadge == true) ? 1 : 0);

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
            hashCode += getEdgeList().get(cntEdge).hashCode() + getEdgeList().get(cntEdge).getGraphics().getHashCode();

            // TODO: find a way to parse the TEDGE mDEGE to take timeout into accout
        }

        // Add hash of all Conditional Edges
        for (int cntEdge = 0; cntEdge < getSizeOfCEdgeList(); cntEdge++) {
    
            hashCode += mCEdgeList.get(cntEdge).hashCode()
                    + mCEdgeList.get(cntEdge).getGraphics().getHashCode()
                    + mCEdgeList.get(cntEdge).getCondition().hashCode()
                    + mCEdgeList.get(cntEdge).getSourceUnid().hashCode()
                    + mCEdgeList.get(cntEdge).getTargetUnid().hashCode();
        }
        // Add hash of all Probability Edges
        for (int cntEdge = 0; cntEdge < getSizeOfPEdgeList(); cntEdge++) {

            hashCode += mPEdgeList.get(cntEdge).hashCode()
                    + mPEdgeList.get(cntEdge).getGraphics().getHashCode()
                    + mPEdgeList.get(cntEdge).getProbability()
                    + mPEdgeList.get(cntEdge).getSourceUnid().hashCode()
                    + mPEdgeList.get(cntEdge).getTargetUnid().hashCode();
        }

        // Add hash of all Fork Edges
        for (int cntEdge = 0; cntEdge < mFEdgeList.size(); cntEdge++) {

            hashCode += mFEdgeList.get(cntEdge).hashCode()
                    + mFEdgeList.get(cntEdge).getGraphics().getHashCode()
                    + mFEdgeList.get(cntEdge).getSourceUnid().hashCode()
                    + mFEdgeList.get(cntEdge).getTargetUnid().hashCode();
        }

        // Add hash of all Interruptive Edges
        for (int cntEdge = 0; cntEdge < getSizeOfIEdgeList(); cntEdge++) {
            hashCode += mIEdgeList.get(cntEdge).hashCode() 
                    + mIEdgeList.get(cntEdge).getGraphics().getHashCode()
                    + mIEdgeList.get(cntEdge).getCondition().hashCode()
                    + mIEdgeList.get(cntEdge).getSourceUnid().hashCode()
                    + mIEdgeList.get(cntEdge).getTargetUnid().hashCode();
        }

        // Check existing SuperNodes inside of this SuperNode
        for (int cntSNode = 0; cntSNode < mSuperNodeList.size(); cntSNode++) {
            hashCode += getSuperNodeAt(cntSNode).getHashCode();
        }

        // Add hash of all comments on workspace
        for (int cntComment = 0; cntComment < getCommentList().size(); cntComment++) {
            hashCode += mCommentList.get(cntComment).getGraphics().getRectangle().hashCode();
            //hashCode += mCommentList.get(cntComment).getHTMLText().hashCode();
        }

        return hashCode;
    }
}
