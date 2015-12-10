package de.dfki.vsm.model.sceneflow.diagram.nodes;

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.model.sceneflow.diagram.edges.RandomEdge;
import de.dfki.vsm.model.sceneflow.diagram.edges.ForkingEdge;
import de.dfki.vsm.model.sceneflow.diagram.edges.GuardedEdge;
import de.dfki.vsm.model.sceneflow.diagram.edges.InterruptEdge;
import de.dfki.vsm.model.sceneflow.diagram.edges.EpsilonEdge;
import de.dfki.vsm.model.sceneflow.diagram.edges.AbstractEdge;
import de.dfki.vsm.model.sceneflow.diagram.edges.TimeoutEdge;
import de.dfki.vsm.model.sceneflow.language.command.Command;
import de.dfki.vsm.model.sceneflow.language.definition.VariableDefinition;
import de.dfki.vsm.model.sceneflow.diagram.graphics.node.NodeGraphics;
import de.dfki.vsm.util.cpy.CopyTool;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.util.ArrayList;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public class BasicNode implements ModelObject /*SyntaxObject*/ {

    protected String mId = new String();
    protected String mName = new String();
    protected String mComment = new String();
    //
    protected ArrayList<VariableDefinition> mVarDefList = new ArrayList();
    protected ArrayList<Command> mCmdList = new ArrayList();
    protected ArrayList<GuardedEdge> mCEdgeList = new ArrayList();
    protected ArrayList<RandomEdge> mPEdgeList = new ArrayList();
    protected ArrayList<InterruptEdge> mIEdgeList = new ArrayList();
    protected ArrayList<ForkingEdge> mFEdgeList = new ArrayList();
    //
    protected AbstractEdge mDEdge = null;
    protected NodeGraphics mGraphics = null;
    protected SuperNode mParentNode = null;
    protected boolean mIsHistoryNode = false;

    public Byte hasNone = new Byte("0");
    public Byte hasOne = new Byte("1");
    public Byte hasMany = new Byte("2");

    public enum FLAVOUR {

        NONE, ENODE, TNODE, CNODE, PNODE, INODE, FNODE
    };

    public BasicNode() {
    }

    public boolean isSubNodeOf(BasicNode node) {
        if (node instanceof SuperNode) {
            SuperNode parentNode = mParentNode;

            while (parentNode != null) {
                if (parentNode.equals(node)) {
                    return true;
                } else {
                    parentNode = parentNode.getParentNode();
                }
            }
        }

        return false;
    }

    public void setId(String value) {
        mId = value;
    }

    public String getId() {
        return mId;
    }

    public boolean isHistoryNode() {
        return mIsHistoryNode;
    }

    public void setHistoryNodeFlag(boolean value) {
        mIsHistoryNode = value;
    }

    public void setName(String value) {
        mName = value;
    }

    public String getName() {
        return mName;
    }

    public void setNameAndId(String value) {
        mId = value;
        mName = value;
    }

    public void setComment(String value) {
        mComment = value;
    }

    public String getComment() {
        return mComment;
    }

    public boolean hasComment() {
        if (mComment == null) {
            return false;
        }

        if (mComment.length() == 0) {
            return false;
        }

        return true;
    }

    public boolean hasEdge() {

        if (mDEdge != null) {
            return true;
        }

        if (mCEdgeList != null) {
            if (mCEdgeList.size() > 0) {
                return true;
            }
        }

        if (mPEdgeList != null) {
            if (mPEdgeList.size() > 0) {
                return true;
            }
        }

        if (mFEdgeList != null) {
            if (mFEdgeList.size() > 0) {
                return true;
            }
        }

        if (mIEdgeList != null) {
            if (mIEdgeList.size() > 0) {
                return true;
            }
        }

        return false;
    }

    /**
     * Tells if the node has more than 0 Probabilistic edge in case true, it is
     * necessary to reorganise the values of probabilities Used when deleting an
     * edge
     *
     * @return
     */
    public byte hasPEdges() {
        if (mPEdgeList.size() == 1) {
            return hasOne;
        }
        if (mPEdgeList.size() > 1) {
            return hasMany;
        }
        return hasNone;
    }

    public RandomEdge getFirstPEdge() {
        return mPEdgeList.get(0);
    }

    public boolean hasDEdge() {
        return (mDEdge != null);
    }

    public FLAVOUR getFlavour() {
        if (mCEdgeList != null) {
            if (mCEdgeList.size() > 0) {
                return FLAVOUR.CNODE;
            }
        }

        if (mPEdgeList != null) {
            if (mPEdgeList.size() > 0) {
                return FLAVOUR.PNODE;
            }
        }

        if (mFEdgeList != null) {
            if (mFEdgeList.size() > 0) {
                return FLAVOUR.FNODE;
            }
        }

        if (mIEdgeList != null) {
            if (mIEdgeList.size() > 0) {
                return FLAVOUR.INODE;
            }
        }

        if (mDEdge != null) {
            return (mDEdge instanceof TimeoutEdge)
                    ? FLAVOUR.TNODE
                    : FLAVOUR.ENODE;
        }

        return FLAVOUR.NONE;
    }

    public final void setDedge(AbstractEdge value) {
        mDEdge = value;
    }

    public final AbstractEdge getDedge() {
        return mDEdge;
    }

    public final void removeDEdge() {
        mDEdge = null;
    }

    public final void setGraphics(NodeGraphics value) {
        mGraphics = value;
    }

    public final NodeGraphics getGraphics() {
        return mGraphics;
    }

    public final void setParentNode(SuperNode value) {
        mParentNode = value;
    }

    public final SuperNode getParentNode() {
        return mParentNode;
    }

    public final void addVarDef(VariableDefinition value) {
        mVarDefList.add(value);
    }

    public final VariableDefinition getVarDefAt(int index) {
        return mVarDefList.get(index);
    }

    public final void setVarDefAt(final VariableDefinition value, final int index) {
        mVarDefList.set(index, value);
    }

    public final void removeVarDefAt(final int index) {
        mVarDefList.remove(index);
    }

    public final void setVarDefList(final ArrayList value) {
        mVarDefList = value;
    }

    public final ArrayList<VariableDefinition> getVarDefList() {
        return mVarDefList;
    }

    public final ArrayList<VariableDefinition> getCopyOfVarDefList() {
        ArrayList<VariableDefinition> copy = new ArrayList<VariableDefinition>();
        for (VariableDefinition varDef : mVarDefList) {
            copy.add(varDef.getCopy());
        }
        return copy;
    }

    public final void addCmd(Command value) {
        mCmdList.add(value);
    }

    public final Command getCmdAt(int index) {
        return mCmdList.get(index);
    }

    public final void setCmdAt(Command value, int index) {
        mCmdList.set(index, value);
    }

    public final void removeCmdAt(int index) {
        mCmdList.remove(index);
    }

    public final void setCmdList(ArrayList<Command> value) {
        mCmdList = value;
    }

    public final ArrayList<Command> getCmdList() {
        return mCmdList;
    }

    public final int getSizeOfCmdList() {
        return mCmdList.size();
    }

    public final ArrayList<Command> getCopyOfCmdList() {
        ArrayList<Command> copy = new ArrayList<Command>();

        for (Command cmd : mCmdList) {
            copy.add(cmd.getCopy());
        }

        return copy;
    }

    public final void addCEdge(GuardedEdge value) {
        mCEdgeList.add(value);
    }

    public final GuardedEdge getCEdgeAt(int index) {
        return mCEdgeList.get(index);
    }

    public final void removeCEdge(GuardedEdge value) {
        mCEdgeList.remove(value);
    }

    public final void removeAllCEdges() {
        mCEdgeList = new ArrayList<GuardedEdge>();
    }

    public final int getSizeOfCEdgeList() {
        return mCEdgeList.size();
    }

    public final ArrayList<GuardedEdge> getCEdgeList() {
        return mCEdgeList;
    }

    public final ArrayList<GuardedEdge> getCopyOfCEdgeList() {
        ArrayList<GuardedEdge> copy = new ArrayList<GuardedEdge>();

        for (GuardedEdge edge : mCEdgeList) {
            copy.add(edge.getCopy());
        }

        return copy;
    }

    public final void addFEdge(ForkingEdge value) {
        mFEdgeList.add(value);
    }

    public final void removeFEdge(ForkingEdge value) {
        mFEdgeList.remove(value);
    }

    public final void removeAllFEdges() {
        mFEdgeList = new ArrayList<ForkingEdge>();
    }

    public final ArrayList<ForkingEdge> getFEdgeList() {
        return mFEdgeList;
    }

    public final void addPEdge(RandomEdge value) {
        mPEdgeList.add(value);
    }

    public final RandomEdge getPEdgeAt(int index) {
        return mPEdgeList.get(index);
    }

    public final void removePEdge(RandomEdge value) {
        mPEdgeList.remove(value);
    }

    public final void removeAllPEdges() {
        mPEdgeList = new ArrayList<RandomEdge>();
    }

    public final int getSizeOfPEdgeList() {
        return mPEdgeList.size();
    }

    public ArrayList<RandomEdge> getPEdgeList() {
        return mPEdgeList;
    }

    public final ArrayList<RandomEdge> getCopyOfPEdgeList() {
        ArrayList<RandomEdge> copy = new ArrayList<RandomEdge>();

        for (RandomEdge edge : mPEdgeList) {
            copy.add(edge.getCopy());
        }

        return copy;
    }

    public final void addIEdge(InterruptEdge value) {
        mIEdgeList.add(value);
    }

    public final InterruptEdge getIEdgeAt(int index) {
        return mIEdgeList.get(index);
    }

    public final void removeIEdge(InterruptEdge value) {
        mIEdgeList.remove(value);
    }

    public final void removeAllIEdges() {
        mIEdgeList = new ArrayList<InterruptEdge>();
    }

    public final int getSizeOfIEdgeList() {
        return mIEdgeList.size();
    }

    public final ArrayList<InterruptEdge> getIEdgeList() {
        return mIEdgeList;
    }

    public final ArrayList<InterruptEdge> getCopyOfIEdgeList() {
        ArrayList<InterruptEdge> copy = new ArrayList<InterruptEdge>();

        for (InterruptEdge edge : mIEdgeList) {
            copy.add(edge.getCopy());
        }

        return copy;
    }

    public final ArrayList<AbstractEdge> getEdgeList() {
        ArrayList<AbstractEdge> edgeList = new ArrayList<AbstractEdge>();

        for (GuardedEdge edge : mCEdgeList) {
            edgeList.add(edge);
        }

        for (InterruptEdge edge : mIEdgeList) {
            edgeList.add(edge);
        }

        for (RandomEdge edge : mPEdgeList) {
            edgeList.add(edge);
        }

        for (ForkingEdge edge : mFEdgeList) {
            edgeList.add(edge);
        }

        if (mDEdge != null) {
            edgeList.add(mDEdge);
        }

        return edgeList;
    }

    protected void establishTargetNodes() {
        for (AbstractEdge edge : getEdgeList()) {
            edge.setTargetNode(mParentNode.getChildNodeById(edge.getTarget()));
        }
    }

    public final ArrayList<BasicNode> getReachableNodeList() {
        ArrayList<BasicNode> reachableNodeList = new ArrayList<BasicNode>();

        reachableNodeList.add(this);
        fillReachableNodeList(reachableNodeList);

        return reachableNodeList;
    }

    private final void fillReachableNodeList(ArrayList<BasicNode> fromSourceReachableNodeList) {
        for (AbstractEdge edge : getEdgeList()) {
            BasicNode targetNode = edge.getTargetNode();

            if (!fromSourceReachableNodeList.contains(targetNode)) {
                fromSourceReachableNodeList.add(targetNode);
                targetNode.fillReachableNodeList(fromSourceReachableNodeList);
            }
        }
    }
    /*
     @Override
     public final String getAbstractSyntax() {
     return mName;
     }

     @Override
     public final String getConcreteSyntax() {
     return mName;
     }

     @Override
     public final String getFormattedSyntax() {
     return mName;
     }
     */

    @Override
    public BasicNode getCopy() {
        return (BasicNode) CopyTool.copy(this);
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<Node id=\"" + mId + "\" name=\"" + mName + "\" history=\"" + mIsHistoryNode + "\">").push();

        int i = 0;

//        out.println("<Define>").push();
//
//        for (i = 0; i < mTypeDefList.size(); i++) {
//            mTypeDefList.get(i).writeXML(out);
//        }
//
//        out.pop().println("</Define>");
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

        out.pop().println("</Node>");
    }

    @Override
    public void parseXML(Element element) throws XMLParseError {
        mId = element.getAttribute("id");
        mName = element.getAttribute("name");
        mIsHistoryNode = Boolean.valueOf(element.getAttribute("history"));

        final BasicNode node = this;

        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(Element element) throws XMLParseError {
                String tag = element.getTagName();

//                if (tag.equals("Define")) {
//                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
//                        public void run(Element element) throws XMLParseError {
//                            mTypeDefList.add(TypeDef.parse(element));
//                        }
//                    });
//                } else 
                if (tag.equals("Declare")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        @Override
                        public void run(Element element) throws XMLParseError {
                            final VariableDefinition def = new VariableDefinition();
                            def.parseXML(element);
                            mVarDefList.add(def);
                        }
                    });
                } else if (tag.equals("Commands")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        @Override
                        public void run(Element element) throws XMLParseError {
                            mCmdList.add(Command.parse(element));
                        }
                    });
                } else if (tag.equals("Graphics")) {
                    mGraphics = new NodeGraphics();
                    mGraphics.parseXML(element);
                } else if (tag.equals("CEdge")) {
                    GuardedEdge edge = new GuardedEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(node);
                    edge.setSource(node.getId());
                    mCEdgeList.add(edge);
                } else if (tag.equals("PEdge")) {
                    RandomEdge edge = new RandomEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(node);
                    edge.setSource(node.getId());
                    mPEdgeList.add(edge);
                } else if (tag.equals("FEdge")) {
                    ForkingEdge edge = new ForkingEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(node);
                    edge.setSource(node.getId());
                    mFEdgeList.add(edge);
                } else if (tag.equals("IEdge")) {
                    InterruptEdge edge = new InterruptEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(node);
                    edge.setSource(node.getId());
                    mIEdgeList.add(edge);
                } else if (tag.equals("EEdge")) {
                    EpsilonEdge edge = new EpsilonEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(node);
                    edge.setSource(node.getId());
                    mDEdge = edge;
                } else if (tag.equals("TEdge")) {
                    TimeoutEdge edge = new TimeoutEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(node);
                    edge.setSource(node.getId());
                    mDEdge = edge;
                } else {
                    throw new XMLParseError(null,
                            "Cannot parse the element with the tag \"" + tag + "\" into a node child!");
                }
            }
        });
    }

    public int getHashCode() {

        // Add hash of General Attributes
        int hashCode = ((mName == null)
                ? 0
                : mName.hashCode()) + ((mComment == null)
                ? 0
                : mComment.hashCode()) + ((mGraphics == null)
                ? 0
                : mGraphics.getPosition().hashCode());

        // Add hash of all commands inside Node
        for (int cntCommand = 0; cntCommand < mCmdList.size(); cntCommand++) {
            hashCode += mCmdList.get(cntCommand).hashCode();
        }

//        // Add hash of all TypeDef inside Node
//        for (int cntType = 0; cntType < mTypeDefList.size(); cntType++) {
//            hashCode += mTypeDefList.get(cntType).hashCode() + mTypeDefList.get(cntType).getName().hashCode()
//                        + mTypeDefList.get(cntType).toString().hashCode();
//        }
        // Add hash of all Vars inside Node
        for (int cntVar = 0; cntVar < mVarDefList.size(); cntVar++) {
            hashCode += ((mVarDefList.get(cntVar).getName() == null)
                    ? 0
                    : mVarDefList.get(cntVar).getName().hashCode()) + ((mVarDefList.get(cntVar).getType() == null)
                    ? 0
                    : mVarDefList.get(cntVar).getType().hashCode()) + ((mVarDefList.get(cntVar).toString() == null)
                    ? 0
                    : mVarDefList.get(cntVar).toString().hashCode());
        }

        // Epsilon and Time Edges
        for (int cntEdge = 0; cntEdge < getEdgeList().size(); cntEdge++) {
            hashCode += getEdgeList().get(cntEdge).hashCode()
                    + getEdgeList().get(cntEdge).getGraphics().getHashCode();

            // TODO: find a way to parse the TEDGE mDEGE to take timeout into accout
        }
        // Add hash of all Conditional Edges
        for (int cntEdge = 0; cntEdge < getSizeOfCEdgeList(); cntEdge++) {
            hashCode += mCEdgeList.get(cntEdge).hashCode()
                    + mCEdgeList.get(cntEdge).getGraphics().getHashCode()
                    + mCEdgeList.get(cntEdge).getGuard().hashCode()
                    + mCEdgeList.get(cntEdge).getSource().hashCode()
                    + mCEdgeList.get(cntEdge).getTarget().hashCode();
        }
        // Add hash of all Probability Edges
        for (int cntEdge = 0; cntEdge < getSizeOfPEdgeList(); cntEdge++) {
            hashCode += mPEdgeList.get(cntEdge).hashCode()
                    + mPEdgeList.get(cntEdge).getGraphics().getHashCode()
                    + mPEdgeList.get(cntEdge).getProbability()
                    + mPEdgeList.get(cntEdge).getSource().hashCode()
                    + mPEdgeList.get(cntEdge).getTarget().hashCode();
        }
        // Add hash of all Fork Edges
        for (int cntEdge = 0; cntEdge < mFEdgeList.size(); cntEdge++) {
            hashCode += mFEdgeList.get(cntEdge).hashCode()
                    + mFEdgeList.get(cntEdge).getGraphics().getHashCode()
                    + mFEdgeList.get(cntEdge).getSource().hashCode()
                    + mFEdgeList.get(cntEdge).getTarget().hashCode();
        }
        return hashCode;
    }
}
