package de.dfki.vsm.model.sceneflow;

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.model.sceneflow.definition.VarDef;
import de.dfki.vsm.model.sceneflow.definition.type.TypeDef;
import de.dfki.vsm.model.sceneflow.graphics.node.NodeGraphics;
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
public class BasicNode implements ModelObject {

    protected String mId = new String();
    protected String mName = new String();
    protected String mComment = new String();
    //
    protected ArrayList<TypeDef> mTypeDefList = new ArrayList();
    protected ArrayList<VarDef> mVarDefList = new ArrayList();
    protected ArrayList<Command> mCmdList = new ArrayList();
    protected ArrayList<CEdge> mCEdgeList = new ArrayList();
    protected ArrayList<PEdge> mPEdgeList = new ArrayList();
    protected ArrayList<IEdge> mIEdgeList = new ArrayList();
    protected ArrayList<FEdge> mFEdgeList = new ArrayList();
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

    //
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

//    public void setExhaustive(boolean value) {
//        mExhaustive = value;
//    }
//
//    public boolean getExhaustive() {
//        return mExhaustive;
//    }
//
//    public void setPreserving(boolean value) {
//        mPreserving = value;
//    }
//
//    public boolean getPreserving() {
//        return mPreserving;
//    }
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

    public PEdge getFirstPEdge() {
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
            return (mDEdge instanceof TEdge)
                    ? FLAVOUR.TNODE
                    : FLAVOUR.ENODE;
        }

        return FLAVOUR.NONE;
    }

    public void setDedge(AbstractEdge value) {
        mDEdge = value;
    }

    public AbstractEdge getDedge() {
        return mDEdge;
    }

    public void removeDEdge() {
        mDEdge = null;
    }

    public void setGraphics(NodeGraphics value) {
        mGraphics = value;
    }

    public NodeGraphics getGraphics() {
        return mGraphics;
    }

    public void setParentNode(SuperNode value) {
        mParentNode = value;
    }

    public SuperNode getParentNode() {
        return mParentNode;
    }

    public void addVarDef(VarDef value) {
        mVarDefList.add(value);
    }

    public VarDef getVarDefAt(int index) {
        return mVarDefList.get(index);
    }

    public void setVarDefAt(VarDef varDef, int index) {
        mVarDefList.set(index, varDef);
    }

    public void removeVarDefAt(int index) {
        mVarDefList.remove(index);
    }

    public void setVarDefList(ArrayList<VarDef> value) {
        mVarDefList = value;
    }

    public ArrayList<VarDef> getVarDefList() {
        return mVarDefList;
    }

    public ArrayList<VarDef> getCopyOfVarDefList() {
        ArrayList<VarDef> copy = new ArrayList<VarDef>();
        for (VarDef varDef : mVarDefList) {
            copy.add(varDef.getCopy());
        }
        return copy;
    }

    public void addCmd(Command value) {
        mCmdList.add(value);
    }

    public Command getCmdAt(int index) {
        return mCmdList.get(index);
    }

    public void setCmdAt(Command cmd, int index) {
        mCmdList.set(index, cmd);
    }

    public void removeCmdAt(int index) {
        mCmdList.remove(index);
    }

    public void setCmdList(ArrayList<Command> value) {
        mCmdList = value;
    }

    public ArrayList<Command> getCmdList() {
        return mCmdList;
    }

    public int getSizeOfCmdList() {
        return mCmdList.size();
    }

    public ArrayList<Command> getCopyOfCmdList() {
        ArrayList<Command> copy = new ArrayList<Command>();

        for (Command cmd : mCmdList) {
            copy.add(cmd.getCopy());
        }

        return copy;
    }

    public void addTypeDef(TypeDef value) {
        mTypeDefList.add(value);
    }

    public TypeDef getTypeDefAt(int index) {
        return mTypeDefList.get(index);
    }

    public void setTypeDefAt(TypeDef typeDef, int index) {
        mTypeDefList.set(index, typeDef);
    }

    public void removeTypeDefAt(int index) {
        mTypeDefList.remove(index);
    }

    public ArrayList<TypeDef> getTypeDefList() {
        return mTypeDefList;
    }

    public void setTypeDefList(ArrayList<TypeDef> value) {
        mTypeDefList = value;
    }

    public int getSizeOfTypeDefList() {
        return mTypeDefList.size();
    }

    public ArrayList<TypeDef> getCopyOfTypeDefList() {
        ArrayList<TypeDef> copy = new ArrayList<TypeDef>();

        for (TypeDef def : mTypeDefList) {
            copy.add(def.getCopy());
        }

        return copy;
    }

    public void addCEdge(CEdge value) {
        mCEdgeList.add(value);
    }

    public CEdge getCEdgeAt(int index) {
        return mCEdgeList.get(index);
    }

    public void removeCEdge(CEdge value) {
        mCEdgeList.remove(value);
    }

    public void removeAllCEdges() {
        mCEdgeList = new ArrayList<CEdge>();
    }

    public int getSizeOfCEdgeList() {
        return mCEdgeList.size();
    }

    public ArrayList<CEdge> getCEdgeList() {
        return mCEdgeList;
    }

    public ArrayList<CEdge> getCopyOfCEdgeList() {
        ArrayList<CEdge> copy = new ArrayList<CEdge>();

        for (CEdge edge : mCEdgeList) {
            copy.add(edge.getCopy());
        }

        return copy;
    }

    public void addFEdge(FEdge value) {
        mFEdgeList.add(value);
    }

    public void removeFEdge(FEdge value) {
        mFEdgeList.remove(value);
    }

    public void removeAllFEdges() {
        mFEdgeList = new ArrayList<FEdge>();
    }

    public ArrayList<FEdge> getFEdgeList() {
        return mFEdgeList;
    }

    public void addPEdge(PEdge value) {
        mPEdgeList.add(value);
    }

    public PEdge getPEdgeAt(int index) {
        return mPEdgeList.get(index);
    }

    public void removePEdge(PEdge value) {
        mPEdgeList.remove(value);
    }

    public void removeAllPEdges() {
        mPEdgeList = new ArrayList<PEdge>();
    }

    public int getSizeOfPEdgeList() {
        return mPEdgeList.size();
    }

    public ArrayList<PEdge> getPEdgeList() {
        return mPEdgeList;
    }

    public ArrayList<PEdge> getCopyOfPEdgeList() {
        ArrayList<PEdge> copy = new ArrayList<PEdge>();

        for (PEdge edge : mPEdgeList) {
            copy.add(edge.getCopy());
        }

        return copy;
    }

    public void addIEdge(IEdge value) {
        mIEdgeList.add(value);
    }

    public IEdge getIEdgeAt(int index) {
        return mIEdgeList.get(index);
    }

    public void removeIEdge(IEdge value) {
        mIEdgeList.remove(value);
    }

    public void removeAllIEdges() {
        mIEdgeList = new ArrayList<IEdge>();
    }

    public int getSizeOfIEdgeList() {
        return mIEdgeList.size();
    }

    public ArrayList<IEdge> getIEdgeList() {
        return mIEdgeList;
    }

    public ArrayList<IEdge> getCopyOfIEdgeList() {
        ArrayList<IEdge> copy = new ArrayList<IEdge>();

        for (IEdge edge : mIEdgeList) {
            copy.add(edge.getCopy());
        }

        return copy;
    }

    public ArrayList<AbstractEdge> getEdgeList() {
        ArrayList<AbstractEdge> edgeList = new ArrayList<AbstractEdge>();

        for (CEdge edge : mCEdgeList) {
            edgeList.add(edge);
        }

        for (IEdge edge : mIEdgeList) {
            edgeList.add(edge);
        }

        for (PEdge edge : mPEdgeList) {
            edgeList.add(edge);
        }

        for (FEdge edge : mFEdgeList) {
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

    public ArrayList<BasicNode> getReachableNodeList() {
        ArrayList<BasicNode> reachableNodeList = new ArrayList<BasicNode>();

        reachableNodeList.add(this);
        fillReachableNodeList(reachableNodeList);

        return reachableNodeList;
    }

    private void fillReachableNodeList(ArrayList<BasicNode> fromSourceReachableNodeList) {
        for (AbstractEdge edge : getEdgeList()) {
            BasicNode targetNode = edge.getTargetNode();

            if (!fromSourceReachableNodeList.contains(targetNode)) {
                fromSourceReachableNodeList.add(targetNode);
                targetNode.fillReachableNodeList(fromSourceReachableNodeList);
            }
        }
    }

    public String getAbstractSyntax() {
        return null;
    }

    public String getConcreteSyntax() {
        return this.mName;
    }

    public String getFormattedSyntax() {
        return null;
    }

    public BasicNode getCopy() {
        return (BasicNode) CopyTool.copy(this);
    }

    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<Node id=\"" + mId + "\" name=\"" + mName + "\" history=\"" + mIsHistoryNode + "\">").push();

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

        out.pop().println("</Node>");
    }

    public void parseXML(Element element) throws XMLParseError {
        mId = element.getAttribute("id");
        mName = element.getAttribute("name");
        mIsHistoryNode = Boolean.valueOf(element.getAttribute("history"));

        final BasicNode node = this;

        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                String tag = element.getTagName();

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
                } else if (tag.equals("Graphics")) {
                    mGraphics = new NodeGraphics();
                    mGraphics.parseXML(element);
                } else if (tag.equals("CEdge")) {
                    CEdge edge = new CEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(node);
                    edge.setSource(node.getId());
                    mCEdgeList.add(edge);
                } else if (tag.equals("PEdge")) {
                    PEdge edge = new PEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(node);
                    edge.setSource(node.getId());
                    mPEdgeList.add(edge);
                } else if (tag.equals("FEdge")) {
                    FEdge edge = new FEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(node);
                    edge.setSource(node.getId());
                    mFEdgeList.add(edge);
                } else if (tag.equals("IEdge")) {
                    IEdge edge = new IEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(node);
                    edge.setSource(node.getId());
                    mIEdgeList.add(edge);
                } else if (tag.equals("EEdge")) {
                    EEdge edge = new EEdge();

                    edge.parseXML(element);
                    edge.setSourceNode(node);
                    edge.setSource(node.getId());
                    mDEdge = edge;
                } else if (tag.equals("TEdge")) {
                    TEdge edge = new TEdge();

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

        // Add hash of all commands inside BasicNode
        for (int cntCommand = 0; cntCommand < mCmdList.size(); cntCommand++) {
            hashCode += mCmdList.get(cntCommand).hashCode();
        }

        // Add hash of all TypeDef inside BasicNode
        for (int cntType = 0; cntType < mTypeDefList.size(); cntType++) {
            hashCode += mTypeDefList.get(cntType).hashCode() + mTypeDefList.get(cntType).getName().hashCode()
                    + mTypeDefList.get(cntType).toString().hashCode();
        }

        // Add hash of all Vars inside BasicNode
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

        return hashCode;
    }
}
