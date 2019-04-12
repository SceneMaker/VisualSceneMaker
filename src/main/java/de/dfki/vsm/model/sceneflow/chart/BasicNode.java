package de.dfki.vsm.model.sceneflow.chart;

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.model.sceneflow.chart.edge.*;
import de.dfki.vsm.model.sceneflow.chart.graphics.node.NodeGraphics;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.model.sceneflow.glue.command.definition.DataTypeDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.util.cpy.CopyTool;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;

import java.util.ArrayList;

/**
 * @author Gregor Mehlmann
 */
public class BasicNode implements ModelObject {

    public Byte hasOne = Byte.valueOf("1");
    public Byte hasMany = Byte.valueOf("2");
    protected ArrayList<Command> mCmdList = new ArrayList<>();
    String mNodeId = "";
    String mNodeName = "";
    String mComment = "";
    //
    ArrayList<DataTypeDefinition> mTypeDefList = new ArrayList<>();
    ArrayList<VariableDefinition> mVarDefList = new ArrayList<>();
    ArrayList<GuargedEdge> mCEdgeList = new ArrayList<>();
    ArrayList<RandomEdge> mPEdgeList = new ArrayList<>();
    ArrayList<InterruptEdge> mIEdgeList = new ArrayList<>();
    protected NodeGraphics mGraphics = null;
    ArrayList<ForkingEdge> mFEdgeList = new ArrayList<>();
    //
    AbstractEdge mDEdge = null;
    SuperNode mParentNode = null;
    boolean mIsHistoryNode = false;
    private Byte hasNone = Byte.valueOf("0");

    public enum FLAVOUR {

        NONE, ENODE, TNODE, CNODE, PNODE, INODE, FNODE
    }

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
        mNodeId = value;
    }

    public String getId() {
        return mNodeId;
    }

    public boolean isHistoryNode() {
        return mIsHistoryNode;
    }

    public void setHistoryNodeFlag(boolean value) {
        mIsHistoryNode = value;
    }

    public void setName(String value) {
        mNodeName = value;
    }

    public String getName() {
        return mNodeName;
    }

    public void setNameAndId(String value) {
        mNodeId = value;
        mNodeName = value;
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

        return mComment.length() != 0;
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
            return mIEdgeList.size() > 0;
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

    public void addVarDef(VariableDefinition value) {
        mVarDefList.add(value);
    }

    public VariableDefinition getVarDefAt(int index) {
        return mVarDefList.get(index);
    }

    public void setVarDefAt(VariableDefinition varDef, int index) {
        mVarDefList.set(index, varDef);
    }

    public void removeVarDefAt(int index) {
        mVarDefList.remove(index);
    }

    public void setVarDefList(ArrayList<VariableDefinition> value) {
        mVarDefList = value;
    }

    public ArrayList<VariableDefinition> getVarDefList() {
        return mVarDefList;
    }

    public ArrayList<VariableDefinition> getCopyOfVarDefList() {
        ArrayList<VariableDefinition> copy = new ArrayList<>();
        for (VariableDefinition varDef : mVarDefList) {
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
        ArrayList<Command> copy = new ArrayList<>();

        for (Command cmd : mCmdList) {
            copy.add(cmd.getCopy());
        }

        return copy;
    }

    public void addTypeDef(DataTypeDefinition value) {
        mTypeDefList.add(value);
    }

    public DataTypeDefinition getTypeDefAt(int index) {
        return mTypeDefList.get(index);
    }

    public void setTypeDefAt(DataTypeDefinition typeDef, int index) {
        mTypeDefList.set(index, typeDef);
    }

    public void removeTypeDefAt(int index) {
        mTypeDefList.remove(index);
    }

    public ArrayList<DataTypeDefinition> getTypeDefList() {
        return mTypeDefList;
    }

    public void setTypeDefList(ArrayList<DataTypeDefinition> value) {
        mTypeDefList = value;
    }

    public int getSizeOfTypeDefList() {
        return mTypeDefList.size();
    }

    public ArrayList<DataTypeDefinition> getCopyOfTypeDefList() {
        final ArrayList<DataTypeDefinition> copy = new ArrayList<>();
        for (final DataTypeDefinition def : mTypeDefList) {
            copy.add(def.getCopy());
        }

        return copy;
    }

    public void addCEdge(GuargedEdge value) {
        mCEdgeList.add(value);
    }

    public GuargedEdge getCEdgeAt(int index) {
        return mCEdgeList.get(index);
    }

    public void removeCEdge(GuargedEdge value) {
        mCEdgeList.remove(value);
    }

    public void removeAllCEdges() {
        mCEdgeList = new ArrayList<>();
    }

    public int getSizeOfCEdgeList() {
        return mCEdgeList.size();
    }

    public ArrayList<GuargedEdge> getCEdgeList() {
        return mCEdgeList;
    }

    public ArrayList<GuargedEdge> getCopyOfCEdgeList() {
        ArrayList<GuargedEdge> copy = new ArrayList<>();

        for (GuargedEdge edge : mCEdgeList) {
            copy.add(edge.getCopy());
        }

        return copy;
    }

    public void addFEdge(ForkingEdge value) {
        mFEdgeList.add(value);
    }

    public void removeFEdge(ForkingEdge value) {
        mFEdgeList.remove(value);
    }

    public void removeAllFEdges() {
        mFEdgeList = new ArrayList<>();
    }

    public ArrayList<ForkingEdge> getFEdgeList() {
        return mFEdgeList;
    }

    public void addPEdge(RandomEdge value) {
        mPEdgeList.add(value);
    }

    public RandomEdge getPEdgeAt(int index) {
        return mPEdgeList.get(index);
    }

    public void removePEdge(RandomEdge value) {
        mPEdgeList.remove(value);
    }

    public void removeAllPEdges() {
        mPEdgeList = new ArrayList<>();
    }

    public int getSizeOfPEdgeList() {
        return mPEdgeList.size();
    }

    public ArrayList<RandomEdge> getPEdgeList() {
        return mPEdgeList;
    }

    public ArrayList<RandomEdge> getCopyOfPEdgeList() {
        ArrayList<RandomEdge> copy = new ArrayList<>();

        for (RandomEdge edge : mPEdgeList) {
            copy.add(edge.getCopy());
        }

        return copy;
    }

    public void addIEdge(InterruptEdge value) {
        mIEdgeList.add(value);
    }

    public InterruptEdge getIEdgeAt(int index) {
        return mIEdgeList.get(index);
    }

    public void removeIEdge(InterruptEdge value) {
        mIEdgeList.remove(value);
    }

    public void removeAllIEdges() {
        mIEdgeList = new ArrayList<>();
    }

    public int getSizeOfIEdgeList() {
        return mIEdgeList.size();
    }

    public ArrayList<InterruptEdge> getIEdgeList() {
        return mIEdgeList;
    }

    public ArrayList<InterruptEdge> getCopyOfIEdgeList() {
        ArrayList<InterruptEdge> copy = new ArrayList<>();

        for (InterruptEdge edge : mIEdgeList) {
            copy.add(edge.getCopy());
        }

        return copy;
    }

    public ArrayList<AbstractEdge> getEdgeList() {
        ArrayList<AbstractEdge> edgeList = new ArrayList<>();

        edgeList.addAll(mCEdgeList);

        edgeList.addAll(mIEdgeList);

        edgeList.addAll(mPEdgeList);

        edgeList.addAll(mFEdgeList);

        if (mDEdge != null) {
            edgeList.add(mDEdge);
        }

        return edgeList;
    }

    protected void establishTargetNodes() {
        for (AbstractEdge edge : getEdgeList()) {
            edge.setTargetNode(mParentNode.getChildNodeById(edge.getTargetUnid()));
        }
    }

    public ArrayList<BasicNode> getReachableNodeList() {
        ArrayList<BasicNode> reachableNodeList = new ArrayList<>();

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
        return this.mNodeName;
    }

    public String getFormattedSyntax() {
        return null;
    }

    public BasicNode getCopy() {
        return (BasicNode) CopyTool.copy(this);
    }

    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<Node id=\"" + mNodeId + "\" name=\"" + mNodeName + "\" history=\"" + mIsHistoryNode + "\">").push();

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
        mNodeId = element.getAttribute("id");
        mNodeName = element.getAttribute("name");
        mIsHistoryNode = Boolean.valueOf(element.getAttribute("history"));

        final BasicNode node = this;

        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                String tag = element.getTagName();

                switch (tag) {
                    case "Define":
                        XMLParseAction.processChildNodes(element, new XMLParseAction() {
                            public void run(Element element) throws XMLParseError {
                                mTypeDefList.add(DataTypeDefinition.parse(element));
                            }
                        });
                        break;
                    case "Declare":
                        XMLParseAction.processChildNodes(element, new XMLParseAction() {
                            @Override
                            public void run(Element element) throws XMLParseError {
                                VariableDefinition def = new VariableDefinition();

                                def.parseXML(element);
                                mVarDefList.add(def);

                            }
                        });
                        break;
                    case "Commands":
                        XMLParseAction.processChildNodes(element, new XMLParseAction() {
                            @Override
                            public void run(Element element) throws XMLParseError {
                                mCmdList.add(Command.parse(element));
                            }
                        });
                        break;
                    case "Graphics":
                        mGraphics = new NodeGraphics();
                        mGraphics.parseXML(element);
                        break;
                    case "CEdge": {
                        GuargedEdge edge = new GuargedEdge();

                        edge.parseXML(element);
                        edge.setSourceNode(node);
                        edge.setSourceUnid(node.getId());
                        mCEdgeList.add(edge);
                        break;
                    }
                    case "PEdge": {
                        RandomEdge edge = new RandomEdge();

                        edge.parseXML(element);
                        edge.setSourceNode(node);
                        edge.setSourceUnid(node.getId());
                        mPEdgeList.add(edge);
                        break;
                    }
                    case "FEdge": {
                        ForkingEdge edge = new ForkingEdge();

                        edge.parseXML(element);
                        edge.setSourceNode(node);
                        edge.setSourceUnid(node.getId());
                        mFEdgeList.add(edge);
                        break;
                    }
                    case "IEdge": {
                        InterruptEdge edge = new InterruptEdge();

                        edge.parseXML(element);
                        edge.setSourceNode(node);
                        edge.setSourceUnid(node.getId());
                        mIEdgeList.add(edge);
                        break;
                    }
                    case "EEdge": {
                        EpsilonEdge edge = new EpsilonEdge();

                        edge.parseXML(element);
                        edge.setSourceNode(node);
                        edge.setSourceUnid(node.getId());
                        mDEdge = edge;
                        break;
                    }
                    case "TEdge": {
                        TimeoutEdge edge = new TimeoutEdge();

                        edge.parseXML(element);
                        edge.setSourceNode(node);
                        edge.setSourceUnid(node.getId());
                        mDEdge = edge;
                        break;
                    }
                    default:
                        throw new XMLParseError(null,
                                "Cannot parse the element with the tag \"" + tag + "\" into a node child!");
                }
            }
        });
    }

    public int getHashCode() {

        // Add hash of General Attributes
        int hashCode = ((mNodeName == null)
                ? 0
                : mNodeName.hashCode()) + ((mComment == null)
                ? 0
                : mComment.hashCode()) + ((mGraphics == null)
                ? 0
                : mGraphics.getPosition().hashCode());

        // Add hash of all commands inside BasicNode
        for (Command command : mCmdList) {
            hashCode += command.hashCode();
        }

        // Add hash of all TypeDef inside BasicNode
        for (DataTypeDefinition dataTypeDefinition : mTypeDefList) {
            hashCode += dataTypeDefinition.hashCode() + dataTypeDefinition.getName().hashCode()
                    + dataTypeDefinition.toString().hashCode();
        }

        // Add hash of all Vars inside BasicNode
        for (VariableDefinition variableDefinition : mVarDefList) {
            hashCode += ((variableDefinition.getName() == null)
                    ? 0
                    : variableDefinition.getName().hashCode()) + ((variableDefinition.getType() == null)
                    ? 0
                    : variableDefinition.getType().hashCode()) + ((variableDefinition.toString() == null)
                    ? 0
                    : variableDefinition.toString().hashCode());
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
        for (ForkingEdge forkingEdge : mFEdgeList) {
            hashCode += forkingEdge.hashCode() + forkingEdge.getGraphics().getHashCode()
                    + forkingEdge.getSourceUnid().hashCode() + forkingEdge.getTargetUnid().hashCode();
        }

        return hashCode;
    }
}
