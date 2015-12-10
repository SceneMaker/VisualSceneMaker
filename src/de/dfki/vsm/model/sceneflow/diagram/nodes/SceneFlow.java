package de.dfki.vsm.model.sceneflow.diagram.nodes;

import de.dfki.vsm.model.sceneflow.diagram.boards.VariableBoard;
import de.dfki.vsm.model.sceneflow.diagram.boards.CommentBoard;
import de.dfki.vsm.Preferences;
import de.dfki.vsm.model.sceneflow.command.AbstractCommand;
import de.dfki.vsm.model.sceneflow.definition.FunctionDefinition;
import de.dfki.vsm.model.sceneflow.definition.VariableDefinition;
import de.dfki.vsm.util.cpy.CopyTool;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * @author Gregor Mehlmann
 */
public final class SceneFlow extends SuperNode {

    private HashMap<String, FunctionDefinition> FunctionDefinitions = new HashMap();

    public SceneFlow() {
    }

    public HashMap<String, FunctionDefinition> getUsrCmdDefMap() {
        return FunctionDefinitions;
    }

    public void setUsrCmdDefMap(HashMap<String, FunctionDefinition> value) {
        FunctionDefinitions = value;
    }

    public void putUsrCmdDef(String key, FunctionDefinition value) {
        FunctionDefinitions.put(key, value);
    }

    public FunctionDefinition getUsrCmdDef(String key) {
        return FunctionDefinitions.get(key);
    }

    public FunctionDefinition removeUsrCmdDef(String key) {
        return FunctionDefinitions.remove(key);
    }

    // TODO:
    public HashMap<String, FunctionDefinition> getCopyOfUserCmdDefMap() {
        HashMap<String, FunctionDefinition> copy = new HashMap<String, FunctionDefinition>();
        Iterator it = FunctionDefinitions.entrySet().iterator();

        while (it.hasNext()) {
            Map.Entry pairs = (Map.Entry) it.next();
            String userCommandName = (String) pairs.getKey();
            FunctionDefinition userCommand = (FunctionDefinition) pairs.getValue();
            FunctionDefinition userCommandCopy = userCommand.getCopy();

            copy.put(userCommandCopy.getName(), userCommandCopy);
        }

        return copy;
    }

    public FunctionDefinition getUserCommandDefinitionAt(String key) {
        return FunctionDefinitions.get(key);
    }

    public void setUserCommandDefinitionAt(String key, FunctionDefinition value) {
        FunctionDefinitions.put(key, value);
    }

    @Override
    public SceneFlow getCopy() {
        return (SceneFlow) CopyTool.copy(this);
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        String start = "";
        for (String id : mStartNodeList.keySet()) {
            start += id + ";";
        }

        out.println("<SceneFlow "
                + "id=\"" + mId + "\" "
                + "name=\"" + mName + "\" "
                + "comment=\"" + mComment + "\" "
                + "hideLocalVar=\"" + mHideLocalVarBadge + "\" "
                + "hideGlobalVar=\"" + mHideGlobalVarBadge + "\" "
                + "modifDate=\"" + Preferences.sDATE_FORMAT.format(new Date()) + "\" "
                + "start=\"" + start + "\">").push();

        int i = 0;

        out.println("<Define>").push();

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

        for (i = 0; i < mPEdgeList.size(); i++) {
            mPEdgeList.get(i).writeXML(out);
        }

        for (i = 0; i < mIEdgeList.size(); i++) {
            mIEdgeList.get(i).writeXML(out);
        }

        if (mDEdge != null) {
            mDEdge.writeXML(out);
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

        for (i = 0; i < mBasicNodeList.size(); i++) {
            mBasicNodeList.get(i).writeXML(out);
        }

        for (i = 0; i < mSuperNodeList.size(); i++) {
            mSuperNodeList.get(i).writeXML(out);
        }

        if (!FunctionDefinitions.isEmpty()) {
            out.println("<UserCommands>").push();

            for (FunctionDefinition definition : FunctionDefinitions.values()) {
                definition.writeXML(out);
            }
            //Iterator it = mUserCmdDefMap.entrySet().iterator();

            //while (it.hasNext()) {
            //    Map.Entry pairs = (java.util.Map.Entry) it.next();
            //    FunctionDefinition def = (FunctionDefinition) pairs.getValue();
            //    
            //}
            out.pop().println("</UserCommands>");
        }
        out.pop().print("</SceneFlow>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mId = element.getAttribute("id");
        mName = element.getAttribute("name");
        mComment = element.getAttribute("comment");
        String start = element.getAttribute("start");

        mHideLocalVarBadge = Boolean.valueOf(element.getAttribute("hideLocalVar"));
        mHideGlobalVarBadge = Boolean.valueOf(element.getAttribute("hideGlobalVar"));

        String[] arr = start.split(";");
        for (String str : arr) {
            if (!str.isEmpty()) {
                mStartNodeList.put(str, null);
            }
        }

        final SceneFlow sceneFlow = this;

        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(final Element element) throws XMLParseError {
                final String tag = element.getTagName();

                if (tag.equals("Declare")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        public void run(final Element element) throws XMLParseError {
                            final VariableDefinition def = new VariableDefinition();
                            def.parseXML(element);
                            mVarDefList.add(def);
                        }
                    });
                } else if (tag.equals("LocalVariableBadge")) {
                    VariableBoard varBadge = new VariableBoard("LocalVariableBadge");
                    varBadge.parseXML(element);
                    mLocalVariableBadge = varBadge;
                } else if (tag.equals("GlobalVariableBadge")) {
                    VariableBoard varBadge = new VariableBoard("GlobalVariableBadge");
                    varBadge.parseXML(element);
                    mGlobalVariableBadge = varBadge;
                } else if (tag.equals("Comment")) {
                    CommentBoard comment = new CommentBoard();
                    comment.parseXML(element);
                    comment.setParentNode(sceneFlow);
                    mCommentList.add(comment);
                } else if (tag.equals("Node")) {
                    BasicNode node = new BasicNode();
                    node.parseXML(element);
                    node.setParentNode(sceneFlow);
                    mBasicNodeList.add(node);
                } else if (tag.equals("SuperNode")) {
                    SuperNode node = new SuperNode();
                    node.parseXML(element);
                    node.setParentNode(sceneFlow);
                    mSuperNodeList.add(node);
                } else if (tag.equals("Commands")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        @Override
                        public void run(final Element element) throws XMLParseError {
                            mCmdList.add(AbstractCommand.parse(element));
                        }
                    });
                } else if (tag.equals("UserCommands")) {
                    XMLParseAction.processChildNodes(element, "UserCommand", new XMLParseAction() {
                        @Override
                        public void run(final Element element) throws XMLParseError {
                            final FunctionDefinition def = new FunctionDefinition();
                            def.parseXML(element);
                            FunctionDefinitions.put(def.getName(), def);
                        }
                    });
                } else {
                    throw new XMLParseError(null, null);
                }
            }
        });
    }

    @Override
    public int getHashCode() {

        // Add hash of General Attributes
        int hashCode = ((mName == null)
                ? 0
                : mName.hashCode()) + ((mComment == null)
                ? 0
                : mComment.hashCode()) + ((mGraphics == null)
                ? 0
                : mGraphics.hashCode()) + ((mParentNode == null)
                ? 0
                : mParentNode.hashCode()) + ((mHistoryNode == null)
                ? 0
                : mHistoryNode.hashCode()) + ((mStartNodeList == null)
                ? 0
                : mStartNodeList.hashCode()) + ((mIsHistoryNode == true)
                ? 1
                : 0) + ((mLocalVariableBadge == null)
                ? 0
                : mLocalVariableBadge.hashCode()) + ((mGlobalVariableBadge == null)
                ? 0
                : mGlobalVariableBadge.hashCode()) + ((mHideLocalVarBadge == true)
                ? 1
                : 0) + ((mHideGlobalVarBadge == true)
                ? 1
                : 0);

        // Add hash of existing user commands
        for (FunctionDefinition fundDef : FunctionDefinitions.values()) {
            hashCode += fundDef.getName().hashCode() + fundDef.getClazz().hashCode()
                    + fundDef.getMethod().hashCode() + fundDef.getArgList().hashCode();
        }

        // Add hash of all nodes on workspace
        for (int cntNode = 0; cntNode < mBasicNodeList.size(); cntNode++) {
            hashCode += getNodeAt(cntNode).getHashCode();
        }

        // Add hash of all superNodes on workspace
        for (int cntSNode = 0; cntSNode < mSuperNodeList.size(); cntSNode++) {
            hashCode += getSuperNodeAt(cntSNode).getHashCode();
        }

        // Add hash of all commands on workspace
        for (int cntCommand = 0; cntCommand < getSizeOfCmdList(); cntCommand++) {
            hashCode += mCmdList.get(cntCommand).hashCode();
        }

//        // Add hash of all TypeDef on workspace
//        for (int cntType = 0; cntType < getSizeOfTypeDefList(); cntType++) {
//            hashCode += mTypeDefList.get(cntType).hashCode() + mTypeDefList.get(cntType).getName().hashCode()
//                    + mTypeDefList.get(cntType).toString().hashCode();
//        }
        // Add hash of VarDef on workspace
        for (int cntVar = 0; cntVar < getVarDefList().size(); cntVar++) {
            hashCode += getVarDefList().get(cntVar).getName().hashCode()
                    + getVarDefList().get(cntVar).getType().hashCode()
                    + getVarDefList().get(cntVar).toString().hashCode();
        }

        // Add hash of all comments on workspace
        for (int cntComment = 0; cntComment < getCommentList().size(); cntComment++) {
            hashCode += mCommentList.get(cntComment).getGraphics().getRect().hashCode();
            //hashCode += mCommentList.get(cntComment).getHTMLText().hashCode();
        }

        return hashCode;
    }
}
