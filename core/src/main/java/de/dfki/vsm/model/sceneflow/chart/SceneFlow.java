package de.dfki.vsm.model.sceneflow.chart;

import de.dfki.vsm.Preferences;
import de.dfki.vsm.model.sceneflow.chart.badge.CommentBadge;
import de.dfki.vsm.model.sceneflow.chart.badge.VariableBadge;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.model.sceneflow.glue.command.definition.ArgumentDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.DataTypeDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.FunctionDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.util.cpy.CopyTool;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Gregor Mehlmann
 */
public final class SceneFlow extends SuperNode {

    private String mXMLNameSpace = "";
    private String mXMLSchemeInstance = "";
    private String mXMLSchemeLocation = "";
    private String mPackageName = "";
    private String mContextClass = "";
    private String mContextCode = "";
    private ArrayList<String> mClassPathList = new ArrayList<>();
    private HashMap<String, FunctionDefinition> mUserCmdDefMap = new HashMap<>();
    private String mModifDate = "";

    public SceneFlow() {
    }

    public String getContextCode() {
        return mContextCode;
    }

    public void setContextCode(String initContext) {
        mContextCode = initContext;
    }

    public String getContextClass() {
        return mContextClass;
    }

    public void setContextClass(String value) {
        mContextClass = value;
    }

    public String getPackageName() {
        return mPackageName;
    }

    public void setPackageName(String value) {
        mPackageName = value;
    }

    public ArrayList<String> getClassPathList() {
        return mClassPathList;
    }

    public void setClassPathList(ArrayList<String> classPath) {
        mClassPathList = classPath;
    }

    public ArrayList<String> getCopyOfClassPathList() {
        return new ArrayList<>(mClassPathList);
    }

    public HashMap<String, FunctionDefinition> getUsrCmdDefMap() {
        return mUserCmdDefMap;
    }

    public void setUsrCmdDefMap(HashMap<String, FunctionDefinition> value) {
        mUserCmdDefMap = value;
    }

    public void putUsrCmdDef(String key, FunctionDefinition value) {
        mUserCmdDefMap.put(key, value);
    }

    public FunctionDefinition getUsrCmdDef(String key) {
        return mUserCmdDefMap.get(key);
    }

    public FunctionDefinition removeUsrCmdDef(String key) {
        return mUserCmdDefMap.remove(key);
    }

    // TODO:
    public HashMap<String, FunctionDefinition> getCopyOfUserCmdDefMap() {
        HashMap<String, FunctionDefinition> copy = new HashMap<>();

        for (Map.Entry<String, FunctionDefinition> stringFunctionDefinitionEntry : mUserCmdDefMap.entrySet()) {
            Map.Entry pairs = stringFunctionDefinitionEntry;
            String userCommandName = (String) pairs.getKey();
            FunctionDefinition userCommand = (FunctionDefinition) pairs.getValue();
            FunctionDefinition userCommandCopy = userCommand.getCopy();

            copy.put(userCommandCopy.getName(), userCommandCopy);
        }

        return copy;
    }

    public FunctionDefinition getUserCommandDefinitionAt(String key) {
        return mUserCmdDefMap.get(key);
    }

    public void setUserCommandDefinitionAt(String key, FunctionDefinition value) {
        mUserCmdDefMap.put(key, value);
    }

    @Override
    public SceneFlow getCopy() {
        return (SceneFlow) CopyTool.copy(this);
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        StringBuilder start = new StringBuilder();

        for (String id : mStartNodeMap.keySet()) {
            start.append(id).append(";");
        }

        //out.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
        out.println("<SceneFlow " 
                + "id=\"" + mNodeId + "\" " 
                + "name=\"" + mNodeName + "\" " 
                + "comment=\"" + mComment + "\" hideLocalVar=\"" + mHideLocalVarBadge + "\" hideGlobalVar=\"" + mHideGlobalVarBadge + "\" "
                + "modifDate=\"" + Preferences.sDATE_FORMAT.format(new Date()) + "\" " + "start=\""
                + start + "\" "
                // + "context=\""+(context.equals("") ? "java.lang.Object" : context)+"\" "
                + "context=\"" + mContextClass + "\" " + "package=\"" + mPackageName + "\" "
                // + "scenefile=\"" + mSceneFileName + "\" "
                // + "sceneinfo=\"" + mSceneInfoFileName + "\" "
                + "xmlns=\"" + Preferences.getProperty("xmlns") + "\" " + "xmlns:xsi=\"" + Preferences.getProperty("xmlns_xsi")
                + "\" " + "xsi:schemaLocation=\"" + Preferences.getProperty("xmlns") + " "
                + Preferences.getProperty("xsi_schemeLocation") + "\">").push();

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

        for (i = 0; i < mNodeList.size(); i++) {
            mNodeList.get(i).writeXML(out);
        }

        for (i = 0; i < mSuperNodeList.size(); i++) {
            mSuperNodeList.get(i).writeXML(out);
        }

        if (!mUserCmdDefMap.isEmpty()) {
            out.println("<UserCommands>").push();

            for (Map.Entry<String, FunctionDefinition> stringFunctionDefinitionEntry : mUserCmdDefMap.entrySet()) {
                Map.Entry pairs = stringFunctionDefinitionEntry;
                FunctionDefinition def = (FunctionDefinition) pairs.getValue();
                if (def.isActive()) {
                    def.writeXML(out);
                }
            }

            out.pop().println("</UserCommands>");
        }

        out.println("<ClassPath>").push();

        for (i = 0; i < mClassPathList.size(); i++) {
            out.println("<ClassPathElement>").push();
            out.println(mClassPathList.get(i));
            out.pop().println("</ClassPathElement>");
        }

        out.pop().println("</ClassPath>");
        out.print("<InitContext>");
        out.print(mContextCode);
        out.println("</InitContext>");
        out.pop().print("</SceneFlow>");
    }

    @Override
    public void parseXML(Element element) throws XMLParseError {
        mNodeId = element.getAttribute("id");
        mNodeName = element.getAttribute("name");
        mComment = element.getAttribute("comment");

        String start = element.getAttribute("start");

        mContextClass = element.getAttribute("context");
        mPackageName = element.getAttribute("package");
        mXMLSchemeLocation = element.getAttribute("xsi:schemaLocation");
        mXMLNameSpace = element.getAttribute("xmlns");
        mXMLSchemeInstance = element.getAttribute("xmlns:xsi");
        mHideLocalVarBadge = Boolean.valueOf(element.getAttribute("hideLocalVar"));
        mHideGlobalVarBadge = Boolean.valueOf(element.getAttribute("hideGlobalVar"));
        mModifDate = element.getAttribute("modifDate");

        /**
         * Construct start node list from the start string
         */
        String[] arr = start.split(";");

        for (String str : arr) {
            if (!str.isEmpty()  && !str.equals("null")) {
                mStartNodeMap.put(str, null);
            }
        }

        final SceneFlow sceneFlow = this;

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
                            public void run(Element element) throws XMLParseError {
                                VariableDefinition def = new VariableDefinition();

                                def.parseXML(element);
                                mVarDefList.add(def);
                            }
                        });
                        break;
                    case "LocalVariableBadge": {
                        VariableBadge varBadge = new VariableBadge("LocalVariableBadge");

                        varBadge.parseXML(element);
                        mLocalVariableBadge = varBadge;
                        break;
                    }
                    case "GlobalVariableBadge": {
                        VariableBadge varBadge = new VariableBadge("GlobalVariableBadge");

                        varBadge.parseXML(element);
                        mGlobalVariableBadge = varBadge;
                        break;
                    }
                    case "VariableBadge":

                        // do nothing (left for old project's compatibility)
                        break;
                    case "Comment":
                        CommentBadge comment = new CommentBadge();

                        comment.parseXML(element);
                        comment.setParentNode(sceneFlow);
                        mCommentList.add(comment);
                        break;
                    case "Node": {
                        BasicNode node = new BasicNode();

                        node.parseXML(element);
                        node.setParentNode(sceneFlow);
                        mNodeList.add(node);
                        break;
                    }
                    case "SuperNode": {
                        SuperNode node = new SuperNode();

                        node.parseXML(element);
                        node.setParentNode(sceneFlow);
                        mSuperNodeList.add(node);
                        break;
                    }
                    case "Commands":
                        XMLParseAction.processChildNodes(element, new XMLParseAction() {
                            public void run(Element element) throws XMLParseError {
                                mCmdList.add(Command.parse(element));
                            }
                        });
                        break;
                    case "UserCommands":
                        XMLParseAction.processChildNodes(element, "UserCommand", new XMLParseAction() {
                            public void run(Element element) throws XMLParseError {
                                FunctionDefinition def = new FunctionDefinition();

                                def.parseXML(element);
                                mUserCmdDefMap.put(def.getName(), def);
                            }
                        });
                        break;
                    case "ClassPath":
                        XMLParseAction.processChildNodes(element, "ClassPathElement", new XMLParseAction() {
                            public void run(Element element) throws XMLParseError {
                                mClassPathList.add(element.getTextContent().trim());
                            }
                        });
                        break;
                    case "InitContext":
                        mContextCode = element.getTextContent().trim();
                        break;
                    default:
                        throw new XMLParseError(null,
                                "Cannot parse the element with the tag \"" + tag
                                        + "\" into a sceneflow child!");
                }
            }
        });
    }

    @Override
    public int getHashCode() {

        // Add hash of General Attributes
        int hashCode;
        if (mNodeName == null) hashCode = ((mComment == null)
                ? 0
                : mComment.hashCode()) + ((mGraphics == null)
                ? 0
                : mGraphics.hashCode()) + ((mParentNode == null)
                ? 0
                : mParentNode.hashCode()) + ((mHistoryNode == null)
                ? 0
                : mHistoryNode.hashCode()) + ((mStartNodeMap == null)
                ? 0
                : mStartNodeMap.hashCode()) + ((mIsHistoryNode)
                ? 1
                : 0) + ((mLocalVariableBadge == null)
                ? 0
                : mLocalVariableBadge.hashCode()) + ((mGlobalVariableBadge == null)
                ? 0
                : mGlobalVariableBadge.hashCode()) + ((mHideLocalVarBadge)
                ? 1
                : 0) + ((mHideGlobalVarBadge)
                ? 1
                : 0);
        else hashCode = mNodeName.hashCode() + ((mComment == null)
                ? 0
                : mComment.hashCode()) + ((mGraphics == null)
                ? 0
                : mGraphics.hashCode()) + ((mParentNode == null)
                ? 0
                : mParentNode.hashCode()) + ((mHistoryNode == null)
                ? 0
                : mHistoryNode.hashCode()) + ((mStartNodeMap == null)
                ? 0
                : mStartNodeMap.hashCode()) + ((mIsHistoryNode)
                ? 1
                : 0) + ((mLocalVariableBadge == null)
                ? 0
                : mLocalVariableBadge.hashCode()) + ((mGlobalVariableBadge == null)
                ? 0
                : mGlobalVariableBadge.hashCode()) + ((mHideLocalVarBadge)
                ? 1
                : 0) + ((mHideGlobalVarBadge)
                ? 1
                : 0);


        // Add hash of existing user commands
        for (FunctionDefinition fundDef : mUserCmdDefMap.values()) {
            hashCode += fundDef.getName().hashCode() + fundDef.getClassName().hashCode()
                    + fundDef.getMethod().hashCode();
                    for(ArgumentDefinition var: fundDef.getParamList()){ //Otherwise the hascode was not unique
                        hashCode+= var.getName().hashCode();
                        hashCode+= var.getType().hashCode();
                    }
                    //+ fundDef.getParamList().hashCode();
        }
       
        // Add hash of all nodes on workspace
        for (int cntNode = 0; cntNode < mNodeList.size(); cntNode++) {
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
      
         // Add hash of all TypeDef on workspace
        for (int cntType = 0; cntType < getSizeOfTypeDefList(); cntType++) {
            hashCode += mTypeDefList.get(cntType).hashCode() + mTypeDefList.get(cntType).getName().hashCode()
                    + mTypeDefList.get(cntType).toString().hashCode();
        }

        // Add hash of VarDef on workspace
        for (int cntVar = 0; cntVar < getVarDefList().size(); cntVar++) {
            hashCode += getVarDefList().get(cntVar).getName().hashCode()
                    + getVarDefList().get(cntVar).getType().hashCode()
                    + getVarDefList().get(cntVar).toString().hashCode();
        }
        
        // Add hash of all comments on workspace
        for (int cntComment = 0; cntComment < getCommentList().size(); cntComment++) {          
            hashCode += mCommentList.get(cntComment).getGraphics().getRectangle().hashCode();
            hashCode += mCommentList.get(cntComment).getHTMLText().hashCode();
        }


        return hashCode;
    }
}
