package de.dfki.vsm.model.sceneflow;

import de.dfki.vsm.Preferences;
import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.model.sceneflow.definition.FunDef;
import de.dfki.vsm.model.sceneflow.definition.ParamDef;
import de.dfki.vsm.model.sceneflow.definition.VarDef;
import de.dfki.vsm.model.sceneflow.definition.type.TypeDef;
import de.dfki.vsm.util.cpy.CopyTool;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * @author Gregor Mehlmann
 */
public class SceneFlow extends SuperNode {

    protected String mXMLNameSpace = new String();
    protected String mXMLSchemeInstance = new String();
    protected String mXMLSchemeLocation = new String();
    protected String mPackageName = new String();
    protected String mContextClass = new String();
    protected String mContextCode = new String();
    protected ArrayList<String> mClassPathList = new ArrayList<String>();
    protected HashMap<String, FunDef> mUserCmdDefMap = new HashMap<String, FunDef>();
    protected String mModifDate = new String();

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
        ArrayList<String> copy = new ArrayList<String>();

        for (String str : mClassPathList) {
            copy.add(str);
        }

        return copy;
    }

    public HashMap<String, FunDef> getUsrCmdDefMap() {
        return mUserCmdDefMap;
    }

    public void setUsrCmdDefMap(HashMap<String, FunDef> value) {
        mUserCmdDefMap = value;
    }

    public void putUsrCmdDef(String key, FunDef value) {
        mUserCmdDefMap.put(key, value);
    }

    public FunDef getUsrCmdDef(String key) {
        return mUserCmdDefMap.get(key);
    }

    public FunDef removeUsrCmdDef(String key) {
        return mUserCmdDefMap.remove(key);
    }

    // TODO:
    public HashMap<String, FunDef> getCopyOfUserCmdDefMap() {
        HashMap<String, FunDef> copy = new HashMap<String, FunDef>();
        Iterator it = mUserCmdDefMap.entrySet().iterator();

        while (it.hasNext()) {
            Map.Entry pairs = (Map.Entry) it.next();
            String userCommandName = (String) pairs.getKey();
            FunDef userCommand = (FunDef) pairs.getValue();
            FunDef userCommandCopy = userCommand.getCopy();

            copy.put(userCommandCopy.getName(), userCommandCopy);
        }

        return copy;
    }

    public FunDef getUserCommandDefinitionAt(String key) {
        return mUserCmdDefMap.get(key);
    }

    public void setUserCommandDefinitionAt(String key, FunDef value) {
        mUserCmdDefMap.put(key, value);
    }

    @Override
    public SceneFlow getCopy() {
        return (SceneFlow) CopyTool.copy(this);
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        String start = "";

        for (String id : mStartNodeMap.keySet()) {
            start += id + ";";
        }

        //out.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
        out.println("<SceneFlow " 
                + "id=\"" + mId + "\" " 
                + "name=\"" + mName + "\" " 
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

            Iterator it = mUserCmdDefMap.entrySet().iterator();

            while (it.hasNext()) {
                Map.Entry pairs = (java.util.Map.Entry) it.next();
                FunDef    def   = (FunDef) pairs.getValue();
                if(def.isActive())
                {
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
        mId = element.getAttribute("id");
        mName = element.getAttribute("name");
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
                    comment.setParentNode(sceneFlow);
                    mCommentList.add(comment);
                } else if (tag.equals("Node")) {
                    BasicNode node = new BasicNode();

                    node.parseXML(element);
                    node.setParentNode(sceneFlow);
                    mNodeList.add(node);
                } else if (tag.equals("SuperNode")) {
                    SuperNode node = new SuperNode();

                    node.parseXML(element);
                    node.setParentNode(sceneFlow);
                    mSuperNodeList.add(node);
                } else if (tag.equals("Commands")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        public void run(Element element) throws XMLParseError {
                            mCmdList.add(Command.parse(element));
                        }
                    });
                } else if (tag.equals("UserCommands")) {
                    XMLParseAction.processChildNodes(element, "UserCommand", new XMLParseAction() {
                        public void run(Element element) throws XMLParseError {
                            FunDef def = new FunDef();

                            def.parseXML(element);
                            mUserCmdDefMap.put(def.getName(), def);
                        }
                    });
                } else if (tag.equals("ClassPath")) {
                    XMLParseAction.processChildNodes(element, "ClassPathElement", new XMLParseAction() {
                        public void run(Element element) throws XMLParseError {
                            mClassPathList.add(element.getTextContent().trim());
                        }
                    });
                } else if (tag.equals("InitContext")) {
                    mContextCode = element.getTextContent().trim();
                } else {
                    throw new XMLParseError(null,
                            "Cannot parse the element with the tag \"" + tag
                            + "\" into a sceneflow child!");
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
                : mGraphics.hashCode()) + ((mParentNode == null)
                ? 0
                : mParentNode.hashCode()) + ((mHistoryNode == null)
                ? 0
                : mHistoryNode.hashCode()) + ((mStartNodeMap == null)
                ? 0
                : mStartNodeMap.hashCode()) + ((mIsHistoryNode == true)
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
        for (FunDef fundDef : mUserCmdDefMap.values()) {
            hashCode += fundDef.getName().hashCode() + fundDef.getClassName().hashCode()
                    + fundDef.getMethod().hashCode();
                    for(ParamDef var: fundDef.getParamList()){ //Otherwise the hascode was not unique
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
            hashCode += mCommentList.get(cntComment).mGraphics.getRectangle().hashCode();
            hashCode += mCommentList.get(cntComment).getHTMLText().hashCode();
        }


        return hashCode;
    }
}
