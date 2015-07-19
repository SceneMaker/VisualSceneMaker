package de.dfki.vsm.model.sceneflow;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.util.Preferences;
import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.model.sceneflow.definition.FunDef;
import de.dfki.vsm.model.sceneflow.definition.VarDef;
import de.dfki.vsm.model.sceneflow.definition.type.TypeDef;
import de.dfki.vsm.util.cpy.CopyTool;
import de.dfki.vsm.util.ios.IndentWriter;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import org.xml.sax.SAXException;

//~--- JDK imports ------------------------------------------------------------

import java.io.File;
import java.io.IOException;

import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Date;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public class SceneFlow extends SuperNode {

    // protected Logger mLogger = Logger.getInstance();
    // protected String mSceneFileName = new String();
    // protected String mSceneInfoFileName = new String();
    // protected VariableBadge mVariableBadge = null;
    protected String                  mXMLNameSpace      = new String();
    protected String                  mXMLSchemeInstance = new String();
    protected String                  mXMLSchemeLocation = new String();
    protected String                  mPackageName       = new String();
    protected String                  mContextClass      = new String();
    protected String                  mContextCode       = new String();
    protected Vector<String>          mClassPathList     = new Vector<String>();
    protected HashMap<String, FunDef> mUserCmdDefMap     = new HashMap<String, FunDef>();
    protected String                  mModifDate         = new String();

    public SceneFlow() {}

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

    public Vector<String> getClassPathList() {
        return mClassPathList;
    }

    public void setClassPathList(Vector<String> classPath) {
        mClassPathList = classPath;
    }

    public Vector<String> getCopyOfClassPathList() {
        Vector<String> copy = new Vector<String>();

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
        Iterator                it   = mUserCmdDefMap.entrySet().iterator();

        while (it.hasNext()) {
            Map.Entry pairs           = (Map.Entry) it.next();
            String    userCommandName = (String) pairs.getKey();
            FunDef    userCommand     = (FunDef) pairs.getValue();
            FunDef    userCommandCopy = userCommand.getCopy();

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
    public void writeXML(IndentWriter out) throws XMLWriteError {
        String start = "";

        for (String id : mStartNodeMap.keySet()) {
            start += id + ";";
        }

        out.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
        out.println("<SceneFlow " + "id=\"" + mId + "\" " + "name=\"" + mName + "\" " + "comment=\"" + mComment
                    + "\" hideLocalVar=\"" + mHideLocalVarBadge + "\" hideGlobalVar=\"" + mHideGlobalVarBadge + "\" "
                    + "exhaustive=\"" + mExhaustive + "\" " + "preserving=\"" + mPreserving + "\" " + "modifDate=\"" + new SimpleDateFormat("dd.MM.yyyy").format(new Date()) + "\" "+ "start=\""
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
        out.pop().println("</SceneFlow>");
    }

    public void parseFromXMLFile(URL url) throws

    /*
     *  IOException,
     * SAXException,
     * ParserConfigurationException,
     */
    XMLParseError {
        DocumentBuilder        parser                 = null;
        Schema                 schema                 = null;
        Document               document               = null;
        DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();

        documentBuilderFactory.setNamespaceAware(true);

        // Create parser
        try {
            parser = documentBuilderFactory.newDocumentBuilder();

            // Logger.getInstance().info("XML Parser configuration successfull!");
        } catch (ParserConfigurationException e) {

            // e.printStackTrace();
            LOGDefaultLogger.getInstance().failure("ERROR: XML Parser configuration failed!");
        }

        // Create the XML schema validator
        SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);

        try {

            // PG: XSD is loaded from scenemaker.jar!
            URL sceneFlowXSDURL = null;

            try {
                sceneFlowXSDURL = getClass().getResource("data/xsd/sceneflow.xsd");
            } catch (Exception e) {
                e.printStackTrace();
            }

            schema = schemaFactory.newSchema(sceneFlowXSDURL);
            LOGDefaultLogger.getInstance().message("XML Schema configuration successfull!");
        } catch (SAXException e) {

            // e.printStackTrace();
            LOGDefaultLogger.getInstance().failure("ERROR: XML Schema configuration failed!");
        }

        Validator validator = schema.newValidator();

        // added PG: Parse from URL
        try {
            document = parser.parse(url.openStream());

            // Logger.getInstance().info("Parsing of file '" + url.getPath() + "' successfull!");
        } catch (IOException e) {

            // e.printStackTrace();
            LOGDefaultLogger.getInstance().message("ERROR: Cannot read file '" + url.getPath() + "'!");

            // throw e;
        } catch (SAXException e) {

            // e.printStackTrace();
            LOGDefaultLogger.getInstance().failure("ERROR: Cannot parse file '" + url.getPath() + "'!");

            // throw e;
        }

        // Validate
        try {
            validator.validate(new DOMSource(document));
            LOGDefaultLogger.getInstance().message("Validation of file '" + url.getPath() + "' successfull!");
        } catch (IOException e) {

            // e.printStackTrace();
            LOGDefaultLogger.getInstance().failure("ERROR: Cannot read file '" + url.getPath() + "'!");

            // throw e;
        } catch (SAXException e) {

            // e.printStackTrace();
            LOGDefaultLogger.getInstance().failure("ERROR: Cannot validate file '" + url.getPath() + "'!");

            // throw e;
        }

        parseXML(document.getDocumentElement());
    }

    public void parseFromXMLFile(File file) throws

    /*
     *  IOException,
     * SAXException,
     * ParserConfigurationException,
     */
    XMLParseError {
        DocumentBuilder        parser                 = null;
        Schema                 schema                 = null;
        Document               document               = null;
        DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();

        documentBuilderFactory.setNamespaceAware(true);

        // Create parser
        try {
            parser = documentBuilderFactory.newDocumentBuilder();

            // Logger.getInstance().info("XML Parser configuration successfull!");
        } catch (ParserConfigurationException e) {

            // e.printStackTrace();
            LOGDefaultLogger.getInstance().failure("ERROR: XML Parser configuration failed!");
        }

        // Create the XML schema validator
//      SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
//      try {
//          // TODO: get the properties file xsd
//          schema = schemaFactory.newSchema(new StreamSource(new File("data/xsd/sceneflow.xsd")));//ResourceLoader.class.getResourceAsStream("data/xsd/sceneflow.xsd")
//          Logger.getInstance().info("XML Schema configuration successfull!");
//      } catch (SAXException e) {
//          e.printStackTrace();
//          Logger.getInstance().info("ERROR: XML Schema configuration failed!");
//      }
//      Validator validator = schema.newValidator();
        // Parse
        try {
            document = parser.parse(file);

            // Logger.getInstance().info("Parsing of file '" + file.getAbsolutePath() + "' successfull!");
        } catch (IOException e) {

            // e.printStackTrace();
            LOGDefaultLogger.getInstance().failure("ERROR: Cannot read file '" + file.getAbsolutePath() + "'!");

            // throw e;
        } catch (SAXException e) {

            // e.printStackTrace();
            LOGDefaultLogger.getInstance().failure("ERROR: Cannot parse file '" + file.getAbsolutePath() + "'!");

            // throw e;
        }

        // Validate
//      try {
//          validator.validate(new DOMSource(document));
//          Logger.getInstance().info("Validation of file '" + file.getAbsolutePath() + "' successfull!");
//      } catch (IOException e) {
//          //e.printStackTrace();
//          Logger.getInstance().info("ERROR: Cannot read file '" + file.getAbsolutePath() + "'!");
//          //throw e;
//      } catch (SAXException e) {
//          //e.printStackTrace();
//          Logger.getInstance().info("ERROR: Cannot validate file '" + file.getAbsolutePath() + "'!");
//          //throw e;
//      }
        parseXML(document.getDocumentElement());
    }

    @Override
    public void parseXML(Element element) throws XMLParseError {
        mId      = element.getAttribute("id");
        mName    = element.getAttribute("name");
        mComment = element.getAttribute("comment");

        String start = element.getAttribute("start");

        mContextClass       = element.getAttribute("context");
        mPackageName        = element.getAttribute("package");
        mExhaustive         = Boolean.valueOf(element.getAttribute("exhaustive"));
        mPreserving         = Boolean.valueOf(element.getAttribute("preserving"));
        mXMLSchemeLocation  = element.getAttribute("xsi:schemaLocation");
        mXMLNameSpace       = element.getAttribute("xmlns");
        mXMLSchemeInstance  = element.getAttribute("xmlns:xsi");
        mHideLocalVarBadge  = Boolean.valueOf(element.getAttribute("hideLocalVar"));
        mHideGlobalVarBadge = Boolean.valueOf(element.getAttribute("hideGlobalVar"));
        mModifDate          = element.getAttribute("modifDate");
        // mSceneFileName = element.getAttribute("scenefile");
        // mSceneInfoFileName = element.getAttribute("sceneinfo");

        /**
         * Construct start node list from the start string
         */
        String[] arr = start.split(";");

        for (String str : arr) {
            if (!str.isEmpty()) {
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
                    Node node = new Node();

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
                        + fundDef.getMethod().hashCode() + fundDef.getParamList().hashCode();
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

        // Add hash of all comments on workspace
        for (int cntComment = 0; cntComment < getCommentList().size(); cntComment++) {
            hashCode += mCommentList.get(cntComment).mGraphics.toString().hashCode();
            hashCode += mCommentList.get(cntComment).mHTMLText.hashCode();
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

        return hashCode;
    }
}
