package de.dfki.vsm.model.sceneflow.command;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.ios.IOSIndentWriter;

import org.w3c.dom.Element;

/**
 * @author Not me
 */
public class UnblockAllSceneGroups extends Command {
    private boolean mNonAbstract;

    public UnblockAllSceneGroups() {
        mNonAbstract = Boolean.FALSE;
    }

    public UnblockAllSceneGroups(boolean nonAbstract) {
        mNonAbstract = nonAbstract;
    }

    public void setNonAbstract(boolean value) {
        mNonAbstract = value;
    }

    public boolean getNonAbstract() {
        return mNonAbstract;
    }

    public CmdType getCmdType() {
        return CmdType.UASG;
    }

    public String getAbstractSyntax() {
        return "Command(UnblockAllSceneGroups ( " + mNonAbstract + " ))";
    }

    public String getConcreteSyntax() {
        return "UnblockAllSceneGroups ( " + mNonAbstract + " )";
    }

    public String getFormattedSyntax() {
        return "#p#UnblockAllSceneGroups ( " + "#r#" + mNonAbstract + " )";
    }

    public UnblockAllSceneGroups getCopy() {
        return new UnblockAllSceneGroups(mNonAbstract);
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<UnblockAllSceneGroups non-abstract=\"" + mNonAbstract + "\"/>");
    }

    public void parseXML(Element element) {
        mNonAbstract = Boolean.valueOf(element.getAttribute("non-abstract"));
    }
}
