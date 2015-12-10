package de.dfki.vsm.model.sceneflow.language.command.invocation;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.language.command.Invocation;
import de.dfki.vsm.util.ios.IOSIndentWriter;

import org.w3c.dom.Element;

/**
 * @author Not me
 */
public class UnblockSceneScript extends Invocation {
    private boolean mNonAbstract;

    public UnblockSceneScript() {
        mNonAbstract = Boolean.FALSE;
    }

    public UnblockSceneScript(boolean nonAbstract) {
        mNonAbstract = nonAbstract;
    }

    public void setNonAbstract(boolean value) {
        mNonAbstract = value;
    }

    public boolean getNonAbstract() {
        return mNonAbstract;
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

    public UnblockSceneScript getCopy() {
        return new UnblockSceneScript(mNonAbstract);
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<UnblockAllSceneGroups non-abstract=\"" + mNonAbstract + "\"/>");
    }

    public void parseXML(Element element) {
        mNonAbstract = Boolean.valueOf(element.getAttribute("non-abstract"));
    }
}
