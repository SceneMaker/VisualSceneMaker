package de.dfki.vsm.model.sceneflow.command;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

/**
 * @author Not me
 */
public class UnblockSceneGroup extends Command {
    private Expression mArg;

    public UnblockSceneGroup() {
        mArg = null;
    }

    public UnblockSceneGroup(Expression arg) {
        mArg = arg;
    }

    public Expression getArg() {
        return mArg;
    }

    public void setArg(Expression value) {
        mArg = value;
    }

    public CmdType getCmdType() {
        return CmdType.USG;
    }

    public String getAbstractSyntax() {
        return "Command(UnblockSceneGroup (" + ((mArg != null)
                ? mArg.getAbstractSyntax()
                : "") + "))";
    }

    public String getConcreteSyntax() {
        return "UnblockSceneGroup ( " + ((mArg != null)
                                         ? mArg.getConcreteSyntax()
                                         : "") + " )";
    }

    public String getFormattedSyntax() {
        return "#p#UnblockSceneGroup ( " + ((mArg != null)
                ? mArg.getFormattedSyntax()
                : "") + " )";
    }

    public UnblockSceneGroup getCopy() {
        return new UnblockSceneGroup(mArg.getCopy());
    }

    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<UnblockSceneGroup>").push();

        if (mArg != null) {
            mArg.writeXML(out);
        }

        out.pop().println("</UnblockSceneGroup>");
    }

    public void parseXML(Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                mArg = Expression.parse(element);
            }
        });
    }
}
