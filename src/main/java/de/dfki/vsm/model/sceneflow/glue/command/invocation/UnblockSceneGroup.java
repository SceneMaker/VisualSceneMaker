package de.dfki.vsm.model.sceneflow.glue.command.invocation;

import de.dfki.vsm.model.sceneflow.glue.command.Invocation;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class UnblockSceneGroup extends Invocation {

    private Expression mArgument;

    public UnblockSceneGroup() {
        mArgument = null;
    }

    public UnblockSceneGroup(final Expression arg) {
        mArgument = arg;
    }

    public final Expression getArg() {
        return mArgument;
    }

    public final void setArg(final Expression value) {
        mArgument = value;
    }

    @Override
    public final String getAbstractSyntax() {
        return "UnblockSceneGroup(" + ((mArgument != null)
                ? mArgument.getAbstractSyntax()
                : "") + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return "UnblockSceneGroup(" + ((mArgument != null)
                ? mArgument.getConcreteSyntax()
                : "") + ")";
    }

    @Override
    public final String getFormattedSyntax() {
        return "#p#UnblockSceneGroup ( " + ((mArgument != null)
                ? mArgument.getFormattedSyntax()
                : "") + " ) ";
    }

    @Override
    public final UnblockSceneGroup getCopy() {
        return new UnblockSceneGroup(mArgument.getCopy());
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) throws XMLWriteError {
        out.println("<UnblockSceneGroup>").push();
        if (mArgument != null) {
            mArgument.writeXML(out);
        }
        out.pop().println("</UnblockSceneGroup>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public final void run(final Element element) throws XMLParseError {
                mArgument = Expression.parse(element);
            }
        });
    }
}