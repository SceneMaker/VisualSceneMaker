package de.dfki.vsm.model.sceneflow.glue.command.invocation;

import de.dfki.vsm.model.sceneflow.glue.command.Invocation;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.util.ArrayList;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class PlayActionActivity extends Invocation {

    public enum PlayMode {
        Default,
        Concurrent,
        Sequential
    }

    private PlayMode mMode;
    private Expression mCommand;
    private ArrayList<Expression> mArgList;

    public PlayActionActivity() {
        mMode = PlayMode.Concurrent;
        mCommand = null;
        mArgList = new ArrayList();
    }

    public PlayActionActivity(
            final Expression arg,
            final PlayMode mode) {
        mMode = mode;
        mCommand = arg;
        mArgList = new ArrayList();
    }

    public PlayActionActivity(
            final Expression arg,
            final ArrayList<Expression> argList,
            final PlayMode mode) {
        mMode = mode;
        mCommand = arg;
        mArgList = argList;
    }

    public final Expression getCommand() {
        return mCommand;
    }

    public final void setCommand(final Expression arg) {
        mCommand = arg;
    }

    public final ArrayList<Expression> getArgList() {
        return mArgList;
    }

    public void setArgList(ArrayList<Expression> value) {
        mArgList = value;
    }

    public final ArrayList<Expression> getCopyOfArgList() {
        final ArrayList<Expression> copy = new ArrayList();
        for (final Expression exp : mArgList) {
            copy.add(exp.getCopy());
        }
        return copy;
    }

    @Override
    public final String getAbstractSyntax() {
        StringBuilder desc = new StringBuilder("PlayAction(");
        desc.append((mCommand != null)
                ? mCommand.getAbstractSyntax()
                : "");
        for (Expression expression : mArgList) {
            desc.append(", ").append(expression.getAbstractSyntax());
        }
        return desc + ")";
    }

    @Override
    public final String getConcreteSyntax() {

        StringBuilder desc = new StringBuilder((mMode == PlayMode.Default ? "PlayAction("
                : (mMode == PlayMode.Sequential ? "!-" : "!=")));
        desc.append((mCommand != null)
                ? mCommand.getConcreteSyntax()
                : "");
        for (Expression expression : mArgList) {
            desc.append(", ").append(expression.getConcreteSyntax());
        }
         return desc + (mMode == PlayMode.Default ? ")"
                : (mMode == PlayMode.Sequential ? "." : "."));
    }

    // TODO: Check the mode for syntax here
    @Override
    public final String getFormattedSyntax() {
        StringBuilder desc = new StringBuilder("#p#" + (mMode == PlayMode.Default ? "PlayAction ( "
                : (mMode == PlayMode.Sequential ? "!- " : "!= ")));
        desc.append((mCommand != null)
                ? mCommand.getFormattedSyntax()
                : "");
        for (Expression expression : mArgList) {
            desc.append(" , ").append(expression.getFormattedSyntax());
        }
        return desc + (mMode == PlayMode.Default ? " ) "
                : (mMode == PlayMode.Sequential ? " ." : " ."));
    }

    @Override
    public final PlayActionActivity getCopy() {
        return new PlayActionActivity(mCommand.getCopy(), getCopyOfArgList(), mMode);
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) throws XMLWriteError {
        out.println("<PlayAction mode=\"" + mMode + "\">").push();
        if (mCommand != null) {
            mCommand.writeXML(out);
        }
        for (Expression expression : mArgList) {
            expression.writeXML(out);
        }
        out.pop().println("</PlayAction>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {

        mMode = PlayMode.valueOf(element.getAttribute("mode"));

        final ArrayList<Expression> expList = new ArrayList();
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(Element element) throws XMLParseError {
                expList.add(Expression.parse(element));
            }
        });
        mCommand = expList.remove(0);
        mArgList = expList;
    }
}
