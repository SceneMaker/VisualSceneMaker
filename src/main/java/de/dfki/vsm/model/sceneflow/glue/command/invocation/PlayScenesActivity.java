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
public final class PlayScenesActivity extends Invocation {

    private Expression mArgument;
    private ArrayList<Expression> mArgList;

    public PlayScenesActivity() {
        mArgument = null;
        mArgList = new ArrayList();
    }

    public PlayScenesActivity(Expression arg) {
        mArgument = arg;
        mArgList = new ArrayList();
    }

    public PlayScenesActivity(final Expression arg, final ArrayList argList) {
        mArgument = arg;
        mArgList = argList;
    }

    public final Expression getArgument() {
        return mArgument;
    }

    public final void setArgument(final Expression arg) {
        mArgument = arg;
    }

    public final ArrayList<Expression> getArgList() {
        return mArgList;
    }

    public final void setArgList(final ArrayList value) {
        mArgList = value;
    }

    public final ArrayList<Expression> getCopyOfArgList() {
        ArrayList<Expression> copy = new ArrayList();
        for (final Expression exp : mArgList) {
            copy.add(exp.getCopy());
        }
        return copy;
    }

    @Override
    public final String getAbstractSyntax() {
        StringBuilder desc = new StringBuilder("PlayScene(");
        desc.append((mArgument != null)
                ? mArgument.getAbstractSyntax()
                : "");
        for (Expression expression : mArgList) {
            desc.append(", ").append(expression.getAbstractSyntax());
        }
        return desc + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        StringBuilder desc = new StringBuilder("PlayScene(");
        desc.append((mArgument != null)
                ? mArgument.getConcreteSyntax()
                : "");
        for (Expression expression : mArgList) {
            desc.append(", ").append(expression.getConcreteSyntax());
        }
        return desc + ")";
    }

    @Override
    public final String getFormattedSyntax() {
        StringBuilder desc = new StringBuilder("#p#PlayScene ( ");
        desc.append((mArgument != null)
                ? mArgument.getFormattedSyntax()
                : "");
        for (Expression expression : mArgList) {
            desc.append(" , ").append(expression.getFormattedSyntax());
        }
        return desc + " ) ";
    }

    @Override
    public final PlayScenesActivity getCopy() {
        return new PlayScenesActivity(mArgument.getCopy(), getCopyOfArgList());
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) throws XMLWriteError {
        out.println("<PlayScene>").push();
        if (mArgument != null) {
            mArgument.writeXML(out);
        }
        for (Expression expression : mArgList) {
            expression.writeXML(out);
        }
        out.pop().println("</PlayScene>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        final ArrayList<Expression> expList = new ArrayList();
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public final void run(final Element element) throws XMLParseError {
                expList.add(Expression.parse(element));
            }
        });
        mArgument = expList.remove(0);
        mArgList = expList;
    }
}
