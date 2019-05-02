package de.dfki.vsm.model.sceneflow.glue.command.expression.record;

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
public final class ArrayExpression extends Expression {

    final private ArrayList<Expression> mExpList;

    public ArrayExpression() {
        mExpList = new ArrayList<>();
    }

    public ArrayExpression(final ArrayList expList) {
        mExpList = expList;
    }

    public final ArrayList<Expression> getExpList() {
        return mExpList;
    }

//    public final void setExpList(ArrayList expList) {
//        mExpList = expList;
//    }

    public final ArrayList<Expression> getCopyOfExpList() {
        final ArrayList<Expression> copy = new ArrayList();
        for (final Expression exp : mExpList) {
            copy.add(exp.getCopy());
        }
        return copy;
    }

    @Override
    public final String getAbstractSyntax() {
        StringBuilder desc = new StringBuilder();
        for (int i = 0; i < mExpList.size(); i++) {
            desc.append(mExpList.get(i).getConcreteSyntax());

            if (i != mExpList.size() - 1) {
                desc.append(" , ");
            }
        }
        return "List(" + desc + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        StringBuilder desc = /* mType + */ new StringBuilder("[ ");
        for (int i = 0; i < mExpList.size(); i++) {
            desc.append(mExpList.get(i).getConcreteSyntax());

            if (i != mExpList.size() - 1) {
                desc.append(" , ");
            }
        }
        return desc + " ]";
    }

    @Override
    public final String getFormattedSyntax() {
        StringBuilder desc = /* mType + */ new StringBuilder("[ ");
        for (int i = 0; i < mExpList.size(); i++) {
            desc.append(mExpList.get(i).getFormattedSyntax());

            if (i != mExpList.size() - 1) {
                desc.append(" , ");
            }
        }
        return desc + " ]";
    }

    @Override
    public final ArrayExpression getCopy() {
        return new ArrayExpression(getCopyOfExpList());
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) throws XMLWriteError {
        out.println("<ArrayExpression>").push();
        for (Expression expression : mExpList) {
            expression.writeXML(out);
        }
        out.pop().println("</ArrayExpression>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {

        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public final void run(final Element element) throws XMLParseError {
                final Expression exp = Expression.parse(element);
                mExpList.add(exp);
            }
        });
    }
}