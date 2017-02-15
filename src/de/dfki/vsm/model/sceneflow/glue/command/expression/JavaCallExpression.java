package de.dfki.vsm.model.sceneflow.glue.command.expression;

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
public final class JavaCallExpression extends Expression {

    private String mName;
    private ArrayList<Expression> mArgList;

    public JavaCallExpression() {
        mName = new String();
        mArgList = new ArrayList();
    }

    public JavaCallExpression(final String name) {
        mName = name;
        mArgList = new ArrayList();
    }

    public JavaCallExpression(final String name, final ArrayList list) {
        mName = name;
        mArgList = list;
    }

    public final String getName() {
        return mName;
    }

    public final void setName(final String value) {
        mName = value;
    }

    public final ArrayList<Expression> getArgList() {
        return mArgList;
    }

    public final void setArgList(final ArrayList<Expression> value) {
        mArgList = value;
    }

    public final int getSizeOfArgList() {
        return mArgList.size();
    }

    public final ArrayList<Expression> getCopyOfArgList() {
        ArrayList<Expression> copy = new ArrayList<Expression>();

        for (Expression exp : mArgList) {
            copy.add(exp.getCopy());
        }

        return copy;
    }

    public final boolean addArg(final Expression value) {
        return mArgList.add(value);
    }

    public final Expression getArgAt(final int index) {
        return mArgList.get(index);
    }

    @Override
    public final String getAbstractSyntax() {
        String desc = "JavaCallExpression( " + mName + "( ";
        for (int i = 0; i < mArgList.size(); i++) {
            desc += mArgList.get(i).getAbstractSyntax();

            if (i != mArgList.size() - 1) {
                desc += " , ";
            }
        }
        return desc + " ) )";
    }

    @Override
    public final String getConcreteSyntax() {
        String desc = mName + " ( ";
        for (int i = 0; i < mArgList.size(); i++) {
            desc += mArgList.get(i).getConcreteSyntax();

            if (i != mArgList.size() - 1) {
                desc += " , ";
            }
        }
        return desc + " )";
    }

    @Override
    public final String getFormattedSyntax() {
        String desc = "#b#" + mName + " ( ";
        for (int i = 0; i < mArgList.size(); i++) {
            desc += mArgList.get(i).getFormattedSyntax();

            if (i != mArgList.size() - 1) {
                desc += " , ";
            }
        }
        return desc + " ) ";
    }

    @Override
    public final JavaCallExpression getCopy() {
        return new JavaCallExpression(mName, getCopyOfArgList());
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) throws XMLWriteError {
        out.println("<JavaCallExpression name=\"" + mName + "\">").push();
        for (int i = 0; i < mArgList.size(); i++) {
            mArgList.get(i).writeXML(out);
        }
        out.pop().println("</JavaCallExpression>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public final void run(final Element element) throws XMLParseError {
                final Expression exp = Expression.parse(element);
                mArgList.add(exp);
            }
        });
    }
}
