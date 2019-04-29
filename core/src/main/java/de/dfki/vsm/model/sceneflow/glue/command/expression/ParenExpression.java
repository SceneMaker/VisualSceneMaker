package de.dfki.vsm.model.sceneflow.glue.command.expression;

import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class ParenExpression extends Expression {

    private Expression mExp;

    public ParenExpression() {
        mExp = null;
    }

    public ParenExpression(final Expression exp) {
        mExp = exp;
    }

    public final void setExp(final Expression value) {
        mExp = value;
    }

    public final Expression getExp() {
        return mExp;
    }


    @Override
    public final String getAbstractSyntax() {
        return "ParenExpression( " + mExp.getAbstractSyntax() + " )";
    }

    @Override
    public final String getConcreteSyntax() {
        return "( " + mExp.getConcreteSyntax() + " )";
    }

    @Override
    public final String getFormattedSyntax() {
        return "( " + mExp.getFormattedSyntax() + " )";
    }

    @Override
    public final ParenExpression getCopy() {
        return new ParenExpression(mExp.getCopy());
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) throws XMLWriteError {
        out.println("<ParenExpression>").push();
        mExp.writeXML(out);
        out.pop().println("</ParenExpression>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public final void run(final Element element) throws XMLParseError {
                mExp = Expression.parse(element);
            }
        });
    }
}