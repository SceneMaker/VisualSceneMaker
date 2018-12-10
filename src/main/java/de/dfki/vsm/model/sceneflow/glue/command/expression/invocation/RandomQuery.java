package de.dfki.vsm.model.sceneflow.glue.command.expression.invocation;

import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class RandomQuery extends Expression {

    private Expression mExpression;

    public RandomQuery() {
    }

    public RandomQuery(final Expression exp) {
        mExpression = exp;
    }

    public final Expression getExpression() {
        return mExpression;
    }

    @Override
    public final String getAbstractSyntax() {
        return "RandomValue(" + mExpression.getAbstractSyntax() + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return "Random(" + mExpression.getConcreteSyntax() + ")";
    }

    @Override
    public final String getFormattedSyntax() {
        return "#p#Random ( " + "#c#" + mExpression.getConcreteSyntax() + " ) ";
    }

    @Override
    public final RandomQuery getCopy() {
        return new RandomQuery(mExpression.getCopy());
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) throws XMLWriteError {
        out.println("<RandomQuery>");
        mExpression.writeXML(out);
        out.println("</RandomQuery>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public final void run(final Element element) throws XMLParseError {
                mExpression = Expression.parse(element);
            }
        });
    }
}
