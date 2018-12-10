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
public final class PrologQuery extends Expression {

    private Expression mExpression;

    public PrologQuery() {
    }

    public PrologQuery(final Expression expression) {
        mExpression = expression;
    }

    public final Expression getExpression() {
        return mExpression;
    }

    @Override
    public final String getAbstractSyntax() {
        return "PrologQuery(" + mExpression.getAbstractSyntax() + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return "?- " + mExpression.getConcreteSyntax() + ".";
    }

    @Override
    public String getFormattedSyntax() {
        return "#p#?- " + mExpression.getFormattedSyntax() + " .";
    }

    @Override
    public final PrologQuery getCopy() {
        return new PrologQuery(mExpression.getCopy());
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) throws XMLWriteError {
        out.println("<PrologQuery>");
        mExpression.writeXML(out);
        out.println("</PrologQuery>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public final void run(final Element element) throws XMLParseError {
                mExpression = Expression.parse(element);
            }
        });
    }
}
