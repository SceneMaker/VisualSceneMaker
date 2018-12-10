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
public final class TimeoutQuery extends Expression {

    // private long mTimeout;
    private Expression mExpression;

    public TimeoutQuery() {
    }

    public TimeoutQuery(final Expression exp) {
        mExpression = exp;
    }

    public final Expression getExpression() {
        return mExpression;
    }

    @Override
    public final String getAbstractSyntax() {
        return "TimeoutCond(" + mExpression.getAbstractSyntax() + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return "Timeout(" + mExpression.getConcreteSyntax() + ")";
    }

    @Override
    public final String getFormattedSyntax() {
        return "#p#Timeout ( " + "#c#" + mExpression.getConcreteSyntax() + " ) ";
    }

    @Override
    public final TimeoutQuery getCopy() {
        return new TimeoutQuery(mExpression.getCopy());
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) throws XMLWriteError {
        out.println("<TimeoutQuery>");
        mExpression.writeXML(out);
        out.println("</TimeoutQuery>");
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
