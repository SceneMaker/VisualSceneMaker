package de.dfki.vsm.model.sceneflow.language.command.expression.invocation;

import de.dfki.vsm.model.sceneflow.language.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class TimeoutFunction extends Expression {

    private Expression mExpression;

    public TimeoutFunction() {
    }

    public TimeoutFunction(final Expression exp) {
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
        return "";
    }

    @Override
    public final TimeoutFunction getCopy() {
        return new TimeoutFunction(mExpression.getCopy());
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) throws XMLWriteError {
        out.println("<TimeoutCondition>");
        mExpression.writeXML(out);
        out.println("</TimeoutCondition>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(Element element) throws XMLParseError {
                mExpression = Expression.parse(element);
            }
        });
    }
}
