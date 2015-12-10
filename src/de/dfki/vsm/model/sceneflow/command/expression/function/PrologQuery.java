package de.dfki.vsm.model.sceneflow.command.expression.function;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann me
 */
public final class PrologQuery extends Expression {

    Expression mExpression;

    public PrologQuery() {
    }

    public PrologQuery(final Expression cmd) {
        mExpression = cmd;
    }

    public final Expression getExpression() {
        return mExpression;
    }

    @Override
    public final ExpType getExpType() {
        return ExpType.PROLOG_QUERY;
    }

    @Override
    public final String getAbstractSyntax() {
        return "PrologCondition(" + mExpression.getAbstractSyntax() + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return "query(" + mExpression.getConcreteSyntax() + ")";
    }

    @Override
    public final String getFormattedSyntax() {
        return "query" + mExpression.getFormattedSyntax() + ")";
    }

    @Override
    public final PrologQuery getCopy() {
        return new PrologQuery(mExpression.getCopy());
    }

    @Override
    public final void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<PrologCondition>");
        mExpression.writeXML(out);
        out.println("</PrologCondition>");
    }

    @Override
    public final void parseXML(Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(Element element) throws XMLParseError {
                mExpression = Expression.parse(element);
            }
        });
    }
}
