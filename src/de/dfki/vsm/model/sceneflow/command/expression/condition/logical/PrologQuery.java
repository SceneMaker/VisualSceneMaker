package de.dfki.vsm.model.sceneflow.command.expression.condition.logical;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class PrologQuery extends LogicalCond {

    //
    private Expression mExpression;

    public PrologQuery() {
    }

    public PrologQuery(final Expression expression) {
        mExpression = expression;
    }

    public final Expression getExpression() {
        return mExpression;
    }
//
//    public LogicalType getLogicalType() {
//        return LogicalType.PROLOG;
//    }

    @Override
    public final String getAbstractSyntax() {
        return "PrologQuery ( " + mExpression.getAbstractSyntax() + " )";
    }

    @Override
    public final String getConcreteSyntax() {
        return "Query ( " + mExpression.getConcreteSyntax() + " )";
    }

    @Override
    public String getFormattedSyntax() {
        return "#p#Query ( " + mExpression.getFormattedSyntax() + " )";
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
            public void run(final Element element) throws XMLParseError {
                mExpression = Expression.parse(element);
            }
        });
    }
}
