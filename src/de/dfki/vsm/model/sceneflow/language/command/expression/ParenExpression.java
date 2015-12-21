package de.dfki.vsm.model.sceneflow.language.command.expression;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.model.sceneflow.language.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

/**
 * @author Not me
 */
public class ParenExpression extends Expression {

    private Expression mExp;

    public ParenExpression() {
        mExp = null;

    }

    public ParenExpression(Expression exp) {
        mExp = exp;
    }

    public void setExp(Expression value) {
        mExp = value;
    }

    public Expression getExp() {
        return mExp;
    }

//    @Override
//    public ExpType getExpType() {
//        return ExpType.PARENEXP;
//    }

    @Override
    public String getAbstractSyntax() {
        return "ParenthesesExpression( " + mExp.getAbstractSyntax() + " )";
    }

    @Override
    public String getConcreteSyntax() {
        return "( " + mExp.getConcreteSyntax() + " )";
    }

    @Override
    public String getFormattedSyntax() {
        return "( " + mExp.getFormattedSyntax() + " )";
    }

    @Override
    public ParenExpression getCopy() {
        return new ParenExpression(mExp.getCopy());
    }

    @Override
    public void writeXML(final IOSIndentWriter out) throws XMLWriteError {
        out.println("<Parentheses>").push();
        mExp.writeXML(out);
        out.pop().println("</Parentheses>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(final Element element) throws XMLParseError {
                mExp = Expression.parse(element);
            }
        });
    }
}
