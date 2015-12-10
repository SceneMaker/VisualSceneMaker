package de.dfki.vsm.model.sceneflow.command.expression;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

/**
 * @author Not me
 */
public class ParenthesesExpression extends AbstractExpression {

    private AbstractExpression mExp;

    public ParenthesesExpression() {
        mExp = null;

    }

    public ParenthesesExpression(AbstractExpression exp) {
        mExp = exp;
    }

    public void setExp(AbstractExpression value) {
        mExp = value;
    }

    public AbstractExpression getExp() {
        return mExp;
    }

    @Override
    public ExpType getExpType() {
        return ExpType.PARENEXP;
    }

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
    public ParenthesesExpression getCopy() {
        return new ParenthesesExpression(mExp.getCopy());
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
                mExp = AbstractExpression.parse(element);
            }
        });
    }
}
