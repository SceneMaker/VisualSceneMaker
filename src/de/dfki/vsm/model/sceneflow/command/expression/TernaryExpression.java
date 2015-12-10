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
public class TernaryExpression extends AbstractExpression {

    private AbstractExpression mCondition;
    private AbstractExpression mThenExp;
    private AbstractExpression mElseExp;

    public TernaryExpression() {
        mCondition = null;
        mThenExp = null;
        mElseExp = null;
    }

    public TernaryExpression(AbstractExpression cond, AbstractExpression thenExp, AbstractExpression elseExp) {
        mCondition = cond;
        mThenExp = thenExp;
        mElseExp = elseExp;
    }

    public AbstractExpression getCondition() {
        return mCondition;
    }

    public void setCondition(AbstractExpression value) {
        mCondition = value;
    }

    public AbstractExpression getThenExp() {
        return mThenExp;
    }

    public void setThenExp(AbstractExpression value) {
        mThenExp = value;
    }

    public AbstractExpression getElseExp() {
        return mElseExp;
    }

    public void setElseExp(AbstractExpression value) {
        mElseExp = value;
    }

    @Override
    public ExpType getExpType() {
        return ExpType.CONDITIONAL;
    }

    @Override
    public String getAbstractSyntax() {
        return "ConditionalExp( " + ((mCondition != null)
                ? mCondition.getAbstractSyntax()
                : "") + " , " + ((mThenExp != null)
                ? mThenExp.getAbstractSyntax()
                : "") + " , " + ((mElseExp != null)
                ? mElseExp.getAbstractSyntax()
                : "") + " )";
    }

    @Override
    public String getConcreteSyntax() {
        return "( " + ((mCondition != null)
                ? mCondition.getConcreteSyntax()
                : "") + " ? " + ((mThenExp != null)
                ? mThenExp.getConcreteSyntax()
                : "") + " : " + ((mElseExp != null)
                ? mElseExp.getConcreteSyntax()
                : "") + " )";
    }

    @Override
    public String getFormattedSyntax() {
        return "( " + ((mCondition != null)
                ? mCondition.getFormattedSyntax()
                : "") + " ? " + ((mThenExp != null)
                ? mThenExp.getFormattedSyntax()
                : "") + " : " + ((mElseExp != null)
                ? mElseExp.getFormattedSyntax()
                : "") + " )";
    }

    @Override
    public TernaryExpression getCopy() {
        return new TernaryExpression(mCondition.getCopy(), mThenExp.getCopy(), mElseExp.getCopy());
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<If>").push();
        mCondition.writeXML(out);
        out.println("<Then>").push();
        mThenExp.writeXML(out);
        out.pop().println("</Then>");
        out.println("<Else>").push();
        mElseExp.writeXML(out);
        out.pop().println("</Else>");
        out.pop().println("</If>");
    }

    @Override
    public void parseXML(Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(Element element) throws XMLParseError {
                if (element.getTagName().equals("Then")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        @Override
                        public void run(Element element) throws XMLParseError {
                            mThenExp = AbstractExpression.parse(element);
                        }
                    });
                } else if (element.getTagName().equals("Else")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        @Override
                        public void run(Element element) throws XMLParseError {
                            mElseExp = AbstractExpression.parse(element);
                        }
                    });
                } else {
                    mCondition = AbstractExpression.parse(element);
                }
            }
        });
    }
}
