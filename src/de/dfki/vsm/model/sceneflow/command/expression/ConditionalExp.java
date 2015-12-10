package de.dfki.vsm.model.sceneflow.command.expression;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.expression.condition.Condition;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

/**
 * @author Not me
 */
public class ConditionalExp extends Expression {
    private Condition  mCondition;
    private Expression mThenExp;
    private Expression mElseExp;

    public ConditionalExp() {
        mCondition = null;
        mThenExp   = null;
        mElseExp   = null;
    }

    public ConditionalExp(Condition cond, Expression thenExp, Expression elseExp) {
        mCondition = cond;
        mThenExp   = thenExp;
        mElseExp   = elseExp;
    }

    public Condition getCondition() {
        return mCondition;
    }

    public void setCondition(Condition value) {
        mCondition = value;
    }

    public Expression getThenExp() {
        return mThenExp;
    }

    public void setThenExp(Expression value) {
        mThenExp = value;
    }

    public Expression getElseExp() {
        return mElseExp;
    }

    public void setElseExp(Expression value) {
        mElseExp = value;
    }

    public ExpType getExpType() {
        return ExpType.IF;
    }

    public String getAbstractSyntax() {
        return "ConditionalExp( " + ((mCondition != null)
                                     ? mCondition.getAbstractSyntax()
                                     : "") + " , " + ((mThenExp != null)
                ? mThenExp.getAbstractSyntax()
                : "") + " , " + ((mElseExp != null)
                                 ? mElseExp.getAbstractSyntax()
                                 : "") + " )";
    }

    public String getConcreteSyntax() {
        return "( " + ((mCondition != null)
                       ? mCondition.getConcreteSyntax()
                       : "") + " ? " + ((mThenExp != null)
                                        ? mThenExp.getConcreteSyntax()
                                        : "") + " : " + ((mElseExp != null)
                ? mElseExp.getConcreteSyntax()
                : "") + " )";
    }

    public String getFormattedSyntax() {
        return "( " + ((mCondition != null)
                       ? mCondition.getFormattedSyntax()
                       : "") + " ? " + ((mThenExp != null)
                                        ? mThenExp.getFormattedSyntax()
                                        : "") + " : " + ((mElseExp != null)
                ? mElseExp.getFormattedSyntax()
                : "") + " )";
    }

    public ConditionalExp getCopy() {
        return new ConditionalExp(mCondition.getCopy(), mThenExp.getCopy(), mElseExp.getCopy());
    }

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

    public void parseXML(Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                if (element.getTagName().equals("Then")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        public void run(Element element) throws XMLParseError {
                            mThenExp = Expression.parse(element);
                        }
                    });
                } else if (element.getTagName().equals("Else")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        public void run(Element element) throws XMLParseError {
                            mElseExp = Expression.parse(element);
                        }
                    });
                } else {
                    mCondition = Condition.parse(element);
                }
            }
        });
    }
}
