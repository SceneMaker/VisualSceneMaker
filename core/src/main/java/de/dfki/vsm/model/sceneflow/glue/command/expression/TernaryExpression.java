package de.dfki.vsm.model.sceneflow.glue.command.expression;

import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class TernaryExpression extends Expression {

    private Expression mCondition;
    private Expression mThenExp;
    private Expression mElseExp;

    public TernaryExpression() {
        mCondition = null;
        mThenExp = null;
        mElseExp = null;
    }

    public TernaryExpression(Expression cond, Expression thenExp, Expression elseExp) {
        mCondition = cond;
        mThenExp = thenExp;
        mElseExp = elseExp;
    }

    public Expression getCondition() {
        return mCondition;
    }

    public void setCondition(Expression value) {
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

    @Override
    public String getAbstractSyntax() {
        return "TernaryExpression( " + ((mCondition != null)
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
        out.println("<TernaryExpression>").push();
        mCondition.writeXML(out);
        out.println("<Then>").push();
        mThenExp.writeXML(out);
        out.pop().println("</Then>");
        out.println("<Else>").push();
        mElseExp.writeXML(out);
        out.pop().println("</Else>");
        out.pop().println("</TernaryExpression>");
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
                            mThenExp = Expression.parse(element);
                        }
                    });
                } else if (element.getTagName().equals("Else")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        @Override
                        public void run(Element element) throws XMLParseError {
                            mElseExp = Expression.parse(element);
                        }
                    });
                } else {
                    mCondition = Expression.parse(element);
                }
            }
        });
    }
}
