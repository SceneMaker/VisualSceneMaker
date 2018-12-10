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
public final class UnaryExpression extends Expression {

    private Expression mExp;
    private UnaryOp mOperator;

    public enum UnaryOp {

        Neg, Not, Lnot, Inc, Dec
    }

    public UnaryExpression() {
        mExp = null;
        mOperator = null;
    }

    public UnaryExpression(Expression exp, UnaryOp operator) {
        mExp = exp;
        mOperator = operator;
    }

    public void setExp(Expression value) {
        mExp = value;
    }

    public Expression getExp() {
        return mExp;
    }

    public void setOperator(UnaryOp value) {
        mOperator = value;
    }

    public UnaryOp getOperator() {
        return mOperator;
    }

    @Override
    public String getAbstractSyntax() {
        return "UnaryExp( " + ((mOperator != null)
                ? mOperator.name()
                : "") + " , " + ((mExp != null)
                        ? mExp.getAbstractSyntax()
                        : "") + " )";
    }

    @Override
    public String getConcreteSyntax() {
        String op = "";
        String exp = (mExp != null)
                ? mExp.getConcreteSyntax()
                : "";
        if (mOperator == null) {
            return "";
        }
        switch (mOperator) {
            case Neg:
                op = "- " + exp;
                break;
            case Not:
                op = "! " + exp;
                break;
            case Lnot:
                op = "~ " + exp;
                break;
            case Inc:
                op = "++ " + exp;
                break;
            case Dec:
                op = "-- " + exp;
                break;
        }
        return op;
    }

    @Override
    public String getFormattedSyntax() {
        String op = "";
        String exp = (mExp != null)
                ? mExp.getFormattedSyntax()
                : "";
        if (mOperator == null) {
            return "";
        }
        switch (mOperator) {
            case Neg:
                op = "- " + exp;
                break;
            case Not:
                op = "! " + exp;
                break;
            case Lnot:
                op = "~ " + exp;
                break;
            case Inc:
                op = "++ " + exp;
                break;
            case Dec:
                op = "-- " + exp;
                break;
        }
        return op;
    }

    @Override
    public UnaryExpression getCopy() {
        return new UnaryExpression(mExp.getCopy(), mOperator);
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<" + mOperator.name() + ">").push();
        mExp.writeXML(out);
        out.pop().println("</" + mOperator.name() + ">");
    }

    @Override
    public void parseXML(Element element) throws XMLParseError {
        mOperator = UnaryOp.valueOf(element.getTagName());
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(Element element) throws XMLParseError {
                mExp = Expression.parse(element);
            }
        });
    }
}
