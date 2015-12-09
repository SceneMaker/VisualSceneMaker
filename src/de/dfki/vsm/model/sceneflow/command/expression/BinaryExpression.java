package de.dfki.vsm.model.sceneflow.command.expression;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;
import java.util.Vector;

/**
 * @author Gregor Mehlmann
 */
public class BinaryExpression extends AbstractExpression {

    private AbstractExpression mLeftExp;
    private AbstractExpression mRightExp;
    private Operator mOperator;

    public enum Operator {

        And, Or,
        Add, Div, Mul, Sub,
        Eq, Ge, Gt, Le, Lt, Neq,
        Get, Contains, Remove, AddFirst, AddLast
    }

    public BinaryExpression() {
        mLeftExp = null;
        mRightExp = null;
        mOperator = null;
    }

    public BinaryExpression(AbstractExpression left, Operator op, AbstractExpression right) {
        mLeftExp = left;
        mOperator = op;
        mRightExp = right;
    }

    public void setLeftExp(AbstractExpression value) {
        mLeftExp = value;
    }

    public AbstractExpression getLeftExp() {
        return mLeftExp;
    }

    public void setRightExp(AbstractExpression value) {
        mRightExp = value;
    }

    public AbstractExpression getRightExp() {
        return mRightExp;
    }

    public void setOperator(Operator value) {
        mOperator = value;
    }

    public Operator getOperator() {
        return mOperator;
    }

    @Override
    public ExpType getExpType() {
        return ExpType.BINARYEXP;
    }

    @Override
    public String getAbstractSyntax() {
        return "BinaryExp(" + ((mOperator != null)
                ? mOperator.name()
                : "") + "(" + ((mLeftExp != null)
                ? mLeftExp.getAbstractSyntax()
                : "") + "," + ((mRightExp != null)
                ? mRightExp.getAbstractSyntax()
                : "") + "))";
    }

    @Override
    public String getConcreteSyntax() {
        String opString = "";
        String left = (mLeftExp != null)
                ? mLeftExp.getConcreteSyntax()
                : "";
        String right = (mRightExp != null)
                ? mRightExp.getConcreteSyntax()
                : "";

        if (mOperator == null) {
            return "";
        }

        switch (mOperator) {
            case And:
                opString = left + " && " + right;
                break;
            case Or:
                opString = left + " || " + right;
                break;
            case Add:
                opString = left + " + " + right;
                break;
            case Div:
                opString = left + " / " + right;
                break;
            case Mul:
                opString = left + " * " + right;
                break;
            case Sub:
                opString = left + " - " + right;
                break;
            case Eq:
                opString = left + " == " + right;
                break;
            case Ge:
                opString = left + " >= " + right;
                break;
            case Gt:
                opString = left + " > " + right;
                break;
            case Le:
                opString = left + " <= " + right;
                break;
            case Lt:
                opString = left + " < " + right;
                break;
            case Neq:
                opString = left + " != " + right;
                break;
            case Get:
                opString = "Get(" + left + " , " + right + ")";
                break;
            case Contains:
                opString = "Contains(" + left + " , " + right + ")";
                break;
            case Remove:
                opString = "Remove(" + left + " , " + right + ")";
                break;
            case AddFirst:
                opString = "AddFirst(" + left + " , " + right + ")";
                break;
            case AddLast:
                opString = "AddLast(" + left + " , " + right + ")";
                break;
        }
        return opString;
    }

    @Override
    public String getFormattedSyntax() {
        String opString = "";
        String left = (mLeftExp != null)
                ? mLeftExp.getFormattedSyntax()
                : "";
        String right = (mRightExp != null)
                ? mRightExp.getFormattedSyntax()
                : "";

        if (mOperator == null) {
            return "";
        }

        switch (mOperator) {
            case And:
                opString = left + " && " + right;
                break;
            case Or:
                opString = left + " || " + right;
                break;
            case Add:
                opString = left + " + " + right;
                break;
            case Div:
                opString = left + " / " + right;
                break;
            case Mul:
                opString = left + " * " + right;
                break;
            case Sub:
                opString = left + " - " + right;
                break;
            case Eq:
                opString = left + " == " + right;
                break;
            case Ge:
                opString = left + " >= " + right;
                break;
            case Gt:
                opString = left + " > " + right;
                break;
            case Le:
                opString = left + " <= " + right;
                break;
            case Lt:
                opString = left + " < " + right;
                break;
            case Neq:
                opString = left + " != " + right;
                break;
            case Get:
                opString = "Get(" + left + " , " + right + ")";
                break;
            case Contains:
                opString = "Contains(" + left + " , " + right + ")";
                break;
            case Remove:
                opString = "Remove(" + left + " , " + right + ")";
                break;
            case AddFirst:
                opString = "AddFirst(" + left + " , " + right + ")";
                break;
            case AddLast:
                opString = "AddLast(" + left + " , " + right + ")";
                break;
        }
        return opString;
    }

    @Override
    public BinaryExpression getCopy() {
        return new BinaryExpression(mLeftExp.getCopy(), mOperator, mRightExp.getCopy());
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {

        // TODO: Check if operand or expression are null
        out.println("<" + mOperator.name() + ">").push();
        mLeftExp.writeXML(out);
        mRightExp.writeXML(out);
        out.pop().println("</" + mOperator.name() + ">");
    }

    @Override
    public void parseXML(Element element) throws XMLParseError {
        mOperator = Operator.valueOf(element.getTagName());

        final Vector<AbstractExpression> expList = new Vector<AbstractExpression>();

        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(Element element) throws XMLParseError {
                expList.add(AbstractExpression.parse(element));
            }
        });
        mLeftExp = expList.firstElement();
        mRightExp = expList.lastElement();
    }
}
