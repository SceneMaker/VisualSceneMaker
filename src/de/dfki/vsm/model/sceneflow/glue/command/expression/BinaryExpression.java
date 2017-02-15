package de.dfki.vsm.model.sceneflow.glue.command.expression;

import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.util.LinkedList;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class BinaryExpression extends Expression {

    private Expression mLeftExp;
    private Expression mRightExp;
    private Operator mOperator;

    public enum Operator {
        // Logical Expressions
        AndAnd, OrOr, And, Or, Xor,
        // Arithmetric Expressions
        Add, Sub, Mul, Div, Mod,
        // Comparison Expressions
        Eq, Neq, Ge, Gt, Le, Lt
    }

    public BinaryExpression() {
        mLeftExp = null;
        mRightExp = null;
        mOperator = null;
    }

    public BinaryExpression(
            final Expression left,
            final Operator op,
            final Expression right) {
        mLeftExp = left;
        mOperator = op;
        mRightExp = right;
    }

    public final void setLeftExp(final Expression value) {
        mLeftExp = value;
    }

    public final Expression getLeftExp() {
        return mLeftExp;
    }

    public final void setRightExp(final Expression value) {
        mRightExp = value;
    }

    public final Expression getRightExp() {
        return mRightExp;
    }

    public final void setOperator(final Operator value) {
        mOperator = value;
    }

    public final Operator getOperator() {
        return mOperator;
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
        String op = "";
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
            case AndAnd:
                op = left + " && " + right;
                break;
            case OrOr:
                op = left + " || " + right;
                break;
            case And:
                op = left + " & " + right;
                break;
            case Or:
                op = left + " | " + right;
                break;
            case Xor:
                op = left + " ^ " + right;
                break;
            case Add:
                op = left + " + " + right;
                break;
            case Sub:
                op = left + " - " + right;
                break;
            case Mul:
                op = left + " * " + right;
                break;
            case Div:
                op = left + " / " + right;
                break;
            case Mod:
                op = left + " % " + right;
                break;
            case Eq:
                op = left + " == " + right;
                break;
            case Neq:
                op = left + " != " + right;
                break;
            case Ge:
                op = left + " >= " + right;
                break;
            case Gt:
                op = left + " > " + right;
                break;
            case Le:
                op = left + " <= " + right;
                break;
            case Lt:
                op = left + " < " + right;
                break;
        }
        return op;
    }

    @Override
    public String getFormattedSyntax() {
        String op = "";
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
            case AndAnd:
                op = left + " && " + right;
                break;
            case OrOr:
                op = left + " || " + right;
                break;
            case And:
                op = left + " & " + right;
                break;
            case Or:
                op = left + " | " + right;
                break;
            case Xor:
                op = left + " ^ " + right;
                break;
            case Add:
                op = left + " + " + right;
                break;
            case Sub:
                op = left + " - " + right;
                break;
            case Mul:
                op = left + " * " + right;
                break;
            case Div:
                op = left + " / " + right;
                break;
            case Mod:
                op = left + " % " + right;
                break;
            case Eq:
                op = left + " == " + right;
                break;
            case Neq:
                op = left + " != " + right;
                break;
            case Ge:
                op = left + " >= " + right;
                break;
            case Gt:
                op = left + " > " + right;
                break;
            case Le:
                op = left + " <= " + right;
                break;
            case Lt:
                op = left + " < " + right;
                break;
        }
        return op;
    }

    @Override
    public final BinaryExpression getCopy() {
        return new BinaryExpression(
                mLeftExp.getCopy(), mOperator, mRightExp.getCopy());
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) throws XMLWriteError {
        out.println("<" + mOperator.name() + ">").push();
        mLeftExp.writeXML(out);
        mRightExp.writeXML(out);
        out.pop().println("</" + mOperator.name() + ">");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        mOperator = Operator.valueOf(element.getTagName());
        final LinkedList<Expression> expList = new LinkedList();
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public final void run(final Element element) throws XMLParseError {
                expList.add(Expression.parse(element));
            }
        });
        mLeftExp = expList.getFirst();
        mRightExp = expList.getLast();
    }
}
