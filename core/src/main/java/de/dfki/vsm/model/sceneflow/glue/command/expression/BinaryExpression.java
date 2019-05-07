package de.dfki.vsm.model.sceneflow.glue.command.expression;

import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.jetbrains.annotations.NotNull;
import org.w3c.dom.Element;

import java.util.LinkedList;


/**
 * @author Gregor Mehlmann
 */
public final class BinaryExpression extends Expression {

    private Expression mLeftExp;
    private Expression mRightExp;
    private BinaryOp mOperator;

    public enum BinaryOp {
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
            final BinaryOp op,
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

    public final void setOperator(final BinaryOp value) {
        mOperator = value;
    }

    public final BinaryOp getOperator() {
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
        op = joinWithOperator(left, right);
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
        op += joinWithOperator(left, right);
        return op;
    }

    @NotNull
    private String joinWithOperator(String left, String right) {
        switch (mOperator) {
            case AndAnd:
                return left + " && " + right;
            case OrOr:
                return left + " || " + right;
            case And:
                return left + " & " + right;
            case Or:
                return left + " | " + right;
            case Xor:
                return left + " ^ " + right;
            case Add:
                return left + " + " + right;
            case Sub:
                return left + " - " + right;
            case Mul:
                return left + " * " + right;
            case Div:
                return left + " / " + right;
            case Mod:
                return left + " % " + right;
            case Eq:
                return left + " == " + right;
            case Neq:
                return left + " != " + right;
            case Ge:
                return left + " >= " + right;
            case Gt:
                return left + " > " + right;
            case Le:
                return left + " <= " + right;
            case Lt:
                return left + " < " + right;
            default:
                throw new IllegalStateException("Unexpected value: " + mOperator);
        }
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
        mOperator = BinaryOp.valueOf(element.getTagName());
        final LinkedList<Expression> expList = new LinkedList<>();
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
