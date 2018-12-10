package de.dfki.vsm.model.sceneflow.glue.command.expression;

import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.util.LinkedList;
import org.w3c.dom.Element;

// Static Imports
/*
import de.dfki.vsm.model.sceneflow.glue.command.invocation.PlayActivity.Mode;
import static de.dfki.vsm.model.sceneflow.glue.command.invocation.PlayActivity.Mode.Sequential;
import static de.dfki.vsm.model.sceneflow.glue.command.invocation.PlayActivity.Mode.Concurrent;
import static de.dfki.vsm.model.sceneflow.glue.command.expression.UnaryExpression.Operator.Neg;
import static de.dfki.vsm.model.sceneflow.glue.command.expression.UnaryExpression.Operator.Not;
import static de.dfki.vsm.model.sceneflow.glue.command.expression.UnaryExpression.Operator.Lnot;
import static de.dfki.vsm.model.sceneflow.glue.command.expression.UnaryExpression.Operator.Inc;
import static de.dfki.vsm.model.sceneflow.glue.command.expression.UnaryExpression.Operator.Dec;
import static de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression.Operator.AndAnd;
import static de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression.Operator.OrOr;
import static de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression.Operator.And;
import static de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression.Operator.Or;
import static de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression.Operator.Xor;
import static de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression.Operator.Add;
import static de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression.Operator.Sub;
import static de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression.Operator.Mul;
import static de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression.Operator.Div;
import static de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression.Operator.Mod;
import static de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression.Operator.Eq;
import static de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression.Operator.Neq;
import static de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression.Operator.Ge;
import static de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression.Operator.Gt;
import static de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression.Operator.Le;
import static de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression.Operator.Lt;
*/

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
        mOperator = BinaryOp.valueOf(element.getTagName());
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
