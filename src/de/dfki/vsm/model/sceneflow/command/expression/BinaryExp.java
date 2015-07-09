package de.dfki.vsm.model.sceneflow.command.expression;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------

import java.util.Vector;

/**
 * @author Not me
 */
public class BinaryExp extends Expression {
    private Expression mLeftExp;
    private Expression mRightExp;
    private Operator   mOperator;

    public enum Operator {
        Add, Div, Mul, Sub, Get, Remove, AddFirst, AddLast
    }

    public BinaryExp() {
        mLeftExp  = null;
        mRightExp = null;
        mOperator = null;
    }

    public BinaryExp(Expression left, Operator op, Expression right) {
        mLeftExp  = left;
        mOperator = op;
        mRightExp = right;
    }

    public void setLeftExp(Expression value) {
        mLeftExp = value;
    }

    public Expression getLeftExp() {
        return mLeftExp;
    }

    public void setRightExp(Expression value) {
        mRightExp = value;
    }

    public Expression getRightExp() {
        return mRightExp;
    }

    public void setOperator(Operator value) {
        mOperator = value;
    }

    public Operator getOperator() {
        return mOperator;
    }

    public ExpType getExpType() {
        return ExpType.BIN;
    }

    public String getAbstractSyntax() {
        return "BinaryExp(" + ((mOperator != null)
                               ? mOperator.name()
                               : "") + "(" + ((mLeftExp != null)
                ? mLeftExp.getAbstractSyntax()
                : "") + "," + ((mRightExp != null)
                               ? mRightExp.getAbstractSyntax()
                               : "") + "))";
    }

    public String getConcreteSyntax() {
        String opString = "";
        String left     = (mLeftExp != null)
                          ? mLeftExp.getConcreteSyntax()
                          : "";
        String right    = (mRightExp != null)
                          ? mRightExp.getConcreteSyntax()
                          : "";

        if (mOperator == null) {
            return "";
        }

        switch (mOperator) {
        case Add :
            opString = left + " + " + right;

            break;

        case Div :
            opString = left + " / " + right;

            break;

        case Mul :
            opString = left + " * " + right;

            break;

        case Sub :
            opString = left + " - " + right;

            break;

        case Get :
            opString = "Get(" + left + " , " + right + ")";

            break;

        case Remove :
            opString = "Remove(" + left + " , " + right + ")";

            break;

        case AddFirst :
            opString = "AddFirst(" + left + " , " + right + ")";

            break;

        case AddLast :
            opString = "AddLast(" + left + " , " + right + ")";

            break;
        }

        return opString;
    }

    public String getFormattedSyntax() {
        String opString = "";
        String left     = (mLeftExp != null)
                          ? mLeftExp.getFormattedSyntax()
                          : "";
        String right    = (mRightExp != null)
                          ? mRightExp.getFormattedSyntax()
                          : "";

        if (mOperator == null) {
            return "";
        }

        switch (mOperator) {
        case Add :
            opString = left + " + " + right;

            break;

        case Div :
            opString = left + " / " + right;

            break;

        case Mul :
            opString = left + " * " + right;

            break;

        case Sub :
            opString = left + " - " + right;

            break;

        case Get :
            opString = "Get( " + left + " , " + right + " )";

            break;

        case Remove :
            opString = "Remove( " + left + " , " + right + " )";

            break;

        case AddFirst :
            opString = "AddFirst( " + left + " , " + right + " )";

            break;

        case AddLast :
            opString = "AddLast( " + left + " , " + right + " )";

            break;
        }

        return opString;
    }

    public BinaryExp getCopy() {
        return new BinaryExp(mLeftExp.getCopy(), mOperator, mRightExp.getCopy());
    }

    public void writeXML(IOSIndentWriter out) throws XMLWriteError {

        // TODO: Check if operand or expression are null
        out.println("<" + mOperator.name() + ">").push();
        mLeftExp.writeXML(out);
        mRightExp.writeXML(out);
        out.pop().println("</" + mOperator.name() + ">");
    }

    public void parseXML(Element element) throws XMLParseError {
        mOperator = Operator.valueOf(element.getTagName());

        final Vector<Expression> expList = new Vector<Expression>();

        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                expList.add(Expression.parse(element));
            }
        });
        mLeftExp  = expList.firstElement();
        mRightExp = expList.lastElement();
    }
}
