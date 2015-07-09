package de.dfki.vsm.model.sceneflow.command.expression.condition.logical;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------

import java.util.Vector;

/**
 * A comparision logical condition.
 *
 * @author Not me
 */
public class ComparisionCond extends LogicalCond {
    public static String sID = "ComparisionCond";
    private Expression   mLeftExp;
    private Expression   mRightExp;
    private Operator     mOperator;

    public enum Operator {
        Eq, Ge, Gt, Le, Lt, Neq
    }

    public ComparisionCond() {
        mLeftExp  = null;
        mRightExp = null;
        mOperator = null;
    }

    public ComparisionCond(Expression leftCond, Expression rightCond, Operator operator) {
        mLeftExp  = leftCond;
        mRightExp = rightCond;
        mOperator = operator;
    }

    public Expression getLeftExp() {
        return mLeftExp;
    }

    public Expression getRightExp() {
        return mRightExp;
    }

    public Operator getOperator() {
        return mOperator;
    }

    public LogicalType getLogicalType() {
        return LogicalType.COMP;
    }

    public String getAbstractSyntax() {
        return ((mOperator != null)
                ? mOperator.name()
                : "") + "(" + ((mLeftExp != null)
                               ? mLeftExp.getAbstractSyntax()
                               : "") + "," + ((mRightExp != null)
                ? mRightExp.getAbstractSyntax()
                : "") + ")";
    }

    public String getConcreteSyntax() {
        String out = "( " + ((mLeftExp != null)
                             ? mLeftExp.getConcreteSyntax()
                             : "");

        if (mOperator != null) {
            switch (mOperator) {
            case Eq :
                out += " == ";

                break;

            case Ge :
                out += " >= ";

                break;

            case Gt :
                out += " > ";

                break;

            case Le :
                out += " <= ";

                break;

            case Lt :
                out += " < ";

                break;

            case Neq :
                out += " != ";

                break;
            }
        }

        out += ((mRightExp != null)
                ? mRightExp.getConcreteSyntax()
                : "") + " )";

        return out;
    }

    public String getFormattedSyntax() {
        String out = "( " + ((mLeftExp != null)
                             ? mLeftExp.getConcreteSyntax()
                             : "");

        if (mOperator != null) {
            switch (mOperator) {
            case Eq :
                out += " == ";

                break;

            case Ge :
                out += " >= ";

                break;

            case Gt :
                out += " > ";

                break;

            case Le :
                out += " <= ";

                break;

            case Lt :
                out += " < ";

                break;

            case Neq :
                out += " != ";

                break;
            }
        }

        out += ((mRightExp != null)
                ? mRightExp.getFormattedSyntax()
                : "") + " )";

        return out;
    }

    public ComparisionCond getCopy() {
        return new ComparisionCond(mLeftExp.getCopy(), mRightExp.getCopy(), mOperator);
    }

    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
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
