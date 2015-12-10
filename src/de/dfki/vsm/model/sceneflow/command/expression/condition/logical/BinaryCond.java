package de.dfki.vsm.model.sceneflow.command.expression.condition.logical;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.expression.condition.Condition;
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
public class BinaryCond extends LogicalCond {
    private Condition mLeftCond;
    private Condition mRightCond;
    private Operator  mOperator;

    public enum Operator { And, Or }

    public BinaryCond() {
        mLeftCond  = null;
        mRightCond = null;
        mOperator  = null;
    }

    public BinaryCond(Condition leftCond, Condition rightCond, Operator operator) {
        mLeftCond  = leftCond;
        mRightCond = rightCond;
        mOperator  = operator;
    }

    public Condition getLeftCond() {
        return mLeftCond;
    }

    public Condition getRightCond() {
        return mRightCond;
    }

    public Operator getOperator() {
        return mOperator;
    }

    public LogicalType getLogicalType() {
        return LogicalType.BIN;
    }

    public String getAbstractSyntax() {
        return ((mOperator != null)
                ? mOperator.name()
                : "") + "(" + ((mLeftCond != null)
                               ? mLeftCond.getAbstractSyntax()
                               : "") + "," + ((mRightCond != null)
                ? mRightCond.getAbstractSyntax()
                : "") + ")";
    }

    public String getConcreteSyntax() {
        return ((mLeftCond != null)
                ? mLeftCond.getConcreteSyntax()
                : "") + ((mOperator != null)
                         ? (mOperator.name().equals("And")
                            ? " && "
                            : " || ")
                         : "") + ((mRightCond != null)
                                  ? mRightCond.getConcreteSyntax()
                                  : "");
    }

    public String getFormattedSyntax() {
        String opString = "";
        String left     = (mLeftCond != null)
                          ? mLeftCond.getFormattedSyntax()
                          : "";
        String right    = (mRightCond != null)
                          ? mRightCond.getFormattedSyntax()
                          : "";

        if (mOperator == null) {
            return "";
        }

        switch (mOperator) {
        case And :
            opString = left + " && " + right;

            break;

        case Or :
            opString = left + " || " + right;

            break;
        }

        return opString;
    }

    public BinaryCond getCopy() {
        return new BinaryCond(mLeftCond.getCopy(), mRightCond.getCopy(), mOperator);
    }

    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<" + mOperator.name() + ">").push();
        mLeftCond.writeXML(out);
        mRightCond.writeXML(out);
        out.pop().println("</" + mOperator.name() + ">");
    }

    public void parseXML(Element element) throws XMLParseError {
        mOperator = Operator.valueOf(element.getTagName());

        final Vector<Condition> condList = new Vector<Condition>();

        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                condList.add(Condition.parse(element));
            }
        });
        mLeftCond  = condList.firstElement();
        mRightCond = condList.lastElement();
    }
}
