package de.dfki.vsm.model.sceneflow.command.expression.condition.logical;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.expression.condition.Condition;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

/**
 * A unary logical condition
 *
 * @author Not me
 */
public class UnaryCond extends LogicalCond {
    private Condition mCondition;
    private Operator  mOperator;

    public enum Operator { Not }

    public UnaryCond() {
        mCondition = null;
        mOperator  = null;
    }

    public UnaryCond(Condition condition, Operator operator) {
        mCondition = condition;
        mOperator  = operator;
    }

    public Condition getCondition() {
        return mCondition;
    }

    public Operator getOperator() {
        return mOperator;
    }

    public LogicalType getLogicalType() {
        return LogicalType.UN;
    }

    public String getAbstractSyntax() {
        return ((mOperator != null)
                ? mOperator.name()
                : "") + "(" + ((mCondition != null)
                               ? mCondition.getAbstractSyntax()
                               : "") + ")";
    }

    public String getConcreteSyntax() {
        return ((mOperator != null)
                ? "!"
                : "") + ((mCondition != null)
                         ? mCondition.getConcreteSyntax()
                         : "");
    }

    public String getFormattedSyntax() {
        return "";
    }

    public UnaryCond getCopy() {
        return new UnaryCond(mCondition.getCopy(), mOperator);
    }

    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<" + mOperator.name() + ">").push();
        mCondition.writeXML(out);
        out.pop().println("</" + mOperator.name() + ">");
    }

    public void parseXML(Element element) throws XMLParseError {
        mOperator = Operator.valueOf(element.getTagName());
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                mCondition = Condition.parse(element);
            }
        });
    }
}
