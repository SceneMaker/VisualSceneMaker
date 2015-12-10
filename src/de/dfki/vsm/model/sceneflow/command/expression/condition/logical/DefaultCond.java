package de.dfki.vsm.model.sceneflow.command.expression.condition.logical;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.expression.condition.Condition;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

/**
 * A default logical condition.
 *
 * @author Not me
 */
public class DefaultCond extends LogicalCond {
    private Condition mCondition;

    public DefaultCond() {
        mCondition = null;
    }

    public DefaultCond(Condition condition) {
        mCondition = condition;
    }

    public Condition getCondition() {
        return mCondition;
    }

    public LogicalType getLogicalType() {
        return LogicalType.DEFAULT;
    }

    public String getAbstractSyntax() {
        return "Default(" + ((mCondition != null)
                             ? mCondition.getAbstractSyntax()
                             : "") + ")";
    }

    public String getConcreteSyntax() {
        return "Default(" + ((mCondition != null)
                             ? mCondition.getConcreteSyntax()
                             : "") + ")";
    }

    public String getFormattedSyntax() {
        return "";
    }

    public DefaultCond getCopy() {
        return new DefaultCond(mCondition.getCopy());
    }

    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<DefaultCondition>").push();

        if (mCondition != null) {
            mCondition.writeXML(out);
        }

        out.pop().println("</DefaultCondition>");
    }

    public void parseXML(Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                mCondition = Condition.parse(element);
            }
        });
    }
}
