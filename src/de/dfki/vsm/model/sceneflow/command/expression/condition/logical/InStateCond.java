package de.dfki.vsm.model.sceneflow.command.expression.condition.logical;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.ios.IOSIndentWriter;

import org.w3c.dom.Element;

/**
 *
 * @author Not me
 */
public class InStateCond extends LogicalCond {
    String mState;

    public InStateCond() {}

    public InStateCond(String state) {
        mState = state;
    }

    public String getState() {
        return mState;
    }

    public LogicalType getLogicalType() {
        return LogicalType.STATE;
    }

    public String getAbstractSyntax() {
        return "In( " + mState + " )";
    }

    public String getConcreteSyntax() {
        return "In( " + mState + " )";
    }

    public String getFormattedSyntax() {
        return "In( " + mState + " )";
    }

    public InStateCond getCopy() {
        return new InStateCond(mState);
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<StateCondition state=\"" + mState + "\"/>");
    }

    public void parseXML(Element element) {
        mState = element.getAttribute("state");
    }
}
