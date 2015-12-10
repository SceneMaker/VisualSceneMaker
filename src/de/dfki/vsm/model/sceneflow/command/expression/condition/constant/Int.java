package de.dfki.vsm.model.sceneflow.command.expression.condition.constant;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.ios.IOSIndentWriter;

import org.w3c.dom.Element;

/**
 * An integer constant.
 *
 * @author Not me
 */
public class Int extends Constant {
    private int mValue;

    public Int() {
        mValue = Integer.MIN_VALUE;
    }

    public Int(int value) {
        mValue = value;
    }

    public int getValue() {
        return mValue;
    }

    public void setValue(int value) {
        mValue = value;
    }

    public ConstType getConstType() {
        return ConstType.INT;
    }

    public java.lang.String getAbstractSyntax() {
        return "Int(" + getConcreteSyntax() + ")";
    }

    public java.lang.String getConcreteSyntax() {
        return Integer.toString(mValue);
    }

    public java.lang.String getFormattedSyntax() {
        return "#c#" + Integer.toString(mValue);
    }

    public Int getCopy() {
        return new Int(mValue);
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<Int value=\"" + mValue + "\"/>");
    }

    public void parseXML(Element element) {
        mValue = Integer.valueOf(element.getAttribute("value"));
    }
}
