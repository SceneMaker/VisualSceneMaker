package de.dfki.vsm.model.sceneflow.command.expression.condition.constant;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.ios.IOSIndentWriter;

import org.w3c.dom.Element;

/**
 * A floating point constant.
 *
 * @author Not me
 */
public class Float extends Constant {
    private float mValue;

    public Float() {
        mValue = java.lang.Float.MIN_VALUE;
    }

    public Float(float value) {
        mValue = value;
    }

    public float getValue() {
        return mValue;
    }

    public void setValue(float value) {
        mValue = value;
    }

    public ConstType getConstType() {
        return ConstType.FLOAT;
    }

    public java.lang.String getAbstractSyntax() {
        return "Float(" + getConcreteSyntax() + ")";
    }

    public java.lang.String getConcreteSyntax() {
        return java.lang.Float.toString(mValue);
    }

    public java.lang.String getFormattedSyntax() {
        return "#c#" + java.lang.Float.toString(mValue);
    }

    public Float getCopy() {
        return new Float(mValue);
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<Float value=\"" + mValue + "\"/>");
    }

    public void parseXML(Element element) {
        mValue = java.lang.Float.valueOf(element.getAttribute("value"));
    }
}
