package de.dfki.vsm.model.sceneflow.language.command.expression.literal;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.language.command.expression.LiteralExpression;
import de.dfki.vsm.util.ios.IOSIndentWriter;

import org.w3c.dom.Element;

/**
 * A floating point constant.
 *
 * @author Not me
 */
public class FloatLiteral extends LiteralExpression {
    private float mValue;

    public FloatLiteral() {
        mValue = java.lang.Float.MIN_VALUE;
    }

    public FloatLiteral(float value) {
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

    public FloatLiteral getCopy() {
        return new FloatLiteral(mValue);
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<Float value=\"" + mValue + "\"/>");
    }

    public void parseXML(Element element) {
        mValue = java.lang.Float.valueOf(element.getAttribute("value"));
    }
}
