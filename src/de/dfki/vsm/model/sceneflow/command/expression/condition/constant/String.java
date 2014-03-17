package de.dfki.vsm.model.sceneflow.command.expression.condition.constant;

import de.dfki.vsm.util.TextFormat;
import de.dfki.vsm.util.ios.IndentWriter;
import org.w3c.dom.Element;

/**
 * A string constant.
 *
 * @author Gregor Mehlmann
 */
public class String extends Constant {

    private java.lang.String mValue;

    public String(java.lang.String value) {
        mValue = value;
    }

    public String() {
        mValue = null;
    }

    public java.lang.String getValue() {
        return mValue;
    }

    public void setValue(java.lang.String value) {
        mValue = value;
    }

    public ConstType getConstType() {
        return ConstType.STRING;
    }

    public java.lang.String getAbstractSyntax() {
        return "String(" + getConcreteSyntax() + ")";
    }

    public java.lang.String getConcreteSyntax() {
        return "\"" + mValue + "\"";
    }

    public java.lang.String getFormattedSyntax() {
        return TextFormat.formatConstantStringLiteral("\"" + mValue + "\"");
    }

    public String getCopy() {
        return new String(mValue);
    }

    public void writeXML(IndentWriter out) {
        out.println("<String value=\"" + mValue + "\"/>");
    }

    public void parseXML(Element element) {
        mValue = element.getAttribute("value");
    }
}
