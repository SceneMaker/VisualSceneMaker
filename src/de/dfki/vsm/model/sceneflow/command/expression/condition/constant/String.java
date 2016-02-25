package de.dfki.vsm.model.sceneflow.command.expression.condition.constant;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.util.TextFormat;
import de.dfki.vsm.util.ios.IOSIndentWriter;

import org.w3c.dom.Element;

/**
 * A string constant.
 *
 * @author Not me
 */
public class String extends Constant {

    private java.lang.String mValue;

    public String() {
        mValue = null;
    }

    public String(java.lang.String value) {
        mValue = value;
    }

    public java.lang.String getValue() {
        return mValue;
    }

    public void setValue(java.lang.String value) {
        mValue = value;
    }

    @Override
    public ConstType getConstType() {
        return ConstType.STRING;
    }

    @Override
    public java.lang.String getAbstractSyntax() {
        return "String(" + getConcreteSyntax() + ")";
    }

    @Override
    public java.lang.String getConcreteSyntax() {
        return "\"" + mValue + "\"";
    }

    @Override
    public java.lang.String getFormattedSyntax() {
        return TextFormat.formatConstantStringLiteral("\"" + mValue + "\"");
    }

    @Override
    public String getCopy() {
        return new String(mValue);
    }

    @Override
    public void writeXML(IOSIndentWriter out) {
        out.print("<String><![CDATA[" + mValue + "]]></String>");
        //value=\"" + mValue + "\"/>");
    }

    @Override
    public void parseXML(Element element) {
        if (element.hasAttribute("value")) {
            mValue = element.getAttribute("value");
        } else {
            mValue = element.getTextContent();
        }
    }

}
