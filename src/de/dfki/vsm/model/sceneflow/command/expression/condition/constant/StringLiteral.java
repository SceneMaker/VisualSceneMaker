package de.dfki.vsm.model.sceneflow.command.expression.condition.constant;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.util.TextFormat;
import de.dfki.vsm.util.ios.IOSIndentWriter;

import org.w3c.dom.Element;

/**
 * A string constant.
 *
 * @author Gregor Mehlmann
 */
public class StringLiteral extends LiteralExpression {

    private java.lang.String mValue;

    public StringLiteral() {
        mValue = null;
    }

    public StringLiteral(java.lang.String value) {
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
    public StringLiteral getCopy() {
        return new StringLiteral(mValue);
    }

    @Override
    public void writeXML(IOSIndentWriter out) {
        out.println("<String><![CDATA[" + mValue + "]]></String>");
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
