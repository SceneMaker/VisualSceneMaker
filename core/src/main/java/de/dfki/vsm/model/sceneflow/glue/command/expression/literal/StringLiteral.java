package de.dfki.vsm.model.sceneflow.glue.command.expression.literal;


import de.dfki.vsm.model.sceneflow.glue.command.expression.LiteralExpression;
import de.dfki.vsm.util.TextFormat;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class StringLiteral extends LiteralExpression {

    private String mValue;

    public StringLiteral() {
        mValue = null;
    }

    public StringLiteral(final String value) {
        mValue = value;
    }

    public final String getValue() {
        return mValue;
    }

    public final void setValue(final String value) {
        mValue = value;
    }

    @Override
    public final String getAbstractSyntax() {
        return "StringLiteral(" + getConcreteSyntax() + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return "\"" + mValue + "\"";
    }

    @Override
    public final String getFormattedSyntax() {
        return TextFormat.formatConstantStringLiteral("\"" + mValue + "\"");
    }

    @Override
    public final StringLiteral getCopy() {
        return new StringLiteral(mValue);
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) {
        out.println("<StringLiteral><![CDATA[" + mValue + "]]></StringLiteral>");
    }

    @Override
    public final void parseXML(final Element element) {
        if (element.hasAttribute("value")) {
            mValue = element.getAttribute("value");
        } else {
            mValue = element.getTextContent();
        }
    }
}