package de.dfki.vsm.model.sceneflow.glue.command.expression.literal;

import de.dfki.vsm.model.sceneflow.glue.command.expression.LiteralExpression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class BoolLiteral extends LiteralExpression {

    private boolean mValue;

    public BoolLiteral() {
        mValue = false;
    }

    public BoolLiteral(final boolean value) {
        mValue = value;
    }

    public final boolean getValue() {
        return mValue;
    }

    public final void setValue(final boolean value) {
        mValue = value;
    }

    @Override
    public final String getAbstractSyntax() {
        return "BoolLiteral(" + getConcreteSyntax() + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return Boolean.toString(mValue);
    }

    @Override
    public final String getFormattedSyntax() {
        return "#c#" + Boolean.toString(mValue);
    }

    @Override
    public final BoolLiteral getCopy() {
        return new BoolLiteral(mValue);
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) {
        out.println("<BoolLiteral value=\"" + mValue + "\"/>");
    }

    @Override
    public final void parseXML(final Element element) {
        mValue = Boolean.valueOf(element.getAttribute("value"));
    }
}
