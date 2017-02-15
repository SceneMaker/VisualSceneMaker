package de.dfki.vsm.model.sceneflow.glue.command.expression.literal;

import de.dfki.vsm.model.sceneflow.glue.command.expression.LiteralExpression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class FloatLiteral extends LiteralExpression {

    private float mValue;

    public FloatLiteral() {
        mValue = Float.MIN_VALUE;
    }

    public FloatLiteral(final float value) {
        mValue = value;
    }

    public final float getValue() {
        return mValue;
    }

    public final void setValue(final float value) {
        mValue = value;
    }

    @Override
    public final String getAbstractSyntax() {
        return "FloatLiteral(" + getConcreteSyntax() + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return Float.toString(mValue);
    }

    @Override
    public final String getFormattedSyntax() {
        return "#c#" + Float.toString(mValue);
    }

    @Override
    public final FloatLiteral getCopy() {
        return new FloatLiteral(mValue);
    }

    @Override
    public void writeXML(final IOSIndentWriter out) {
        out.println("<FloatLiteral value=\"" + mValue + "\"/>");
    }

    @Override
    public void parseXML(final Element element) {
        mValue = Float.valueOf(element.getAttribute("value"));
    }
}
