package de.dfki.vsm.model.sceneflow.glue.command.expression.literal;

import de.dfki.vsm.model.sceneflow.glue.command.expression.LiteralExpression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class IntLiteral extends LiteralExpression {

    private int mValue;

    public IntLiteral() {
        mValue = Integer.MIN_VALUE;
    }

    public IntLiteral(int value) {
        mValue = value;
    }

    public final int getValue() {
        return mValue;
    }

    public final void setValue(final int value) {
        mValue = value;
    }

    @Override
    public final String getAbstractSyntax() {
        return "IntLiteral(" + getConcreteSyntax() + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return Integer.toString(mValue);
    }

    @Override
    public final String getFormattedSyntax() {
        return "#c#" + Integer.toString(mValue);
    }

    @Override
    public final IntLiteral getCopy() {
        return new IntLiteral(mValue);
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) {
        out.println("<IntLiteral value=\"" + mValue + "\"/>");
    }

    @Override
    public final void parseXML(final Element element) {
        mValue = Integer.valueOf(element.getAttribute("value"));
    }
}