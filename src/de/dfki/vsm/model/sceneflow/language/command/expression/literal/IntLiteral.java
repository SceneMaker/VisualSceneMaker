package de.dfki.vsm.model.sceneflow.language.command.expression.literal;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.language.command.expression.LiteralExpression;
import de.dfki.vsm.util.ios.IOSIndentWriter;

import org.w3c.dom.Element;

/**
 * An integer constant.
 *
 * @author Not me
 */
public class IntLiteral extends LiteralExpression {
    private int mValue;

    public IntLiteral() {
        mValue = Integer.MIN_VALUE;
    }

    public IntLiteral(int value) {
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

    public IntLiteral getCopy() {
        return new IntLiteral(mValue);
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<Int value=\"" + mValue + "\"/>");
    }

    public void parseXML(Element element) {
        mValue = Integer.valueOf(element.getAttribute("value"));
    }
}
