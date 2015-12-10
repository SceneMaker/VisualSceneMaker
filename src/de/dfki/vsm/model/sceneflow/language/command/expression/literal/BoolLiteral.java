package de.dfki.vsm.model.sceneflow.language.command.expression.literal;

import de.dfki.vsm.model.sceneflow.language.command.expression.LiteralExpression;
import de.dfki.vsm.util.ios.IOSIndentWriter;

import org.w3c.dom.Element;

/**
 * A boolean constant.
 *
 * @author Gregor Mehlmann
 */
public class BoolLiteral extends LiteralExpression {

    private boolean mValue;

    public BoolLiteral() {
        mValue = Boolean.FALSE;
    }

    public BoolLiteral(boolean value) {
        mValue = value;
    }

    public boolean getValue() {
        return mValue;
    }

    public void setValue(boolean value) {
        mValue = value;
    }

    @Override
    public ConstType getConstType() {
        return ConstType.BOOL;
    }

    @Override
    public java.lang.String getAbstractSyntax() {
        return "Bool(" + getConcreteSyntax() + ")";
    }

    @Override
    public java.lang.String getConcreteSyntax() {
        return Boolean.toString(mValue);
    }

    @Override
    public java.lang.String getFormattedSyntax() {
        return "#c#" + Boolean.toString(mValue);
    }

    @Override
    public BoolLiteral getCopy() {
        return new BoolLiteral(mValue);
    }

    @Override
    public void writeXML(IOSIndentWriter out) {
        out.println("<Bool value=\"" + mValue + "\"/>");
    }

    @Override
    public void parseXML(Element element) {
        mValue = Boolean.valueOf(element.getAttribute("value"));
    }
}
