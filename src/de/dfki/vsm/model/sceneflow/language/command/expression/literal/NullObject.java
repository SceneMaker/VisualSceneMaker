package de.dfki.vsm.model.sceneflow.language.command.expression.literal;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.language.command.expression.LiteralExpression;
import de.dfki.vsm.util.ios.IOSIndentWriter;

import org.w3c.dom.Element;

/**
 * A boolean constant.
 *
 * @author Not me
 */
public class NullObject extends LiteralExpression {
    private java.lang.Object mValue;

    public NullObject() {
        mValue = null;
    }

    public NullObject(java.lang.Object value) {
        mValue = value;
    }

    public java.lang.Object getValue() {
        return mValue;
    }

    public void setValue(java.lang.Object value) {
        mValue = value;
    }

    @Override
    public ConstType getConstType() {
        return ConstType.NULL;
    }

    @Override
    public java.lang.String getAbstractSyntax() {
        return "Null(" + getConcreteSyntax() + ")";
    }

    @Override
    public java.lang.String getConcreteSyntax() {
        if (mValue != null) {
            return mValue.toString();
        }

        return "null";
    }

    @Override
    public java.lang.String getFormattedSyntax() {
        return "#c#" + mValue;
    }

    @Override
    public NullObject getCopy() {
        return new NullObject(mValue);
    }

    @Override
    public void writeXML(IOSIndentWriter out) {
        out.println("<Null value=\"" + mValue + "\"/>");
    }

    @Override
    public void parseXML(Element element) {

        // mValue = element.getAttribute("value");
    }
}
