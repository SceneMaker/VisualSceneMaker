package de.dfki.vsm.model.sceneflow.command.expression.condition.constant;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.ios.IOSIndentWriter;

import org.w3c.dom.Element;

/**
 * A boolean constant.
 *
 * @author Not me
 */
public class Bool extends Constant {
    private boolean mValue;

    public Bool() {
        mValue = Boolean.FALSE;
    }

    public Bool(boolean value) {
        mValue = value;
    }

    public boolean getValue() {
        return mValue;
    }

    public void setValue(boolean value) {
        mValue = value;
    }

    public ConstType getConstType() {
        return ConstType.BOOL;
    }

    public java.lang.String getAbstractSyntax() {
        return "Bool(" + getConcreteSyntax() + ")";
    }

    public java.lang.String getConcreteSyntax() {
        return Boolean.toString(mValue);
    }

    public java.lang.String getFormattedSyntax() {
        return "#c#" + Boolean.toString(mValue);
    }

    public Bool getCopy() {
        return new Bool(mValue);
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<Bool value=\"" + mValue + "\"/>");
    }

    public void parseXML(Element element) {
        mValue = Boolean.valueOf(element.getAttribute("value"));
    }
}
