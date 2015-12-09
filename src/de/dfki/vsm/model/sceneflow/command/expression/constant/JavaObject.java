package de.dfki.vsm.model.sceneflow.command.expression.constant;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.ios.IOSIndentWriter;

import org.w3c.dom.Element;

/**
 * A boolean constant.
 *
 * @author Not me
 */
public class JavaObject extends Constant {
    private java.lang.Object mValue;

    public JavaObject() {
        mValue = null;
    }

    public JavaObject(java.lang.Object value) {
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
        return ConstType.OBJECT;
    }

    @Override
    public java.lang.String getAbstractSyntax() {
        return "Object(" + getConcreteSyntax() + ")";
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
    public JavaObject getCopy() {
        return new JavaObject(mValue);
    }

    @Override
    public void writeXML(IOSIndentWriter out) {
        out.println("<Object value=\"" + mValue + "\"/>");
    }

    @Override
    public void parseXML(Element element) {

        // mValue = element.getAttribute("value");
    }
}
