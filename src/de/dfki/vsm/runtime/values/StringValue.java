package de.dfki.vsm.runtime.values;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.TextFormat;

/**
 * @author Not me
 */
public class StringValue extends AbstractValue {
    private final String mValue;

    public StringValue(String value) {
        mValue = value;
    }

    public Type getType() {
        return Type.STRING;
    }

    public String getAbstractSyntax() {
        return "StringValue(" + mValue + ")";
    }

    public String getConcreteSyntax() {
        return "\"" + mValue + "\"";
    }

    public String getFormattedSyntax() {
        return TextFormat.formatConstantStringLiteral("\"" + mValue + "\"");
    }

    public String getValue() {
        return mValue;
    }

    public StringValue getCopy() {
        return new StringValue(mValue);
    }

    public boolean equalsValue(AbstractValue value) {
        if (value.getType() == Type.STRING) {
            return mValue.equals(((StringValue) value).mValue);
        }

        return false;
    }
}
