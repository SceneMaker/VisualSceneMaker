package de.dfki.vsm.runtime.interpreter.value;

import de.dfki.vsm.util.TextFormat;

/**
 * @author Gregor Mehlmann
 */
public class StringValue extends AbstractValue {

    private final String mValue;

    public StringValue(String value) {
        mValue = value;
    }

    @Override
    public Type getType() {
        return Type.STRING;
    }

    @Override
    public String getAbstractSyntax() {
        return "StringValue(" + mValue + ")";
    }

    @Override
    public String getConcreteSyntax() {
        return "\"" + mValue + "\"";
    }

    @Override
    public String getFormattedSyntax() {
        return TextFormat.formatConstantStringLiteral("\"" + mValue + "\"");
    }

    @Override
    public String getValue() {
        return mValue;
    }

    @Override
    public StringValue getCopy() {
        return new StringValue(mValue);
    }

    @Override
    public boolean equalsValue(AbstractValue value) {
        if (value.getType() == Type.STRING) {
            return mValue.equals(((StringValue) value).mValue);
        }

        return false;
    }
}
