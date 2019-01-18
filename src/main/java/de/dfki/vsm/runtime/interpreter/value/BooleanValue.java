package de.dfki.vsm.runtime.interpreter.value;

/**
 * @author Gregor Mehlmann
 */
public class BooleanValue extends AbstractValue {
    private final boolean mValue;

    public BooleanValue(boolean value) {
        mValue = value;
    }

    public boolean booleanValue() {
        return mValue;
    }

    public Type getType() {
        return Type.BOOLEAN;
    }

    public String getAbstractSyntax() {
        return "BoolValue(" + mValue + ")";
    }

    public String getConcreteSyntax() {
        return Boolean.toString(mValue);
    }

    public String getFormattedSyntax() {
        return "#c#" + mValue;
    }

    public Boolean getValue() {
        return mValue;
    }

    public BooleanValue getCopy() {
        return new BooleanValue(mValue);
    }

    public boolean equalsValue(AbstractValue value) {
        if (value.getType() == Type.BOOLEAN) {
            return (mValue == ((BooleanValue) value).mValue);
        }

        return false;
    }
}
