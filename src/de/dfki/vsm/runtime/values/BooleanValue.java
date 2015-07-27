package de.dfki.vsm.runtime.values;

/**
 * @author Not me
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
        return "BoolValue(" + Boolean.toString(mValue) + ")";
    }

    public String getConcreteSyntax() {
        return Boolean.toString(mValue);
    }

    public String getFormattedSyntax() {
        return "#c#" + Boolean.toString(mValue);
    }

    public Boolean getValue() {
        return Boolean.valueOf(mValue);
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
