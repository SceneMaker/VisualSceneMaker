package de.dfki.vsm.runtime.values;

/**
 * @author Not me
 */
public class IntValue extends AbstractValue {
    private final int mValue;

    public IntValue(int value) {
        mValue = value;
    }

    public int intValue() {
        return mValue;
    }

    public Type getType() {
        return Type.INT;
    }

    public String getAbstractSyntax() {
        return "IntValue(" + Integer.toString(mValue) + ")";
    }

    public String getConcreteSyntax() {
        return Integer.toString(mValue);
    }

    public String getFormattedSyntax() {
        return "#c#" + Integer.toString(mValue);
    }

    public Integer getValue() {
        return Integer.valueOf(mValue);
    }

    public IntValue getCopy() {
        return new IntValue(mValue);
    }

    public boolean equalsValue(AbstractValue value) {
        if (value.getType() == Type.INT) {
            return (mValue == ((IntValue) value).mValue);
        }

        return false;
    }
}
