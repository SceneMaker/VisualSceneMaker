package de.dfki.vsm.runtime.values;

/**
 * @author Not me
 */
public class FloatValue extends AbstractValue {
    private final float mValue;

    public FloatValue(float value) {
        mValue = value;
    }

    public float floatValue() {
        return mValue;
    }

    public Type getType() {
        return Type.FLOAT;
    }

    public String getAbstractSyntax() {
        return "FloatValue(" + Float.toString(mValue) + ")";
    }

    // TODO: format character
    public String getConcreteSyntax() {
        return Float.toString(mValue);
    }

    // TODO: format character
    public String getFormattedSyntax() {
        return "#c#" + Float.toString(mValue);
    }

    public Float getValue() {
        return Float.valueOf(mValue);
    }

    public FloatValue getCopy() {
        return new FloatValue(mValue);
    }

    public boolean equalsValue(AbstractValue value) {
        if (value.getType() == Type.FLOAT) {
            return (mValue == ((FloatValue) value).mValue);
        }

        return false;
    }
}
