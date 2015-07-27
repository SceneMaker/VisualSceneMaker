package de.dfki.vsm.runtime.values;

/**
 * @author Not me
 */
public class DoubleValue extends AbstractValue {
    private final double mValue;

    public DoubleValue(double value) {
        mValue = value;
    }

    public double doubleValue() {
        return mValue;
    }

    public Type getType() {
        return Type.DOUBLE;
    }

    public String getAbstractSyntax() {
        return "DoubleValue(" + Double.toString(mValue) + ")";
    }

    // TODO: format character
    public String getConcreteSyntax() {
        return Double.toString(mValue);
    }

    // TODO: format character
    public String getFormattedSyntax() {
        return "#c#" + Double.toString(mValue);
    }

    public Double getValue() {
        return Double.valueOf(mValue);
    }

    public DoubleValue getCopy() {
        return new DoubleValue(mValue);
    }

    public boolean equalsValue(AbstractValue value) {
        if (value.getType() == Type.DOUBLE) {
            return (mValue == ((DoubleValue) value).mValue);
        }

        return false;
    }
}
