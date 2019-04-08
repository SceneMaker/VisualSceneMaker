package de.dfki.vsm.runtime.interpreter.value;

/**
 *
 * @author Gregor Mehlmann
 */
public class LongValue extends AbstractValue {
    private final long mValue;

    public LongValue(long value) {
        mValue = value;
    }

    public long longValue() {
        return mValue;
    }

    public Type getType() {
        return Type.LONG;
    }

    public String getAbstractSyntax() {
        return "LongValue(" + mValue + ")";
    }

    public String getConcreteSyntax() {
        return Long.toString(mValue);
    }

    public String getFormattedSyntax() {
        return "#c#" + mValue;
    }

    public Long getValue() {
        return mValue;
    }

    public LongValue getCopy() {
        return new LongValue(mValue);
    }

    public boolean equalsValue(AbstractValue value) {
        if (value.getType() == Type.LONG) {
            return (mValue == ((LongValue) value).mValue);
        }

        return false;
    }
}
