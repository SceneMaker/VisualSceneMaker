package de.dfki.vsm.runtime.values;

/**
 *
 * @author Not me
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
        return "LongValue(" + Long.toString(mValue) + ")";
    }

    public String getConcreteSyntax() {
        return Long.toString(mValue);
    }

    public String getFormattedSyntax() {
        return "#c#" + Long.toString(mValue);
    }

    public Long getValue() {
        return Long.valueOf(mValue);
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
