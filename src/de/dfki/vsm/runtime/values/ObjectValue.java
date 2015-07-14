package de.dfki.vsm.runtime.values;

/**
 * @author Not me
 */
public class ObjectValue extends AbstractValue {
    private java.lang.Object mValue;

    public ObjectValue() {
        mValue = null;
    }

    public ObjectValue(java.lang.Object value) {
        mValue = value;
    }

    public java.lang.Object objectValue() {
        return mValue;
    }

    @Override
    public Type getType() {
        return Type.OBJECT;
    }

    @Override
    public String getAbstractSyntax() {
        return "ObjectValue(" + ((mValue == null)
                                 ? ""
                                 : mValue.toString()) + ")";
    }

    @Override
    public String getConcreteSyntax() {
        return "ObjectValue(" + ((mValue == null)
                                 ? ""
                                 : mValue.toString()) + ")";
    }

    @Override
    public String getFormattedSyntax() {
        return "ObjectValue(" + ((mValue == null)
                                 ? ""
                                 : mValue.toString()) + ")";
    }

    @Override
    public java.lang.Object getValue() {
        return mValue;
    }

    @Override
    public ObjectValue getCopy() {
        return new ObjectValue(mValue);
    }

    @Override
    public boolean equalsValue(AbstractValue value) {
        if (value.getType() == Type.OBJECT) {
            return mValue.equals(((ObjectValue) value).mValue);
        }

        return false;
    }
}
