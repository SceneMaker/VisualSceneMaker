package de.dfki.vsm.runtime.values;

//~--- JDK imports ------------------------------------------------------------

import java.util.LinkedList;

/**
 * @author Not me
 */
public class ListValue extends AbstractValue {
    private final LinkedList<AbstractValue> mValueList;

    public ListValue(LinkedList<AbstractValue> valueList) {
        mValueList = valueList;
    }

    public LinkedList<AbstractValue> getValueList() {
        return mValueList;
    }

    public boolean isEmpty() {
        return mValueList.isEmpty();
    }

    public LinkedList<AbstractValue> getCopyOfValueList() {
        LinkedList<AbstractValue> valueListCopy = new LinkedList<AbstractValue>();

        for (AbstractValue value : mValueList) {
            valueListCopy.add((AbstractValue) value.getCopy());
        }

        return valueListCopy;
    }

    public Type getType() {
        return Type.LIST;
    }

    @Override
    public String getAbstractSyntax() {
        java.lang.String result = "ListValue(";

        for (int i = 0; i < mValueList.size(); i++) {
            result += mValueList.get(i).getAbstractSyntax();

            if (i < mValueList.size() - 1) {
                result += ",";
            }
        }

        return result + ")";
    }

    @Override
    public String getConcreteSyntax() {
        java.lang.String result = "[";

        for (int i = 0; i < mValueList.size(); i++) {
            result += mValueList.get(i).getConcreteSyntax();

            if (i < mValueList.size() - 1) {
                result += ",";
            }
        }

        return result + "]";
    }

    @Override
    public String getFormattedSyntax() {
        java.lang.String result = "[ ";

        for (int i = 0; i < mValueList.size(); i++) {
            result += mValueList.get(i).getFormattedSyntax();

            if (i < mValueList.size() - 1) {
                result += " , ";
            }
        }

        return result + " ]";
    }

    public Object[] getValue() {
        Object[] objArr = new Object[mValueList.size()];

        for (int i = 0; i < mValueList.size(); i++) {
            objArr[i] = mValueList.get(i).getValue();
        }

        return objArr;
    }

    public ListValue getCopy() {
        return new ListValue(getCopyOfValueList());
    }

    public boolean equalsValue(AbstractValue value) {
        return false;
    }
}
