package de.dfki.vsm.runtime.values;

//~--- JDK imports ------------------------------------------------------------

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * @author Not me
 */
public class StructValue extends AbstractValue {
    private final HashMap<java.lang.String, AbstractValue> mValueMap;

    public StructValue(HashMap<java.lang.String, AbstractValue> valueMap) {
        mValueMap = valueMap;
    }

    public HashMap<java.lang.String, AbstractValue> getValueMap() {
        return mValueMap;
    }

    public HashMap<java.lang.String, AbstractValue> getCopyOfValueMap() {
        HashMap<java.lang.String, AbstractValue> valueMapCopy = new HashMap<java.lang.String, AbstractValue>();
        Iterator                                 it           = mValueMap.entrySet().iterator();

        while (it.hasNext()) {
            Map.Entry        pairs = (Map.Entry) it.next();
            java.lang.String name  = (java.lang.String) pairs.getKey();
            AbstractValue    value = (AbstractValue) pairs.getValue();

            valueMapCopy.put(name, (AbstractValue) value.getCopy());
        }

        return valueMapCopy;
    }

    public Type getType() {
        return Type.STRUCT;
    }

    public String getAbstractSyntax() {
        java.lang.String result = "StructValue(";
        Iterator         it     = mValueMap.entrySet().iterator();

        while (it.hasNext()) {
            Map.Entry        pairs = (Map.Entry) it.next();
            java.lang.String name  = (java.lang.String) pairs.getKey();
            AbstractValue    value = (AbstractValue) pairs.getValue();

            result += "(" + name + "," + value.getAbstractSyntax() + ")";
        }

        return result + ")";
    }

    public String getConcreteSyntax() {
        java.lang.String result = "{";
        Iterator         it     = mValueMap.entrySet().iterator();

        while (it.hasNext()) {
            Map.Entry        pairs = (Map.Entry) it.next();
            java.lang.String name  = (java.lang.String) pairs.getKey();
            AbstractValue    value = (AbstractValue) pairs.getValue();

            result += name + "=" + value.getConcreteSyntax();

            if (it.hasNext()) {
                result += " , ";
            }
        }

        return result + " }";
    }

    public String getFormattedSyntax() {
        java.lang.String result = "{ ";
        Iterator         it     = mValueMap.entrySet().iterator();

        while (it.hasNext()) {
            Map.Entry        pairs = (Map.Entry) it.next();
            java.lang.String name  = (java.lang.String) pairs.getKey();
            AbstractValue    value = (AbstractValue) pairs.getValue();

            result += name + " = " + value.getFormattedSyntax();

            if (it.hasNext()) {
                result += " , ";
            }
        }

        return result + " }";
    }

    public Object[][] getValue() {
        Object[][] objArr = new /* Value */ Object[mValueMap.size()][2];
        Iterator   it     = mValueMap.entrySet().iterator();
        int        index  = 0;

        while (it.hasNext()) {
            Map.Entry        pairs = (Map.Entry) it.next();
            java.lang.String name  = (java.lang.String) pairs.getKey();
            AbstractValue    value = (AbstractValue) pairs.getValue();

            objArr[index][0] = name;
            objArr[index][1] = value.getValue();
            index++;
        }

        return objArr;
    }

    public StructValue getCopy() {
        return new StructValue(getCopyOfValueMap());
    }

    public boolean equalsValue(AbstractValue value) {
        if (value instanceof StructValue) {
            boolean result = this.getValueMap().equals(((StructValue) value).getValueMap());

            return result;
        } else {
            return false;
        }
    }
}
