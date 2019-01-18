package de.dfki.vsm.runtime.interpreter.value;

//~--- JDK imports ------------------------------------------------------------

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * @author Gregor Mehlmann
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
        HashMap<java.lang.String, AbstractValue> valueMapCopy = new HashMap<>();

        for (Map.Entry<String, AbstractValue> stringAbstractValueEntry : mValueMap.entrySet()) {
            Map.Entry pairs = (Map.Entry) stringAbstractValueEntry;
            String name = (String) pairs.getKey();
            AbstractValue value = (AbstractValue) pairs.getValue();

            valueMapCopy.put(name, (AbstractValue) value.getCopy());
        }

        return valueMapCopy;
    }

    public Type getType() {
        return Type.STRUCT;
    }

    public String getAbstractSyntax() {
        StringBuilder result = new StringBuilder("StructValue(");

        for (Map.Entry<String, AbstractValue> stringAbstractValueEntry : mValueMap.entrySet()) {
            Map.Entry pairs = (Map.Entry) stringAbstractValueEntry;
            String name = (String) pairs.getKey();
            AbstractValue value = (AbstractValue) pairs.getValue();

            result.append("(").append(name).append(",").append(value.getAbstractSyntax()).append(")");
        }

        return result + ")";
    }

    public String getConcreteSyntax() {
        StringBuilder result = new StringBuilder("{");
        Iterator         it     = mValueMap.entrySet().iterator();

        while (it.hasNext()) {
            Map.Entry        pairs = (Map.Entry) it.next();
            java.lang.String name  = (java.lang.String) pairs.getKey();
            AbstractValue    value = (AbstractValue) pairs.getValue();

            result.append(name).append("=").append(value.getConcreteSyntax());

            if (it.hasNext()) {
                result.append(" , ");
            }
        }

        return result + " }";
    }

    public String getFormattedSyntax() {
        StringBuilder result = new StringBuilder("{ ");
        Iterator         it     = mValueMap.entrySet().iterator();

        while (it.hasNext()) {
            Map.Entry        pairs = (Map.Entry) it.next();
            java.lang.String name  = (java.lang.String) pairs.getKey();
            AbstractValue    value = (AbstractValue) pairs.getValue();

            result.append(name).append(" = ").append(value.getFormattedSyntax());

            if (it.hasNext()) {
                result.append(" , ");
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
