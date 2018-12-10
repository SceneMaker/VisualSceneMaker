package de.dfki.vsm.runtime.interpreter.value;

import de.dfki.vsm.util.cpy.Copyable;

/**
 * @author Gregor Mehlmann
 */
public abstract class AbstractValue implements Copyable {

    public enum Type {
        BYTE, SHORT, INT, LONG, FLOAT, DOUBLE, BOOLEAN, CHAR, STRING, LIST, STRUCT, VOID, OBJECT
    };

    public abstract String getAbstractSyntax();

    public abstract String getConcreteSyntax();

    public abstract String getFormattedSyntax();

    public abstract Type getType();

    public abstract Object getValue();

    public abstract boolean equalsValue(AbstractValue value);

    @Override
    public abstract AbstractValue getCopy();
}
