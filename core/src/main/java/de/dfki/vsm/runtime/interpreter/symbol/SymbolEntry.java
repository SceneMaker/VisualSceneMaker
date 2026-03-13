package de.dfki.vsm.runtime.interpreter.symbol;

import de.dfki.vsm.event.EventDispatcher;
import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.event.event.VariableChangedEvent;
import de.dfki.vsm.runtime.interpreter.error.InterpreterError;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.interpreter.value.EventValue;
import de.dfki.vsm.runtime.interpreter.value.ListValue;
import de.dfki.vsm.runtime.interpreter.value.StructValue;
import de.dfki.vsm.util.cpy.Copyable;
import de.dfki.vsm.util.tpl.Tuple;

public final class SymbolEntry implements Copyable {

    // The Value of The Entry
    private AbstractValue mValue;
    // The Symbol Of The Entry
    private final String mSymbol;
    // The per-project event dispatcher (may be null for legacy/no-dispatcher paths)
    private final EventDispatcher mDispatcher;

    //
    public SymbolEntry(final String symbol, final AbstractValue value) {
        mSymbol = symbol;
        mValue = value;
        mDispatcher = null;
    }

    //
    public SymbolEntry(final String symbol, final AbstractValue value, final EventDispatcher dispatcher) {
        mSymbol = symbol;
        mValue = value;
        mDispatcher = dispatcher;
    }

    private void dispatch(final EventObject event) {
        if (mDispatcher != null) {
            mDispatcher.convey(event);
        }
    }

    //
    public final AbstractValue getValue() {
        return mValue;
    }

    //
    public final AbstractValue write(final AbstractValue value) throws InterpreterError {
        if (value == null) {
            throw new InterpreterError(this, "null value cannot be written to variable '" + mSymbol + "'");
        }

        // Event variables: enqueue instead of replace
        if (mValue.getType() == AbstractValue.Type.EVENT) {
            ((EventValue) mValue).enqueue(value);
            dispatch(new VariableChangedEvent(this,
                    new Tuple<>(mSymbol, mValue.getFormattedSyntax())));
            return mValue;
        }

        // Check if the type is valid
        if (mValue.getType() == value.getType()) {
            // Set the new value
            mValue = value;

            // Send event to dispatcher
            dispatch(new VariableChangedEvent(this,
                    new Tuple(mSymbol, mValue.getFormattedSyntax())));

            //
            return mValue;
        } else {
            throw new InterpreterError(this, value.getConcreteSyntax() + " has wrong type");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final AbstractValue write(final AbstractValue value, final int index) throws InterpreterError {
        try {
            if (mValue.getType() == AbstractValue.Type.LIST) {
                AbstractValue oldValue = ((ListValue) mValue).getValueList().get(index);

                if (oldValue.getType() == value.getType()) {
                    ((ListValue) mValue).getValueList().set(index, value);

                    //
                    dispatch(new VariableChangedEvent(this,
                            new Tuple<>(mSymbol /* .getName() */, mValue.getFormattedSyntax())));

                    //
                    return mValue;
                } else {
                    throw new InterpreterError(this, value.getAbstractSyntax() + " has wrong type");
                }
            } else {
                throw new InterpreterError(this, mValue.getAbstractSyntax() + " is not a list");
            }
        } catch (ClassCastException e) {
            throw new InterpreterError(this, mValue.getAbstractSyntax() + " is not a list");
        } catch (IndexOutOfBoundsException e) {
            throw new InterpreterError(this, mValue.getAbstractSyntax() + " out of bounds");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final AbstractValue write(final AbstractValue value, final String member) throws InterpreterError {
        try {
            if (mValue.getType() == AbstractValue.Type.STRUCT) {
                if (((StructValue) mValue).getValueMap().containsKey(member)) {
                    AbstractValue oldValue = ((StructValue) mValue).getValueMap().get(member);

                    if (oldValue.getType() == value.getType()) {
                        ((StructValue) mValue).getValueMap().put(member, value);

                        //
                        dispatch(new VariableChangedEvent(this,
                                new Tuple<>(mSymbol /* .getName() */, mValue.getFormattedSyntax())));

                        //
                        return mValue;
                    } else {
                        throw new InterpreterError(this, value.getAbstractSyntax() + " has wrong type");
                    }
                } else {
                    throw new InterpreterError(this,
                            member + " does not exist in struct " + mValue.getAbstractSyntax());
                }
            } else {
                throw new InterpreterError(this, mValue.getAbstractSyntax() + " is not a struct");
            }
        } catch (ClassCastException e) {
            throw new InterpreterError(this, mValue.getAbstractSyntax() + " is not a struct");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final AbstractValue read() {
        return mValue;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final AbstractValue read(final int index) throws InterpreterError {
        try {
            if (mValue.getType() == AbstractValue.Type.LIST) {
                return ((ListValue) mValue).getValueList().get(index);
            } else {
                throw new InterpreterError(this, mValue.getAbstractSyntax() + " is not a list");
            }
        } catch (ClassCastException e) {
            throw new InterpreterError(this, mValue.getAbstractSyntax() + " is not a list");
        } catch (IndexOutOfBoundsException e) {
            throw new InterpreterError(this, mValue.getAbstractSyntax() + " out of bounds");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final AbstractValue read(final String member) throws InterpreterError {
        try {
            if (mValue.getType() == AbstractValue.Type.STRUCT) {
                AbstractValue result = ((StructValue) mValue).getValueMap().get(member);

                if (result != null) {
                    return result;
                } else {
                    throw new InterpreterError(this,
                            member + " does not exist in struct " + mValue.getAbstractSyntax());
                }
            } else {
                throw new InterpreterError(this, mValue.getAbstractSyntax() + " is not a struct");
            }
        } catch (ClassCastException e) {
            throw new InterpreterError(this, mValue.getAbstractSyntax() + " is not a struct");
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final SymbolEntry getCopy() {
        return new SymbolEntry(mSymbol, mValue.getCopy(), mDispatcher);
    }
}
