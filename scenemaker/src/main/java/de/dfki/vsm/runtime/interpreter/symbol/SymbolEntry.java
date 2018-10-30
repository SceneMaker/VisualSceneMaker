package de.dfki.vsm.runtime.interpreter.symbol;

import de.dfki.vsm.editor.event.VariableChangedEvent;
import de.dfki.vsm.runtime.interpreter.error.InterpreterError;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.interpreter.value.ListValue;
import de.dfki.vsm.runtime.interpreter.value.StructValue;
import de.dfki.vsm.util.cpy.Copyable;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.tpl.TPLTuple;

public final class SymbolEntry implements Copyable {

    // The Value of The Entry
    private AbstractValue mValue;
    // The Symbol Of The Entry
    private final String mSymbol;

    //
    public SymbolEntry(final String symbol, final AbstractValue value) {
        mSymbol = symbol;
        mValue = value;
    }

    //
    public final AbstractValue getValue() {
        return mValue;
    }

    //
    public final AbstractValue write(final AbstractValue value) throws InterpreterError {

        // Check if the type is valid
        if (mValue.getType() == value.getType()) {
            // Set the new value
            mValue = value;

            // Send event to dispatcher
            EventDispatcher.getInstance().convey(new VariableChangedEvent(this,
                    new TPLTuple(mSymbol, mValue.getFormattedSyntax())));

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
                    EventDispatcher.getInstance().convey(new VariableChangedEvent(this,
                            new TPLTuple<String, String>(mSymbol /* .getName() */, mValue.getFormattedSyntax())));

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
                        EventDispatcher.getInstance().convey(new VariableChangedEvent(this,
                                new TPLTuple<String, String>(mSymbol /* .getName() */, mValue.getFormattedSyntax())));

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
        return new SymbolEntry(mSymbol, mValue.getCopy());
    }
}
