package de.dfki.vsm.runtime.interpreter;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.runtime.interpreter.error.InterpreterError;
import de.dfki.vsm.runtime.interpreter.symbol.SymbolTable;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.util.cpy.Copyable;

//~--- JDK imports ------------------------------------------------------------

import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public class Environment implements Copyable {
    private final LinkedList<SymbolTable> mSymbolTableList;

    public Environment() {
        mSymbolTableList = new LinkedList<SymbolTable>();
    }

    public Environment(LinkedList<SymbolTable> symbolTableList) {
        mSymbolTableList = symbolTableList;
    }

    public LinkedList<SymbolTable> getCopyOfSymbolTableList() {
        LinkedList<SymbolTable> copy = new LinkedList<SymbolTable>();

        for (SymbolTable table : mSymbolTableList) {
            copy.add(table);
        }

        return copy;
    }

    public SymbolTable getFirst() {
        return mSymbolTableList.getFirst();
    }

    public SymbolTable pop() {
        return mSymbolTableList.removeFirst();
    }

    public void push() {
        mSymbolTableList.addFirst(new SymbolTable());
    }

    public Environment getCopy() {
        return new Environment(getCopyOfSymbolTableList());
    }

    public void create(String symbol, AbstractValue value) throws InterpreterError {
        if (mSymbolTableList.isEmpty()) {
            throw new InterpreterError(symbol, "Runtime Error: Variable '" + symbol + "' cannot be declared.");
        }

        for (SymbolTable symbolTable : mSymbolTableList) {
            if (symbolTable.contains(symbol)) {
                throw new InterpreterError(symbol, "Runtime Error: Variable '" + symbol + "' is already defined.");
            }
        }

        mSymbolTableList.getFirst().create(symbol, value);
    }

    public SymbolTable getActiveSymbolTable() {
        return mSymbolTableList.getFirst();
    }

    public AbstractValue read(String symbol) throws InterpreterError {
        for (SymbolTable table : mSymbolTableList) {
            if (table.contains(symbol)) {
                return table.read(symbol);
            }
        }

        throw new InterpreterError(symbol, "Runtime Error: Variable '" + symbol + "' is not defined.");
    }

    public AbstractValue read(String symbol, int index) throws InterpreterError {
        for (SymbolTable table : mSymbolTableList) {
            if (table.contains(symbol)) {
                return table.read(symbol, index);
            }
        }

        throw new InterpreterError(symbol, "Runtime Error: Variable '" + symbol + "' is not defined.");
    }

    public AbstractValue read(String symbol, String member) throws InterpreterError {
        for (SymbolTable table : mSymbolTableList) {
            if (table.contains(symbol)) {
                return table.read(symbol, member);
            }
        }

        throw new InterpreterError(symbol, "Runtime Error: Variable '" + symbol + "' is not defined.");
    }

    public AbstractValue write(String symbol, AbstractValue value) throws InterpreterError {
        for (SymbolTable table : mSymbolTableList) {
            if (table.contains(symbol)) {
                return table.write(symbol, value);
            }
        }

        throw new InterpreterError(symbol, "Runtime Error: Variable '" + symbol + "' is not defined.");
    }

    public AbstractValue write(String symbol, int index, AbstractValue value) throws InterpreterError {
        for (SymbolTable table : mSymbolTableList) {
            if (table.contains(symbol)) {
                return table.write(symbol, index, value);
            }
        }

        throw new InterpreterError(symbol, "Runtime Error: Variable '" + symbol + "' is not defined.");
    }

    public AbstractValue write(String symbol, String member, AbstractValue value) throws InterpreterError {
        for (SymbolTable table : mSymbolTableList) {
            if (table.contains(symbol)) {
                return table.write(symbol, member, value);
            }
        }

        throw new InterpreterError(symbol, "Runtime Error: Variable '" + symbol + "' is not defined.");
    }
}
