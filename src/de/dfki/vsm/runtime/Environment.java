package de.dfki.vsm.runtime;

import de.dfki.vsm.runtime.symbol.SymbolTable;
import de.dfki.vsm.runtime.error.RunTimeException;
import de.dfki.vsm.runtime.value.AbstractValue;
import de.dfki.vsm.util.cpy.Copyable;
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

    public void create(String symbol, AbstractValue value) throws RunTimeException {
        if (mSymbolTableList.isEmpty()) {
            throw new RunTimeException(symbol,
                    "Runtime Error: Variable '" + symbol + "' cannot be declared.");
        }
        for (SymbolTable symbolTable : mSymbolTableList) {
            if (symbolTable.contains(symbol)) {
                throw new RunTimeException(symbol,
                        "Runtime Error: Variable '" + symbol + "' is already defined.");
            }
        }
        mSymbolTableList.getFirst().create(symbol, value);
    }

    public SymbolTable getActiveSymbolTable() {
        return mSymbolTableList.getFirst();
    }

    public AbstractValue read(String symbol) throws RunTimeException {
        for (SymbolTable table : mSymbolTableList) {
            if (table.contains(symbol)) {
                return table.read(symbol);
            }
        }
        throw new RunTimeException(symbol,
                "Runtime Error: Variable '" + symbol + "' is not defined.");
    }

    public AbstractValue read(String symbol, int index) throws RunTimeException {
        for (SymbolTable table : mSymbolTableList) {
            if (table.contains(symbol)) {
                return table.read(symbol, index);
            }
        }
        throw new RunTimeException(symbol,
                "Runtime Error: Variable '" + symbol + "' is not defined.");
    }

    public AbstractValue read(String symbol, String member) throws RunTimeException {
        for (SymbolTable table : mSymbolTableList) {
            if (table.contains(symbol)) {
                return table.read(symbol, member);
            }
        }
        throw new RunTimeException(symbol,
                "Runtime Error: Variable '" + symbol + "' is not defined.");
    }

    public AbstractValue write(String symbol, AbstractValue value) throws RunTimeException {
        for (SymbolTable table : mSymbolTableList) {
            if (table.contains(symbol)) {
                return table.write(symbol, value);
            }
        }
        throw new RunTimeException(symbol,
                "Runtime Error: Variable '" + symbol + "' is not defined.");
    }

    public AbstractValue write(String symbol, int index, AbstractValue value) throws RunTimeException {
        for (SymbolTable table : mSymbolTableList) {
            if (table.contains(symbol)) {
                return table.write(symbol, index, value);
            }
        }
        throw new RunTimeException(symbol,
                "Runtime Error: Variable '" + symbol + "' is not defined.");
    }

    public AbstractValue write(String symbol, String member, AbstractValue value) throws RunTimeException {
        for (SymbolTable table : mSymbolTableList) {
            if (table.contains(symbol)) {
                return table.write(symbol, member, value);
            }
        }
        throw new RunTimeException(symbol,
                "Runtime Error: Variable '" + symbol + "' is not defined.");
    }
}
