package de.dfki.vsm.runtime.interpreter.symbol;

import de.dfki.vsm.event.EventDispatcher;
import de.dfki.vsm.runtime.interpreter.error.InterpreterError;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.util.cpy.Copyable;

import java.util.HashMap;
import java.util.Map.Entry;

/**
 * Copy-on-write symbol table. When getCopy() is called, the new table shares the
 * backing HashMap with the original. The first mutation (create/write) on either
 * table triggers a deep copy of all entries, making subsequent mutations independent.
 *
 * This eliminates deep copies for read-only snapshots (e.g., history recording).
 *
 * @author Gregor Mehlmann
 */
public final class SymbolTable implements Copyable {

    // The Symbol Table Map (shared when mShared == true)
    private HashMap<String, SymbolEntry> mSymbolTable;

    // True if the backing map is shared with another SymbolTable instance.
    // All mutations must call ensureExclusive() first.
    private boolean mShared;

    // The per-project event dispatcher (may be null for legacy/no-dispatcher paths)
    private final EventDispatcher mDispatcher;

    public SymbolTable() {
        mSymbolTable = new HashMap<>();
        mShared = false;
        mDispatcher = null;
    }

    public SymbolTable(final HashMap<String, SymbolEntry> table) {
        mSymbolTable = table;
        mShared = false;
        mDispatcher = null;
    }

    public SymbolTable(final EventDispatcher dispatcher) {
        mSymbolTable = new HashMap<>();
        mShared = false;
        mDispatcher = dispatcher;
    }

    // COW constructor — shares the backing map
    private SymbolTable(final HashMap<String, SymbolEntry> table, final EventDispatcher dispatcher, boolean shared) {
        mSymbolTable = table;
        mShared = shared;
        mDispatcher = dispatcher;
    }

    // If the backing map is shared, deep-copy it to make mutations safe.
    private void ensureExclusive() {
        if (mShared) {
            mSymbolTable = deepCopyEntries();
            mShared = false;
        }
    }

    public final HashMap<String, SymbolEntry> getSymbolTable() {
        return mSymbolTable;
    }

    // Deep-copy all entries in the backing map.
    private HashMap<String, SymbolEntry> deepCopyEntries() {
        final HashMap<String, SymbolEntry> copy = new HashMap<>();
        for (Entry<String, SymbolEntry> entry : mSymbolTable.entrySet()) {
            copy.put(entry.getKey(), entry.getValue().getCopy());
        }
        return copy;
    }

    public final boolean contains(final SymbolEntry entry) {
        return mSymbolTable.containsValue(entry);
    }

    public final boolean contains(final String symbol) {
        return mSymbolTable.containsKey(symbol);
    }

    public final void create(final String symbol, final AbstractValue value) throws InterpreterError {
        ensureExclusive();
        mSymbolTable.put(symbol, new SymbolEntry(symbol, value, mDispatcher));
    }

    public final AbstractValue write(final String symbol, final AbstractValue value) throws InterpreterError {
        ensureExclusive();
        return mSymbolTable.get(symbol).write(value);
    }

    public final AbstractValue write(final String symbol, final int index, final AbstractValue value)
            throws InterpreterError {
        ensureExclusive();
        return mSymbolTable.get(symbol).write(value, index);
    }

    public final AbstractValue write(final String symbol, final String member, final AbstractValue value)
            throws InterpreterError {
        ensureExclusive();
        return mSymbolTable.get(symbol).write(value, member);
    }

    public final AbstractValue read(final String symbol) {
        return mSymbolTable.get(symbol).read();
    }

    public final AbstractValue read(final String symbol, final int index) throws InterpreterError {
        return mSymbolTable.get(symbol).read(index);
    }

    public final AbstractValue read(final String symbol, final String member) throws InterpreterError {
        return mSymbolTable.get(symbol).read(member);
    }

    /**
     * Returns a copy-on-write snapshot. The new table shares the backing HashMap
     * with this table. The first mutation on either table triggers a deep copy.
     */
    @Override
    public final SymbolTable getCopy() {
        mShared = true;
        return new SymbolTable(mSymbolTable, mDispatcher, true);
    }
}
