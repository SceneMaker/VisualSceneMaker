package de.dfki.vsm.runtime.interpreter.symbol;

import de.dfki.vsm.runtime.interpreter.error.InterpreterError;
import de.dfki.vsm.runtime.interpreter.value.EventValue;
import de.dfki.vsm.runtime.interpreter.value.IntValue;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertThrows;

class SymbolEntryTest {

    @Test
    void rejectsNullWriteForEventVariable() {
        SymbolEntry entry = new SymbolEntry("event", new EventValue(10, null));
        assertThrows(InterpreterError.class, () -> entry.write(null));
    }

    @Test
    void rejectsNullWriteForRegularVariable() {
        SymbolEntry entry = new SymbolEntry("x", new IntValue(1));
        assertThrows(InterpreterError.class, () -> entry.write(null));
    }
}
