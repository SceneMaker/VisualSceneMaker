package de.dfki.vsm.ui.protocol;

import java.util.function.Supplier;

public interface UiEventSink {
    boolean isActive();

    void emit(UiEvent event);

    void emitLazy(Supplier<UiEvent> supplier);
}
