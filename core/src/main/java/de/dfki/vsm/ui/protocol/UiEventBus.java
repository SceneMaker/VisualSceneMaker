package de.dfki.vsm.ui.protocol;

import java.util.UUID;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Supplier;

public final class UiEventBus implements UiEventSink {
    private final CopyOnWriteArrayList<UiEventListener> mListeners = new CopyOnWriteArrayList<>();
    private final AtomicBoolean mActive = new AtomicBoolean(false);
    private final AtomicLong mSequence = new AtomicLong(0L);

    public void setActive(final boolean active) {
        mActive.set(active);
    }

    @Override
    public boolean isActive() {
        return mActive.get() && !mListeners.isEmpty();
    }

    public void addListener(final UiEventListener listener) {
        if (listener != null) {
            mListeners.addIfAbsent(listener);
        }
    }

    public void removeListener(final UiEventListener listener) {
        if (listener != null) {
            mListeners.remove(listener);
        }
    }

    @Override
    public void emit(final UiEvent event) {
        if (!isActive() || event == null) {
            return;
        }
        UiEvent normalized = normalize(event);
        for (UiEventListener listener : mListeners) {
            listener.onEvent(normalized);
        }
    }

    @Override
    public void emitLazy(final Supplier<UiEvent> supplier) {
        if (!isActive() || supplier == null) {
            return;
        }
        UiEvent event = supplier.get();
        if (event != null) {
            emit(event);
        }
    }

    private UiEvent normalize(final UiEvent event) {
        String id = event.getId();
        if (id == null || id.isBlank()) {
            id = UUID.randomUUID().toString();
        }
        long timestamp = event.getTimestamp() > 0 ? event.getTimestamp() : System.currentTimeMillis();
        long sequence = event.getSequence() > 0 ? event.getSequence() : mSequence.incrementAndGet();
        return event.withDefaults(id, timestamp, sequence);
    }
}
