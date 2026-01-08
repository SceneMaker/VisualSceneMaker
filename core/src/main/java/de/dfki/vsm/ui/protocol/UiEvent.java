package de.dfki.vsm.ui.protocol;

public final class UiEvent extends UiMessage {
    private final String mEvent;
    private final long mSequence;
    private final Object mPayload;

    public UiEvent(
            final int version,
            final String id,
            final UiChannel channel,
            final String event,
            final long timestamp,
            final long sequence,
            final Object payload) {
        super(version, id, UiMessageType.EVENT, channel, timestamp);
        mEvent = event;
        mSequence = sequence;
        mPayload = payload;
    }

    public static UiEvent create(final UiChannel channel, final String event, final Object payload) {
        return new UiEvent(UiProtocol.VERSION, null, channel, event, 0L, 0L, payload);
    }

    public String getEvent() {
        return mEvent;
    }

    public long getSequence() {
        return mSequence;
    }

    public Object getPayload() {
        return mPayload;
    }

    public UiEvent withDefaults(final String id, final long timestamp, final long sequence) {
        String nextId = isBlank(getId()) ? id : getId();
        long nextTimestamp = getTimestamp() > 0 ? getTimestamp() : timestamp;
        long nextSequence = mSequence > 0 ? mSequence : sequence;
        if (nextId == getId()
                && nextTimestamp == getTimestamp()
                && nextSequence == mSequence) {
            return this;
        }
        return new UiEvent(getVersion(), nextId, getChannel(), mEvent, nextTimestamp, nextSequence, mPayload);
    }

    private boolean isBlank(final String value) {
        return value == null || value.isBlank();
    }
}
