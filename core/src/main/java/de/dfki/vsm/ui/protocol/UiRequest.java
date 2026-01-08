package de.dfki.vsm.ui.protocol;

public final class UiRequest extends UiMessage {
    private final String mAction;
    private final Object mPayload;

    public UiRequest(
            final int version,
            final String id,
            final UiChannel channel,
            final long timestamp,
            final String action,
            final Object payload) {
        super(version, id, UiMessageType.REQUEST, channel, timestamp);
        mAction = action;
        mPayload = payload;
    }

    public String getAction() {
        return mAction;
    }

    public Object getPayload() {
        return mPayload;
    }
}
