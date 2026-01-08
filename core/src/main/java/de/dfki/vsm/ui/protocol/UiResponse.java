package de.dfki.vsm.ui.protocol;

public final class UiResponse extends UiMessage {
    private final String mRequestId;
    private final boolean mOk;
    private final UiError mError;
    private final Object mPayload;

    public UiResponse(
            final int version,
            final String id,
            final UiChannel channel,
            final long timestamp,
            final String requestId,
            final boolean ok,
            final UiError error,
            final Object payload) {
        super(version, id, UiMessageType.RESPONSE, channel, timestamp);
        mRequestId = requestId;
        mOk = ok;
        mError = error;
        mPayload = payload;
    }

    public String getRequestId() {
        return mRequestId;
    }

    public boolean isOk() {
        return mOk;
    }

    public UiError getError() {
        return mError;
    }

    public Object getPayload() {
        return mPayload;
    }
}
