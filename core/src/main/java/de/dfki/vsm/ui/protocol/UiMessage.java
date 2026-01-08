package de.dfki.vsm.ui.protocol;

public abstract class UiMessage {
    private final int mVersion;
    private final String mId;
    private final UiMessageType mType;
    private final UiChannel mChannel;
    private final long mTimestamp;

    protected UiMessage(
            final int version,
            final String id,
            final UiMessageType type,
            final UiChannel channel,
            final long timestamp) {
        mVersion = version;
        mId = id;
        mType = type;
        mChannel = channel;
        mTimestamp = timestamp;
    }

    public final int getVersion() {
        return mVersion;
    }

    public final String getId() {
        return mId;
    }

    public final UiMessageType getType() {
        return mType;
    }

    public final UiChannel getChannel() {
        return mChannel;
    }

    public final long getTimestamp() {
        return mTimestamp;
    }
}
