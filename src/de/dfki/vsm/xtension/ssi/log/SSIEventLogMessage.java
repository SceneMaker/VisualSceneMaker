package de.dfki.vsm.xtension.ssi.log;

/**
 * @author Patrick
 * @author Gregor Mehlmann
 */
public final class SSIEventLogMessage {

    /*public static enum Type {
        ACT,
        MESSAGE,
        SCENE,
        STATE,
        VARASSIGN,
        VARREQUEST,
        ACTION
    };
     */
 /*
    public static enum State {
        COMPLETED,
        CONTINUED
    };
     */
    //
    public static final String DEFAULT_SENDER_ID = "vsm";
    public static final String DEFAULT_SEPERATOR = "#";

    //
    public String mName;
    public String mEvent;
    public String mSender;
    public String mState;
    public String mTime;
    public String mDuration;
    public String mContent;

    public final void setName(final String name) {
        mName = name;
    }

    public final void setSender(final String sender) {
        mSender = sender;
    }

    public final void setEvent(final String event) {
        mEvent = event;
    }

    public final void setState(final String state) {
        mState = state;
    }

    public final void setTime(final String time) {
        mTime = time;
    }

    public final void setContent(final String content) {
        mContent = content;
    }

    public final void setDuration(final String duration) {
        mDuration = duration;
    }

    @Override
    public final String toString() {
        final StringBuilder builder = new StringBuilder();
        builder.append(DEFAULT_SENDER_ID)
                .append(DEFAULT_SEPERATOR).append(mName)
                .append(DEFAULT_SEPERATOR).append(mSender)
                .append(DEFAULT_SEPERATOR).append(mEvent)
                .append(DEFAULT_SEPERATOR).append(mState)
                .append(DEFAULT_SEPERATOR).append(mTime)
                .append(DEFAULT_SEPERATOR).append(mDuration)
                .append(DEFAULT_SEPERATOR).append(mContent);
        return builder.toString();
    }
}
