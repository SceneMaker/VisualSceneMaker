//package de.dfki.vsm.xtension.remote;
//
///**
// * @author Patrick
// * @author Gregor Mehlmann
// */
//public final class LogMessage {
//
//    public static enum Type {
//        ACT,
//        MESSAGE,
//        SCENE,
//        STATE,
//        VARASSIGN,
//        VARREQUEST,
//        ACTION
//    };
//
//    public static enum State {
//        COMPLETED,
//        CONTINUED
//    };
//    //
//    public Type mType;
//    public State mState;
//    public String mContent;
//    public long mTime = -1;
//    public long mDuration = -1;
//    //
//
//    public static final Type DEFAULT_TYPE = Type.ACTION;
//    public static final State DEFAULT_STATE = State.COMPLETED;
//    public static final String DEFAULT_SENDER_IS = "VSM";
//    public static final String DEFAULT_SEPERATOR = "#";
//    public static final String DEFAULT_CONTENT = "";
//    public static final long DEFAULT_DURATION = 1;
//
//    public final void setType(final Type type) {
//        mType = type;
//    }
//
//    public final void setTime(final long time) {
//        mTime = time;
//    }
//
//    public final void setContent(final String content) {
//        mContent = content;
//    }
//
//    public final void setDuration(final long duration) {
//        mDuration = duration;
//    }
//
//    public final void setState(final State state) {
//        mState = state;
//    }
//
//    @Override
//    public final String toString() {
//        final StringBuilder builder = new StringBuilder();
//        // Message format: <sender>#<class>#<content>#<timestamp>#<duration>#<state>
//        // Message example: VSM#SCENE#Welcome#123123123123123#5300#COMPLETED
//        builder.append(DEFAULT_SENDER_IS)
//                .append(DEFAULT_SEPERATOR).append(mType.name())
//                .append(DEFAULT_SEPERATOR).append(mContent)
//                .append(DEFAULT_SEPERATOR).append(mTime)
//                .append(DEFAULT_SEPERATOR).append(mDuration)
//                .append(DEFAULT_SEPERATOR).append(mState.name());
//        return builder.toString();
//    }
//}
