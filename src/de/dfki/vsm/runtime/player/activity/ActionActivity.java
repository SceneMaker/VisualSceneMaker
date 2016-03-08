package de.dfki.vsm.runtime.player.activity;

/**
 * @author Gregor Mehlmann
 */
public final class ActionActivity implements AbstractActivity {

    /*private final String mMark;*/
    private final String mText;

    public ActionActivity(/*final String mark,*/final String text) {
        //mMark = mark;
        mText = text;
    }

    @Override
    public final String getText() {
        return mText;
    }

    //public final String getMark() {
    //    return mMark;
    //}
    @Override
    public final String toString() {
        return getText();
    }
}
