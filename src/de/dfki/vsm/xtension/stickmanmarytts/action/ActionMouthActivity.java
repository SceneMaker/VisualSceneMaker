package de.dfki.vsm.xtension.stickmanmarytts.action;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.xtension.stickmanmarytts.util.tts.sequence.Phoneme;

/**
 * Created by alvaro on 4/7/16.
 */
public final class ActionMouthActivity extends AbstractActivity {
    private int mDuration;
    private Phoneme mPhoneme;
    private final String mText;
    public ActionMouthActivity(String actor, String mode, String name, String text) {
        super(Policy.PARALLEL, actor, mode, name);
        mText = text;
    }
    public ActionMouthActivity(String actor, String mode, String name, String text, int duration) {
        super(Policy.PARALLEL, actor, mode, name);
        mDuration = duration;
        mText = text;
    }

    public ActionMouthActivity(String actor, String mode, String name, String text, int duration, Phoneme p) {
        super(Policy.PARALLEL, actor, mode, name);
        mDuration = duration;
        mPhoneme = p;
        mText = text;
    }

    public Phoneme getPhoneme() {
        return mPhoneme;
    }

    public int getDuration(){
        return mDuration;
    }

    public final String getText() {
        return mText;
    }
}
