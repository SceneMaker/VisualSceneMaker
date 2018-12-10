package de.dfki.vsm.xtension.stickmantts.action;

import de.dfki.action.sequence.WordTimeMarkSequence;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.xtension.stickmantts.util.tts.sequence.Phoneme;

/**
 * Created by alvaro on 4/7/16.
 */
public final class ActionMouthActivity extends AbstractActivity {

    private int mDuration;
    private Phoneme mPhoneme;
    private final String mText;
    private WordTimeMarkSequence mWts;
    private String word;

    public ActionMouthActivity(String actor, /*String mode,*/ String name, String text) {
        super(Type.parallel, actor, /*mode,*/ name);
        mText = text;
    }

    public ActionMouthActivity(String actor, /*String mode,*/ String name, String text, int duration) {
        super(Type.parallel, actor, /*mode,*/ name);
        mDuration = duration;
        mText = text;
    }

    public ActionMouthActivity(String actor, /*String mode,*/ String name, String text, int duration, WordTimeMarkSequence wts) {
        super(Type.parallel, actor, /*mode,*/ name);
        mDuration = duration;
        mText = text;
        mWts = wts;
    }

    public WordTimeMarkSequence getWortTimeMark() {
        return mWts;
    }

    public ActionMouthActivity(String actor, /*String mode,*/ String name, String text, int duration, Phoneme p) {
        super(Type.parallel, actor, /*mode,*/ name);
        mDuration = duration;
        mPhoneme = p;
        mText = text;
    }

    public Phoneme getPhoneme() {
        return mPhoneme;
    }

    public int getDuration() {
        return mDuration;
    }

    @Override
    public final String getText() {
        return mText;
    }

    public void setWord(String w) {
        word = w;
    }

    public String getWord() {
        return word;
    }
}
