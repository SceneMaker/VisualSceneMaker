
/*
*
 */
package de.dfki.vsm.editor.util;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Color;

import java.util.TimerTask;

import javax.swing.JComponent;

/**
 * This class is used for specifing and representing visual activity cues for a
 * specific amount of time.
 *
 * @author Patrick Gebhard
 */
public class VisualisationTask extends TimerTask {
    int           mSteps       = 0;
    int           mCurrentStep = 0;
    JComponent    mC           = null;
    private Type  mType        = Type.Normal;
    private Color mColor;    // = new Color(246, 0, 0, 100);

    public enum Type { Highlight, Normal }

    ;
    public VisualisationTask(int steps, JComponent c) {
        mSteps       = steps;
        mCurrentStep = mSteps;
        mC           = c;
        mColor       = new Color(246, 0, 0, 100);
        mType        = Type.Normal;
    }

    public VisualisationTask(int steps, JComponent c, Color color) {
        mSteps       = steps;
        mCurrentStep = mSteps;
        mC           = c;
        mColor       = color;
        mType        = Type.Normal;
    }

    public VisualisationTask(int steps, JComponent c, Color color, Type type) {
        mSteps       = steps;
        mCurrentStep = mSteps;
        mC           = c;
        mColor       = color;
        mType        = type;
    }

    public synchronized Color getColor() {
        return mColor;
    }

    public synchronized int getActivitySteps() {
        return mSteps;
    }

    public synchronized int getActivityTime() {
        return (mCurrentStep > 0)
               ? mCurrentStep - 1
               : mCurrentStep;
    }

    public synchronized Type getType() {
        return mType;
    }

    public synchronized boolean isHighLight() {
        return (mType == VisualisationTask.Type.Highlight)
               ? true
               : false;
    }

    public void run() {
        mCurrentStep = (mCurrentStep > 0)
                       ? mCurrentStep - 1
                       : 0;

        if (mCurrentStep == 0) {
            cancel();
        } else {
            mC.repaint();
        }
    }
}
