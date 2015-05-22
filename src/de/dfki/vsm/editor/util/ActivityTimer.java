
/*
* To change this template, choose Tools | Templates
* and open the template in the editor.
 */
package de.dfki.vsm.editor.util;

//~--- JDK imports ------------------------------------------------------------

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JComponent;

/**
 *
 * @author Patrick Gebhard
 */
public class ActivityTimer implements ActionListener {
    JComponent mComponent = null;
    int        mActivityTime;

    public ActivityTimer(int activityTime, JComponent c) {
        mActivityTime = activityTime;
        mComponent    = c;
    }

    public synchronized int getActivityTime() {
        return mActivityTime;
    }

    public void actionPerformed(ActionEvent e) {
        if (mActivityTime > 0) {

            ////System.out.println("activity");
            mActivityTime--;
            mComponent.repaint();
        } else {

            ////System.out.println("no activity");
            mComponent.repaint();
        }
    }
}
