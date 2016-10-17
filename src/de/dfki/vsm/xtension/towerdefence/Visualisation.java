package de.dfki.vsm.xtension.towerdefence;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.Timer;
import processing.core.PApplet;

/**
 *
 * @author Jan Stieling
 * 
 */
public class Visualisation implements ActionListener {

  public int mCurrentStep = 100;
  public Timer mTimer;
  private PApplet mWs;

  public Visualisation(PApplet ws) {
    mWs = ws;
    mTimer = new Timer(25, this);
    mTimer.start();
  }

  public Visualisation(PApplet ws, int intervall) {
    mWs = ws;
    mTimer = new Timer(intervall, this);
    mTimer.start();
  }
  
  public void actionPerformed(ActionEvent e) {
    mCurrentStep -= 1;
    mWs.redraw();
    if (mCurrentStep <= 0) {
      mCurrentStep = 0;
      mTimer.stop();
    }
  }
}
