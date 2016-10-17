
package de.dfki.vsm.xtension.towerdefence;

import java.util.HashMap;
import java.util.Map;
import processing.core.PApplet;

/**
 *
 * @author Jan Stieling
 *
 */
public class PathBox {

    int id/*identification number*/, x/*x coordinate*/, y/*y coordinate*/, width, height, x1/*how much a target in this pathbox should move in x direction*/, y1/*how much a target in this pathbox should move in x direction*/, waz = 0;//number of targets in this pathbox/
    HashMap<String, Targets> zielSpeicher = new HashMap<String, Targets>();//the tarhets in this pathbox
   // Targets[] ziele2;
    private PApplet mApplet;

    PathBox(PApplet applet, int pathCnt, int x_0, int y_0, int x1_0, int y1_0) {
        mApplet = applet;
        id = pathCnt;
        x = x_0;
        y = y_0;
        width = 10;
        height = 10;
        x1 = x1_0;
        y1 = y1_0;
       // ziele2 = new Targets[1000];
        pathCnt++;
    }
//add the given target
    public void fuegehinzuZiel(String zielname, Targets ziel) {
     
        zielSpeicher.put(zielname, ziel);
        waz++;
    }
// remove the given target
    public void entferneZiel(String zielname) {
        zielSpeicher.remove(zielname);
        waz--;
    }
//returns if it contains the asked target
    public boolean hatZiel(String zielname) {
        return zielSpeicher.containsKey(zielname);
    }

    @Override
    public String toString() {
        return "WB " + x + ", " + y;
    }
//draw the pathbox
    public void zeichne() {
        mApplet.strokeWeight(0.5f);
        mApplet.stroke(230, 230, 220);
        mApplet.fill(250, 210, 190);
        mApplet.quad(x - 5, y - 5, x - 2, y - 5, x, y - 2, x - 3, y);
        mApplet.fill(190, 220, 250);
        mApplet.triangle(x - 5, y - 5, x - 3.5f, y - 1, x - 5, y + 1);
        mApplet.fill(210, 250, 190);
        mApplet.quad(x - 3.5f, y - 1, x - 1, y + 5, x - 5, y + 5, x - 5, y + 1);
        mApplet.fill(190, 220, 250);
        mApplet.triangle(x, y - 2, x + 5, y - 5, x - 2, y - 5);
        mApplet.fill(190, 240, 240);
        mApplet.quad(x + 5, y - 5, x + 5, y + 5, x + 3, y + 5, x + 2, y - 3);
        mApplet.fill(240, 210, 240);
        mApplet.quad(x - 3, y, x + 2, y - 3, x + 3, y + 5, x - 1, y + 5);
        mApplet.stroke(0);

        if (BalloonTowerDefence.mDEBUG) {
            mApplet.textSize(8);
            mApplet.text("" + x1 + "" + y1, x, y);

            int zCnt = -12;
            for (Map.Entry<String, Targets> entry : zielSpeicher.entrySet()) {
                mApplet.text(entry.getValue() + "(" + entry.getValue().lv + ")", x, y + zCnt);
                zCnt -= 12;
            }

            mApplet.text(zielSpeicher.size() + "", x, y + 12);
        }
    }
}
