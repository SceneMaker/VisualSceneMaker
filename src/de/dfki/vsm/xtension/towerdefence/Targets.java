

package de.dfki.vsm.xtension.towerdefence;

import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.geld;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.turmangeklickt;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.j;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.preismultiplikator;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.aza;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.alleZiele;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.alleWegBoxen;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.alleZaubereraffen;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.mDEBUG;
import java.awt.Rectangle;
import java.awt.geom.Ellipse2D;
import java.util.ArrayList;
import java.util.Map;
import static org.ujmp.core.MatrixFactory.fill;
import processing.core.PApplet;
import static processing.core.PApplet.sq;
import static processing.core.PApplet.sqrt;
import static processing.core.PConstants.BASELINE;
import static processing.core.PConstants.CENTER;
import static processing.core.PConstants.CLOSE;
import static processing.core.PConstants.LEFT;
import static processing.core.PConstants.MITER;
import static processing.core.PConstants.RIGHT;
import static processing.core.PConstants.SQUARE;
import ddf.minim.AudioPlayer;
import ddf.minim.Minim;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.leben;
 /*
 * @author Jan
 */
 public   class Targets {
 private PApplet mApplet;
        float x, y;
        int lv, wegabschnitt/*its pathtile*/, wid/*id of the pathtile*/, id/*identification number*/;
        PathBox registrierteWB = null;//pathtile it is registerd
        PathBox physikalischeWB = null;//pathtile it is physically in 

        Targets(PApplet applet,int wid_0, int lv_0, int uid) {
            wegabschnitt = 0;
            x = 8;//koordinates where it starts
            y = 400;//
            lv = lv_0;
           
            id = uid;
             mApplet=applet;
        }

        @Override
        public String toString() {
            return "Z [" + id + "]";
        }
//find the pathbox where it is
        public void wegfinden() {
            registrierteWB = null;
            physikalischeWB = null;

            //checks all pathtiles
            for (Map.Entry<String, PathBox> entry : alleWegBoxen.entrySet()) {
                PathBox wb = entry.getValue();
                Rectangle r = new Rectangle(wb.x, wb.y, wb.width, wb.height);
                //checks in wich pathbox it is registerd
                if (wb.hatZiel("" + id)) {
                  
                    registrierteWB = wb;
                       
                }
               //checks in wich pathbox it physically is
                if (r.contains((new Float(x).intValue()), (new Float(y).intValue()))) {
                  
                    physikalischeWB = wb;
                }
            }

            // if it was not registerd until now
            if (registrierteWB == null && physikalischeWB != null) {
               
               physikalischeWB.fuegehinzuZiel("" + id, this);
                        
                registrierteWB = physikalischeWB;
            }

            //if it is physically there where it is registerd
            if (registrierteWB != null && physikalischeWB != null && registrierteWB == physikalischeWB) {
              
            }

            // if it is not where it is registerd
            if (registrierteWB != null && physikalischeWB != null && registrierteWB != physikalischeWB) {
           
                registrierteWB.entferneZiel("" + id);
               physikalischeWB.fuegehinzuZiel("" + id, this);
                wegabschnitt++;
            }

            // if it reached the end of the path
            if (registrierteWB != null && physikalischeWB == null) {
                //System.out.println("Ziel hat die Strecke verlassen, wird aus WegBox eliminiert und später aufgeräumt.");
                registrierteWB.entferneZiel("" + id);
                leben -= lv;
                lv = 0;
            }
        }
//move
        public void bewegen() {
            wegfinden();
            //direction depends from the pathbox it is in
            x += registrierteWB.x1 * ((lv + 3) / 2);
            y += registrierteWB.y1 * ((lv + 3) / 2);
        }
//draw a balloon
        void balloon(float x, float y, float groesse, float r, float g, float b) {
             mApplet.fill(r, g, b, 175);
             mApplet.pushMatrix();
             mApplet.translate(x, y);
             mApplet.scale(groesse);
             mApplet.noStroke();
             mApplet.ellipse(0, 0, 36, 45);
             mApplet.triangle(-0, 21, 5, 27, -5, 27);

             mApplet.beginShape();
             mApplet.fill(255);
             mApplet.vertex(0, -18);
             mApplet.bezierVertex(17, -23, 14, -3, 12, -7);
             mApplet.bezierVertex(10, 2, 12, -8, 0, -18);
             mApplet.endShape();
            mApplet. popMatrix();
        }
// draw the target, size and color depends on its lv
        public void zeichne() {

             mApplet.noStroke();
            switch (lv) {
                case 1:

                    balloon(x, y, 0.5f, 180, 23, 12);
                    break;
                case 2:
                    balloon(x, y, 0.6f, 23, 12, 180);
                    break;
                case 3:
                    balloon(x, y, 0.7f, 23, 180, 12);
                    break;
                case 4:
                    balloon(x, y, 0.9f, 200, 200, 0);
                    break;
                case 5:
                    balloon(x, y, 0.9f, 200, 100, 111);
                    break;
            }

            if (mDEBUG) {
                 mApplet.stroke(1);
                 mApplet.fill(0, 0, 0);
                 mApplet.text(id, x - 2, y + 18);
            }
        }
    }