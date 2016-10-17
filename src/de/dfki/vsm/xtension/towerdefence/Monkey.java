
package de.dfki.vsm.xtension.towerdefence;

import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.geld;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.turmangeklickt;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.j;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.preismultiplikator;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.aa;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.alleZiele;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.alleWegBoxen;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.alleAffen;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.mDEBUG;
import java.awt.Rectangle;
import de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence;
import de.dfki.vsm.xtension.towerdefence.PathBox;
import de.dfki.vsm.xtension.towerdefence.Visualisation;
import java.awt.geom.Ellipse2D;
import java.util.ArrayList;
import java.util.Map;
import static javafx.scene.paint.Color.color;
import static javafx.scene.transform.Transform.translate;
import static org.ujmp.core.MatrixFactory.fill;
import processing.core.PApplet;
import static processing.core.PApplet.sq;
import static processing.core.PApplet.sqrt;
import static processing.core.PConstants.BASELINE;
import static processing.core.PConstants.CENTER;
import static processing.core.PConstants.CLOSE;
import static processing.core.PConstants.LEFT;
import static processing.core.PConstants.RIGHT;

/*
 * @author Jan Stieling
 */
public class Monkey extends Tower{

    private PApplet mApplet;
  
    int feuerrate = 10/*firerrate*/, geschossdurchschlagskraft = 1/*penetration*/,reichweite = 120/*range*/;
   
    Geschoss[] superGeschosse;//an aray for the projectiles
    
   

    Monkey(PApplet applet, float x_0, float y_0,String art_0) {
          super(applet,x_0, y_0, art_0);
       superGeschosse = new Geschoss[10];
      
       x = x_0;
       y = y_0;
        id = aa;
        art=art_0;
        mApplet = applet;
      
        sichtweite = new Ellipse2D.Float(x - reichweite / 2, y - reichweite / 2, reichweite, reichweite);

    
        for (Map.Entry<String, PathBox> entry : alleWegBoxen.entrySet()) {
            PathBox wb = entry.getValue();

          
            if (sichtweite.contains(wb.x, wb.y) && sichtweite.contains(wb.x + wb.width, wb.y + wb.height)) {

                inReichweiteWegBoxen.add(entry.getKey());
            }
        }
        aa++;

    }

    class Geschoss extends Tower.GeschosseSuperklasse {

        float x, x1, y, y1, zielx, ziely;
        int  affeid/*id of the monkey wich created it*/, unschaerfe = 10/*in which range targets count as hit*/;
       

        Geschoss(int affeid_0, float x_0, float y_0,int sprengkraft_0, int durchschlagskraft_0, float zielx_0, float ziely_0, int r_0, int g_0, int b_0, String art_0) {
            super(affeid_0, x_0, y_0, sprengkraft_0, durchschlagskraft_0, zielx_0, ziely_0, r_0, g_0, b_0, art_0);

            x = x_0;
            y = y_0;
            zielx = zielx_0;
            ziely = ziely_0;
            x1 = (zielx - x) / ((sqrt(sq(zielx - x))) + (sqrt(sq(ziely - y))));//see Tower.GeschosseSuperklasse 
            y1 = (ziely - y) / ((sqrt(sq(zielx - x))) + (sqrt(sq(ziely - y))));//
            id = ag;
            affeid = affeid_0;
            durchschlagskraft = durchschlagskraft_0;
            if (durchschlagskraft == 18) {//if penetration == the kind of the projectile is changed to a katapult projectile
                art = "katapultgeschoss";
            }
            r = r_0;
            g = g_0;
            b = b_0;
           
        }
//draw the projectile
        public void zeichne() {

            if (art == "katapultgeschoss") {//if it is a katapult projectile

                mApplet.ellipse(x + 17, y + 8, 10, 10);
                mApplet.triangle(x + 22, y + 7, x + 22, y + 9, x + 25, y + 8);
                mApplet.triangle(x + 12, y + 7, x + 12, y + 9, x + 9, y + 8);
                mApplet.triangle(x + 16, y + 13, x + 18, y + 13, x + 17, y + 16);
                mApplet.triangle(x + 16, y + 3, x + 18, y + 3, x + 17, y);
                mApplet.triangle(x + 14.5f, y + 9, x + 16.5f, y + 11, x + 11, y + 14);
                mApplet.triangle(x + 18, y + 9, x + 20, y + 11, x + 24, y + 15);
                mApplet.triangle(x + 18, y + 4, x + 20, y + 6, x + 23, y + 2);
                mApplet.triangle(x + 14.5f, y + 4, x + 16.5f, y + 6, x + 10.5f, y + 1);

            } else {//if not
                mApplet.fill(r, g, b);
                mApplet.ellipse(x, y, 5, 5);
            }

        }
//makes the projectile moves and hit targets
        public void fliegen() {

            for (Map.Entry<String, PathBox> entry : alleWegBoxen.entrySet()) {
                PathBox wb = entry.getValue();

                Rectangle r = new Rectangle(wb.x - unschaerfe / 2, wb.y - unschaerfe / 2, wb.width + unschaerfe, wb.height + unschaerfe);

                if (r.contains((new Float(x).intValue()), (new Float(y).intValue()))) { //if theire is a target in the pathbox
                    if (wb.zielSpeicher.size() > 0) {
                        Targets z = null;

                     
                        for (Map.Entry<String, Targets> zielEintrag : wb.zielSpeicher.entrySet()) {
                        //the target, which is choosen here, will be hit
                            z = (z == null) ? zielEintrag.getValue() : z;
                        }
                        if (z != null) {
                            //the target is hit
                            alleAffen[affeid].anzahlTreffer++;
                            durchschlagskraft--;//he penetration power of the projectile decrease
                            geld++;//the gold counter increase
                            z.lv--;//the level of the target decrease
                        }
                    }
                }
            }
//the projectile moves
            x += x1 * 10;
            y += y1 * 10;

            // the projectile is deleted if it is to far away from the monkey or its penetrationpower reaches zero
            if (sqrt(sq(alleAffen[affeid].x - x) + sq(alleAffen[affeid].y - y)) > 180 * sqrt(geschossdurchschlagskraft) || durchschlagskraft <= 0) {
                alleAffen[affeid].geschossloeschen(id);
            }
        }
    }

    public void implantiereGedanken(String text) {
        mAnzeigeManager = new Visualisation(mApplet);

        gedanken = text;
    }

   
  //the monkey shoot
    public void schiessen() {
        //only target shooting
        zielen();

        if (zielgefunden == true && 400 / feuerrate <= verzoegerung) {//if it has a target and finished reloading

            verzoegerung = 0;
            //depends on its projectile penetration(which depends of his level) it create different projectiles
            switch (geschossdurchschlagskraft) {
                case 1:
                    superGeschosse[ag] = new Geschoss(id, x, y,0, geschossdurchschlagskraft, ziel.x, ziel.y, 0, 0, 0, "geschoss1");
                    break;
                case 2:
                    superGeschosse[ag] = new Geschoss(id, x, y,0,geschossdurchschlagskraft, ziel.x, ziel.y, 200, 50, 0, "geschoss2");
                    break;
                case 18:
                    superGeschosse[ag] = new Geschoss(id, x, y, 0,geschossdurchschlagskraft, ziel.x, ziel.y, 0, 0, 0, "katapultgeschoss");
                    break;
            }
        }

        verzoegerung++;
    }
//draw speechbubble
    void zeichneSprechblase(float x, float y, String text) {
        mApplet.curveTightness(0.05f);

        float textHoehe = (mApplet.textAscent() + mApplet.textDescent()) * 1.75f;

        float tBreite = mApplet.textWidth(text);
        float einDrittel = tBreite / 3.0f;
       
        einDrittel = (einDrittel < textHoehe) ? textHoehe : einDrittel;

        float h = einDrittel * 0.9f;
        float b = einDrittel * 2 * 0.9f;

        y = y - h;

        mApplet.strokeWeight(b / 100);
        mApplet.fill(255, 255, 255, 128);

        mApplet.beginShape();
        mApplet.curveVertex(x + b, y);
        mApplet.curveVertex(x + (b / 13.3f), y);

        mApplet.curveVertex(x, y + h);

        mApplet.curveVertex(x - (b / 5), y + h + (h / 1.6f));
        mApplet.curveVertex(x + (b / 6.6f), y + h + (h / 8));

        mApplet.curveVertex(x + b, y + h);

        mApplet.curveVertex(x + b, y);
        mApplet.curveVertex(x + (b / 13.3f), y);

        mApplet.curveVertex(x, y + h);
        mApplet.endShape(CLOSE);
        mApplet.textAlign(CENTER, CENTER);
        mApplet.fill(0, 0, 0, 128);
        mApplet.text(text, x + (b / 18), y - (h / 18), b, h);
        mApplet.textAlign(BASELINE);
    }
   //draw the monkey and calls other draw methods
    public void zeichne() {

        if (gedanken != "") {
            boolean zeichneSb = (mAnzeigeManager != null && mAnzeigeManager.mTimer.isRunning());
            if (zeichneSb) {
                zeichneSprechblase(x + 24, y - 60, gedanken);
            } else {
                gedanken = "";
            }
        }

        // makes thet the monkey turns to it target
        float winkel = 0f;
        if (zielgefunden) {
            float azVx = ziel.x - x;
            float azVy = ziel.y - y;
            winkel = new Float(Math.atan2(azVx, azVy)) + new Float(Math.PI);
        }
        mApplet.pushMatrix();

        mApplet.translate(x, y);
        mApplet.rotate(-winkel);
        mApplet.translate(-x, -y);

        
        //choose the methode which draws the monkey depending on its level
        switch (lv) {
            case 1:

                zeichneAffeLv1(x, y);

                break;
            case 2:
             zeichneAffeLv2(x, y);
                break;
            case 3:
               zeichneAffeLv3(x, y);
                break;
            case 4:
              zeichneAffeLv4(x, y);
                break;
        }
        mApplet.popMatrix();

        if (mDEBUG) {
          
            mApplet.noStroke();
            mApplet.fill(0, 255, 0, 128);
            mApplet.ellipse(new Float(sichtweite.getX() + reichweite / 2), new Float(sichtweite.getY() + reichweite / 2), new Float(sichtweite.getWidth()), new Float(sichtweite.getHeight()));

            mApplet.stroke(1);
            mApplet.fill(0, 0, 0);

         
            mApplet.text("überwache " + inReichweiteWegBoxen.size() + " WegBoxen", x + 50, y);
       
            String pZiele = "";
            for (Targets z : potentielleZiele) {
                pZiele += z + ", ";
            }
            mApplet.text("pot. Ziele  " + pZiele, x + 50, y + 12);
        
            mApplet.text("schieße auf " + ziel, x + 50, y + 24);

          
            for (int gc = 0; gc < ag; gc++) {
                mApplet.stroke(2);
                color(255, 0, 0, 128);
                mApplet.line(x, y, superGeschosse[gc].x, superGeschosse[gc].y);
            }
        }

    }


    public void upgraden() {
  if( turmangeklickt == 0){
         ausgewaehlt = 0;
        
        }
        if (mApplet.mousePressed == true) {
            if (mApplet.mouseButton == RIGHT && sqrt(sq(x - mApplet.mouseX) + sq(y - mApplet.mouseY)) < 10 && turmangeklickt == 0) {
                turmangeklickt = 1;
                ausgewaehlt = 1;
            }
        }

        if (mApplet.mousePressed == true) {
            if (mApplet.mouseButton == LEFT && turmangeklickt == 1 && ausgewaehlt == 1) {

                if (mApplet.mouseX > x + 27 && mApplet.mouseY > y && mApplet.mouseX < x + 71 && mApplet.mouseY < y + 10) {
                    j = 0;
                    switch (lv) {
                        case 1:
                            if (geld >= 120 * preismultiplikator) {
                                geschossdurchschlagskraft++;
                                geld -= 120 * preismultiplikator;
                                lv++;
                            }
                            break;
                        case 2:
                            if (geld >= 75 * preismultiplikator) {
                                reichweite += 30;
                                for (Map.Entry<String, PathBox> entry : alleWegBoxen.entrySet()) {
                                    PathBox wb = entry.getValue();
                               
                                    if (sichtweite.contains(wb.x, wb.y) && sichtweite.contains(wb.x + wb.width, wb.y + wb.height)) {
                                    
                                        inReichweiteWegBoxen.add(entry.getKey());
                                    }
                                }
                                geld -= 75 * preismultiplikator;
                                lv++;
                            }
                            break;
                        case 3:
                            if (geld >= 425 * preismultiplikator) {
                                geschossdurchschlagskraft += 16;
                                feuerrate -= 5;
                                reichweite += 50;
                                for (Map.Entry<String, PathBox> entry : alleWegBoxen.entrySet()) {
                                    PathBox wb = entry.getValue();

                                  
                                
                                    if (sichtweite.contains(wb.x, wb.y) && sichtweite.contains(wb.x + wb.width, wb.y + wb.height)) {
                                     
                                        inReichweiteWegBoxen.add(entry.getKey());
                                    }
                                }
                                geld -= 425 * preismultiplikator;
                                lv++;
                            }
                            break;
                    }
                    turmangeklickt = 0;
                    ausgewaehlt = 0;
                }
            }
        }

        if (turmangeklickt == 1 && ausgewaehlt == 1) {
            mApplet.noStroke();
            mApplet.fill(125, 125, 255, 128);
            mApplet.ellipse(x, y, reichweite / 2, reichweite / 2);
            mApplet.fill(255);
            switch (lv) {
                case 1:
                    mApplet.rect(x + 12, y, 110, 10);
                    mApplet.fill(0);
                    mApplet.text("penetration+ " + (int) (120 * preismultiplikator), x + 13, y + 10);
                    break;
                case 2:
                    mApplet.rect(x + 12, y, 70, 10);
                    mApplet.fill(0);
                    mApplet.text("range+ " + (int) (75 * preismultiplikator), x + 13, y + 10);
                    break;
                case 3:
                    mApplet.rect(x + 12, y, 85, 10);
                    mApplet.fill(0);
                    mApplet.text("katapult " + (int) (350 * preismultiplikator), x + 13, y + 10);
                    break;

            }
            mApplet.fill(255);
        }
    }

   
//draw a monkey lv1
    private void zeichneAffeLv1(float x, float y) {

        mApplet.fill(0);
        mApplet.ellipse(x - 10, y - 8, 5, 5);
        mApplet.fill(200, 100, 0);
        mApplet.stroke(0);
        mApplet.ellipse(x + 13, y, 10, 10);
        mApplet.ellipse(x - 13, y, 10, 10);
        mApplet.ellipse(x, y, 25, 20);
        mApplet.noStroke();
        mApplet.ellipse(x + 3, y + 19, 4, 4);
        mApplet.ellipse(x + 3, y + 17, 4, 4);
        mApplet.ellipse(x + 2, y + 15, 4, 4);
        mApplet.ellipse(x, y + 14, 4, 4);
        mApplet.ellipse(x, y + 12, 4, 4);
        mApplet.ellipse(x, y + 10, 4, 4);

    }
//draw a monkey lv1 with another projectile(which makes he become a monkey lv2)
    private void zeichneAffeLv2(float x, float y) {
   
        mApplet.fill(200, 50, 0);
        mApplet.ellipse(x - 10, y - 8, 5, 5);
        mApplet.fill(200, 100, 0);
        mApplet.stroke(0);
        mApplet.ellipse(x + 13, y, 10, 10);
        mApplet.ellipse(x - 13, y, 10, 10);
        mApplet.ellipse(x, y, 25, 20);
        mApplet.noStroke();
        mApplet.ellipse(x + 3, y + 19, 4, 4);
        mApplet.ellipse(x + 3, y + 17, 4, 4);
        mApplet.ellipse(x + 2, y + 15, 4, 4);
        mApplet.ellipse(x, y + 14, 4, 4);
        mApplet.ellipse(x, y + 12, 4, 4);
        mApplet.ellipse(x, y + 10, 4, 4);
    }
   // draw a monkey lv2 with glasses(which makes he become a monkey lv3)
     private void zeichneAffeLv3(float x,float y){
                mApplet.fill(200, 50, 0);
                mApplet.ellipse(x - 10, y - 8, 5, 5);
                mApplet.fill(0, 0);
                mApplet.stroke(0);
                mApplet.ellipse(x - 4, y - 10, 6, 2);
                mApplet.ellipse(x + 4, y - 10, 6, 2);
                mApplet.fill(200, 100, 0);
                mApplet.stroke(0);
                mApplet.ellipse(x + 13, y, 10, 10);
                mApplet.ellipse(x - 13, y, 10, 10);
                mApplet.ellipse(x, y, 25, 20);
                mApplet.noStroke();
                mApplet.ellipse(x + 3, y + 19, 4, 4);
                mApplet.ellipse(x + 3, y + 17, 4, 4);
                mApplet.ellipse(x + 2, y + 15, 4, 4);
                mApplet.ellipse(x, y + 14, 4, 4);
                mApplet.ellipse(x, y + 12, 4, 4);
                mApplet.ellipse(x, y + 10, 4, 4);
}
  //   draw a monkey lv1 having a catapult projectile(which makes he become a monkey lv4)
      private void zeichneAffeLv4(float x,float y){
      
                mApplet.fill(130, 60, 10);
                mApplet.rect(x, y - 35, 5, 45);
                mApplet.rect(x + 30, y - 35, 5, 45);
                mApplet.fill(220, 180, 0);
                mApplet.rect(x + 6, y + 2, 23, 5);
                mApplet.fill(180, 150, 0);
                mApplet.rect(x - 4, y - 28, 4, 8);
                mApplet.rect(x + 36, y - 28, 4, 8);
                mApplet.rect(x + 36, y - 6, 4, 8);
                mApplet.fill(200, 100, 0);
                mApplet.stroke(0);
                mApplet.ellipse(x + 13, y, 10, 10);
                mApplet.ellipse(x - 13, y, 10, 10);
                mApplet.ellipse(x, y, 25, 20);
                mApplet.noStroke();
                mApplet.ellipse(x + 3, y + 19, 4, 4);
                mApplet.ellipse(x + 3, y + 17, 4, 4);
                mApplet.ellipse(x + 2, y + 15, 4, 4);
                mApplet.ellipse(x, y + 14, 4, 4);
                mApplet.ellipse(x, y + 12, 4, 4);
                mApplet.ellipse(x, y + 10, 4, 4);
                mApplet.stroke(1);
                mApplet.fill(220, 180, 0);
                mApplet.rect(x + 6, y - 33, 23, 5);
                mApplet.rect(x + 14, y - 28, 5, 30);
                mApplet.fill(220, 220, 200);
                mApplet.rect(x + 12, y - 7, 9, 3);
                mApplet.rect(x + 12, y - 4, 9, 2);
                mApplet.rect(x + 12, y - 2, 9, 3);
                mApplet.fill(150, 150, 170);
                mApplet.ellipse(x + 17, y + 8, 16, 16);
                mApplet.fill(0, 0, 0);
                mApplet.ellipse(x + 17, y + 8, 10, 10);
                mApplet.triangle(x + 22, y + 7, x + 22, y + 9, x + 25, y + 8);
                mApplet.triangle(x + 12, y + 7, x + 12, y + 9, x + 9, y + 8);
                mApplet.triangle(x + 16, y + 13, x + 18, y + 13, x + 17, y + 16);
                mApplet.triangle(x + 16, y + 3, x + 18, y + 3, x + 17, y);
                mApplet.triangle(x + 14.5f, y + 9, x + 16.5f, y + 11, x + 11, y + 14);
                mApplet.triangle(x + 18, y + 9, x + 20, y + 11, x + 24, y + 15);
                mApplet.triangle(x + 18, y + 4, x + 20, y + 6, x + 23, y + 2);
                mApplet.triangle(x + 14.5f, y + 4, x + 16.5f, y + 6, x + 10.5f, y + 1);
      
      }
}
