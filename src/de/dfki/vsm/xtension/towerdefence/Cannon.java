
package de.dfki.vsm.xtension.towerdefence;

import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.geld;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.turmangeklickt;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.j;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.preismultiplikator;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.ak;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.alleZiele;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.alleWegBoxen;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.alleKanonen;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.mExecutor;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.mDEBUG;
import java.awt.Rectangle;
import ddf.minim.AudioPlayer;
import ddf.minim.Minim;
import java.awt.geom.Ellipse2D;
import java.io.File;
import java.util.ArrayList;
import java.util.Map;
import static javafx.scene.paint.Color.color;
import static javafx.scene.transform.Transform.translate;
import static org.ujmp.core.MatrixFactory.fill;
import static processing.core.PApplet.sq;
import static processing.core.PApplet.sqrt;
import static processing.core.PConstants.BASELINE;
import static processing.core.PConstants.CENTER;
import static processing.core.PConstants.CLOSE;
import static processing.core.PConstants.LEFT;
import static processing.core.PConstants.MITER;
import static processing.core.PConstants.RIGHT;
import static processing.core.PConstants.SQUARE;
import processing.core.PApplet;

/*
 * @author Jan Stieling
 */

class Cannon extends Tower {

    private PApplet mApplet;
    private AudioPlayer mAudioPlayer;
    private Minim minim;
// TowerDefenceExecutor mExecutor;
    Cannon(PApplet applet, float x_0, float y_0, String art_0) {
        super(applet,x_0, y_0, art_0);
        reichweite=160;
        feuerrate=4;
        ag=0;
        kugelsprengkraft=45;
        superGeschosse = new Kugel[10];
        x = x_0;
        y = y_0;
        id = ak;
        mApplet=applet;
       minim=new Minim(mApplet);
      mAudioPlayer = minim.loadFile(mExecutor.mProject.getProjectPath() + File.separator + "snd" + File.separator + "boom.mp3");

       
      
        sichtweite = new Ellipse2D.Float(x - reichweite / 2, y - reichweite / 2, reichweite, reichweite);
  // checks wich pathboxes are in range and put them into "alleWegBoxen"
        for (Map.Entry<String, PathBox> entry : alleWegBoxen.entrySet()) {
              
            PathBox wb = entry.getValue();

            if (sichtweite.contains(wb.x, wb.y) && sichtweite.contains(wb.x + wb.width, wb.y + wb.height)) {
              
                inReichweiteWegBoxen.add(entry.getKey());
            }
        }
        
        ak++;//increase the cannon counter
 
    }

    class Kugel extends Tower.GeschosseSuperklasse {
 
        int kanoneid; //id of the cannon wich created this cannonball
        Ellipse2D sprengradius;
        Kugel(int kanoneid_0, float x_0, float y_0, int sprengkraft1_0, int durchschlagskraft_0, float zielx_0, float ziely_0, int r_0, int g_0, int b_0, String art) {
            super(kanoneid_0, x_0, y_0, sprengkraft1_0, durchschlagskraft_0, zielx_0, ziely_0, r_0, g_0, b_0, art);
            x = x_0;
            y = y_0;
            zielx = zielx_0;
            ziely = ziely_0;
            x1 = (zielx - x) / ((sqrt(sq(zielx - x))) + (sqrt(sq(ziely - y))));//see Tower.GeschosseSuperklasse 
            y1 = (ziely - y) / ((sqrt(sq(zielx - x))) + (sqrt(sq(ziely - y))));
            id = ag;
            kanoneid = kanoneid_0;
            sprengkraft1=sprengkraft1_0;
            durchschlagskraft = 1;
           
           
        }
        

      
//draw the cannonball
        public void zeichne() {
           
            if (explodiert == 1) {//if it is exploding draw explosion
                mApplet.fill(180, 23, 12, 128);
                mApplet.stroke(180, 23, 12, 175);
                mApplet.strokeWeight(0.25f);
                mApplet.pushMatrix();
                mApplet.strokeJoin(MITER);
                mApplet.strokeCap(SQUARE);
                mApplet.scale(sprengkraft1 / 10);
                x = x / (sprengkraft1 / 10);
                y = y / (sprengkraft1 / 10);
                mApplet.beginShape();
                mApplet.vertex(x - 2, y - 5);
                mApplet.bezierVertex(x - 1, y - 2, x + 1, y - 2, x + 3, y - 5);
                mApplet.bezierVertex(x + 1, y - 2, x + 2, y, x + 5, y);
                mApplet.bezierVertex(x + 2, y, x + 1, y + 2, x + 3, y + 5);
                mApplet.bezierVertex(x + 1, y + 2, x - 1, y + 2, x - 3, y + 5);
               mApplet.bezierVertex(x - 1, y + 2, x - 2, y, x - 5, y);
                mApplet.bezierVertex(x - 2, y, x - 1, y - 2, x - 2, y - 5);
                mApplet.endShape();
                mApplet.popMatrix();
                x = x * (sprengkraft1 / 10);
                y = y * (sprengkraft1 / 10);
                sprengkraft1 -= 2; // sprengkraft1 ist die Sprengkraft der Kugel
            } else {
            //if not draw cannonball
                  mApplet.fill(r, g, b);
                 mApplet.ellipse(x, y, 5, 5);
            }
           
        }
//makes the cannonball moves and hit targets
        public void fliegen() {
         
            if (explodiert == 0) {//only if it is not exploding of course
                for (Map.Entry<String, PathBox> entry : alleWegBoxen.entrySet()) {
                    PathBox wb = entry.getValue();

                    Rectangle r = new Rectangle(wb.x - unschaerfe / 2, wb.y - unschaerfe / 2, wb.width + unschaerfe, wb.height + unschaerfe);

                    if (r.contains((new Float(x).intValue()), (new Float(y).intValue()))) {//if theire is a target in the pathbox...
                       
                    
                        if (wb.zielSpeicher.size() > 0) {
                            Targets z = null;
                         
                            for (Map.Entry<String,Targets> zielEintrag : wb.zielSpeicher.entrySet()) {
                               
                                z = (z == null) ? zielEintrag.getValue() : z;
                            }
                            if (z != null) {//if thiere is a target the cannonball explodes 
                                explosion();
                            }
                        }
                    }
                }
                //this makes the cannonball moving
                x += x1 * 10;
                y += y1 * 10;
               
                  }
            //if the ball is to far away or finished its explosion it will be deleted
            if (sqrt(sq(alleKanonen[kanoneid].x - x) + sq(alleKanonen[kanoneid].y - y)) > 240 || sprengkraft1 <= 0) {
                alleKanonen[kanoneid].geschossloeschen(id);
            }
            //this calls its draw method
            zeichne();
        }
//the explosion
        void explosion() {
            explodiert = 1;
           
            sprengradius = new Ellipse2D.Float(x - sprengkraft1, y - sprengkraft1, sprengkraft1 * 2, sprengkraft1 * 2);
            mApplet.fill(200, 180, 0 );
            mApplet.ellipse(x, y, sprengkraft1 * 2, sprengkraft1 * 2);
             mAudioPlayer= minim.loadFile(mExecutor.mProject.getProjectPath() + File.separator + "snd" + File.separator + "boom.mp3");         
            mAudioPlayer.play();

            //this damages all targets in the explosion radius
            for (Map.Entry<String, Targets> entry : alleZiele.entrySet()) {

                if (sprengradius.contains(entry.getValue().x, entry.getValue().y) == true) {

                    alleKanonen[kanoneid].anzahlTreffer++;
                    geld++;
                    entry.getValue().lv--;
                }

            }

        }

    }

   

   
//this makes the cannon shoot
    public void schiessen() {
      //it is wise to search a target before shooting
        zielen();

        if (zielgefunden == true && 400 / feuerrate <= verzoegerung) {//if it has a target and finished reload 
 
            verzoegerung = 0;
            int kraftfarbe = (new Float(kugelsprengkraft * 1.5f)).intValue();//the color of the bal depends from it explosions power
          
            superGeschosse[ag] = new Kugel(id, x, y, kugelsprengkraft, 1, ziel.x, ziel.y, kraftfarbe, 0, 0, "kanonenkugel");

     
        }
//it reloads
        verzoegerung++;
    }
//this draw the explosion
    void zeichneExplosion(float x, float y, float scale) {
        mApplet.fill(180, 23, 12, 128);
        mApplet.stroke(180, 23, 12, 175);
        mApplet.strokeWeight(0.25f);
        mApplet.pushMatrix();
        mApplet.strokeJoin(MITER);
        mApplet.strokeCap(SQUARE);
        mApplet.scale(scale);
        x = x / scale;
        y = y / scale;
    
        mApplet.beginShape();
        mApplet.vertex(x - 2, y - 5);
        mApplet.bezierVertex(x - 1, y - 2, x + 1, y - 2, x + 3, y - 5);
        mApplet.bezierVertex(x + 1, y - 2, x + 2, y, x + 5, y);
        mApplet.bezierVertex(x + 2, y, x + 1, y + 2, x + 3, y + 5);
        mApplet.bezierVertex(x + 1, y + 2, x - 1, y + 2, x - 3, y + 5);
        mApplet.bezierVertex(x - 1, y + 2, x - 2, y, x - 5, y);
        mApplet.bezierVertex(x - 2, y, x - 1, y - 2, x - 2, y - 5);
        mApplet.endShape();
        mApplet.popMatrix();
    }
// this draw the speechbubble
    void zeichneSprechblase(float x, float y, String text) {
        mApplet.curveTightness(0.05f);
        float textHoehe = (mApplet.textAscent() + mApplet.textDescent()) * 1.75f;
        float tBreite = mApplet.textWidth(text);
        float einDrittel = tBreite / 3.0f;
        // Das Ganze darf nicht kleiner als 24 sein.
        einDrittel = (einDrittel < textHoehe) ? textHoehe : einDrittel;
        float h = einDrittel;
        float b = einDrittel * 2;
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
//this draw the cannon and call the other draw methods
    public void zeichne() {

        if (gedanken != "") {
            boolean zeichneSb = (mAnzeigeManager != null && mAnzeigeManager.mTimer.isRunning());
            if (zeichneSb) {
                zeichneSprechblase(x + 24, y - 60, gedanken);
            } else {
                gedanken = "";
            }
        }

        if (habeExplosion == true) {

            boolean zeichneEx = (mAnzeigeManager != null && mAnzeigeManager.mTimer.isRunning());
            if (zeichneEx) {

                zeichneExplosion(x, y, kugelsprengkraft / 2);
            } else {
                habeExplosion = false;
            }
        }
        // makes the tower look on his target
        float winkel = 0f;
      
        if (zielgefunden) {
            float azVx = ziel.x - x;
            float azVy = ziel.y - y;
            winkel = new Float(Math.atan2(azVx, azVy)) + new Float(Math.PI);
        }
        mApplet.pushMatrix();
        mApplet.translate(x, y);
        mApplet.rotate(-winkel);
        mApplet.translate(-x, -(y));
        
        //here the cannon is drawn
                y-=16;   
                mApplet.scale(1);

               mApplet.curveTightness(0.01f);

                mApplet.strokeWeight(1);
                mApplet.stroke(0);
                mApplet.fill(0);
                mApplet.beginShape();
                mApplet.curveVertex(x - 10, y - 8);
                mApplet.curveVertex(x - 10, y - 10);
                mApplet.curveVertex(x + 10, y - 10);
                mApplet.curveVertex(x + 12, y - 7);

                mApplet.curveVertex(x + 15, y + 20);

                mApplet.curveVertex(x + 15, y + 30);
                mApplet.curveVertex(x + 6, y + 40);
                mApplet.curveVertex(x - 6, y + 40);

                mApplet.curveVertex(x - 15, y + 30);
                mApplet.curveVertex(x - 15, y + 20);
                mApplet.curveVertex(x - 10, y - 10);

                mApplet.endShape(CLOSE);
                mApplet.strokeWeight(0.45f);
                mApplet.beginShape();
                mApplet.vertex(x - 11, y - 5);
                mApplet.bezierVertex(x - 20, y - 15, x + 20, y - 15, x + 13, y - 5);
                mApplet.bezierVertex(x + 15, y - 12, x - 15, y - 12, x - 11, y - 5);

                mApplet.endShape(CLOSE);
                mApplet.strokeWeight(0.8f);
                mApplet.ellipse(x, y + 45, 6, 6);
                mApplet.fill(140, 100, 0);
                mApplet.rect(x - 25, y + 22, 4, 7);
                mApplet.rect(x - 21, y + 10, 6, 31);
                mApplet.rect(x + 21, y + 22, 4, 7);
                mApplet.rect(x + 15, y + 10, 6, 31);

        y+=16;
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

   
//this draw the upgrade menu and allows the player to upgrade the cannon
    public void upgraden() {
        if( turmangeklickt == 0){//this deselect this specific tower when no tower is selected
         ausgewaehlt = 0;
        
        }
        if (mApplet.mousePressed == true) {//if the tower is selected
            if (mApplet.mouseButton == RIGHT && sqrt(sq(x - mApplet.mouseX) + sq(y - mApplet.mouseY)) < 10) {
                turmangeklickt = 1;
                ausgewaehlt = 1;
            }
        }
        //if the conditions are fullfilled, you pay the price and the tower levels up, what change (depends on its level) some attributes
        if (mApplet.mousePressed == true) {
            if (mApplet.mouseButton == LEFT && turmangeklickt == 1 && ausgewaehlt == 1) {
                if (mApplet.mouseX > x + 27 && mApplet.mouseY > y && mApplet.mouseX < x + 71 && mApplet.mouseY < y + 10) {
                    j = 0;
                    switch (lv) {
                        case 1:
                            if (geld >= 340 * preismultiplikator) {
                                kugelsprengkraft += 15;
                                geld -= 340 * preismultiplikator;
                                lv++;
                            }
                            break;
                        case 2:
                            if (geld >= 170 * preismultiplikator) {
                                reichweite += 30;
                                for (Map.Entry<String, PathBox> entry : alleWegBoxen.entrySet()) {
                                    PathBox wb = entry.getValue();

                                   
                                
                                    if (sichtweite.contains(wb.x, wb.y) && sichtweite.contains(wb.x + wb.width, wb.y + wb.height)) {
                                  
                                        inReichweiteWegBoxen.add(entry.getKey());
                                    }
                                }
                                geld -= 170 * preismultiplikator;
                                lv++;
                            }
                            break;
                        case 3:
                            if (geld >= 400 * preismultiplikator) {
                                feuerrate += 1;
                                kugelsprengkraft += 20;
                                reichweite += 20;
                                for (Map.Entry<String, PathBox> entry : alleWegBoxen.entrySet()) {
                                    PathBox wb = entry.getValue();
                               
                                 
                                    if (sichtweite.contains(wb.x, wb.y) && sichtweite.contains(wb.x + wb.width, wb.y + wb.height)) {
                          
                                        inReichweiteWegBoxen.add(entry.getKey());
                                    }
                                }
                                geld -= 400 * preismultiplikator;
                                lv++;
                            }
                            break;
                    }
                    turmangeklickt = 0;
                    ausgewaehlt = 0;
                }
            }
        }
         //this draws the upgrade menu
        if (turmangeklickt == 1 && ausgewaehlt == 1) {
            mApplet.fill(255);
            mApplet.noStroke();
            mApplet.fill(125, 125, 255, 128);
             mApplet.ellipse(x, y, reichweite / 2, reichweite / 2);
               mApplet.fill(255);
            switch (lv) {
                case 1:
                    mApplet.rect(x + 12, y, 100, 10);
                    mApplet.fill(0);
                    mApplet.text("blast area+ " + (int) (340 * preismultiplikator), x + 13, y + 10);
                    break;
                case 2:
                    mApplet.rect(x + 12, y, 70, 10);
                    mApplet.fill(0);
                    mApplet.text("range+ " + (int) (170 * preismultiplikator), x + 13, y + 10);
                    break;
                case 3:
                    mApplet.rect(x + 12, y, 100, 10);
                   mApplet.fill(0);
                    mApplet.text("blast area+ " + (int) (400 * preismultiplikator), x + 13, y + 10);
                    break;

            }
            mApplet.fill(255);
        }
    }

 
}
