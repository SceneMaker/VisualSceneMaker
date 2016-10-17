
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
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.mExecutor;
import java.io.File;

/**
 *
 * @author Jan
 */
public class WizardMonkey extends Tower{
   private AudioPlayer mAudioPlayer;
    private Minim minim;

    private PApplet mApplet;
  
    int feuerrate = 7, geschossdurchschlagskraft = 2, lv = 1, reichweite = 120;
 
    Magiekugel[] superGeschosse;
   
  
    String gedanken = "";
    Visualisation mAnzeigeManager;

    WizardMonkey(PApplet applet, float x_0, float y_0,String art_0) {
          super(applet,x_0, y_0, art_0);
        superGeschosse = new Magiekugel[10];
      
        x = x_0;
        y = y_0;
        id = aza;//the wizard gets a number
        aza++;//increase the wizardmonkey counter
        mApplet=applet;
         minim=new Minim(mApplet);
      mAudioPlayer = minim.loadFile(mExecutor.mProject.getProjectPath() + File.separator + "snd" + File.separator + "boom.mp3");

  
        sichtweite = new Ellipse2D.Float(x - reichweite / 2, y - reichweite / 2, reichweite, reichweite);

         for (Map.Entry<String, PathBox> entry : alleWegBoxen.entrySet()) {
            PathBox wb = entry.getValue();

            // nur wegboxen nehmen, die komplett in sichtweite sind
            if (sichtweite.contains(wb.x, wb.y) && sichtweite.contains(wb.x + wb.width, wb.y + wb.height)) {
               
                inReichweiteWegBoxen.add(entry.getKey());
            }
        }

        // Verbindung zu SceneMaker
//            if (sRunTime.hasVariable(pd, "anzahlAffen")) {
//                // Set The Variable Now
//                sRunTime.setVariable(pd, "anzahlAffen", aa);
//            } else {
//                 }
    }

    class Magiekugel extends Tower.GeschosseSuperklasse {

        float x, x1, y, y1, zielx, ziely;
        int id, affeid, durchschlagskraft, unschaerfe = 10, lv, explodiert = 0, sprengkraft1 = 30;
        Ellipse2D sprengradius;
        String art;

        Magiekugel(int affeid_0, float x_0, float y_0,int sprengkraft_0, int durchschlagskraft_0,float zielx_0, float ziely_0,int r_0,int g_0,int b_0,String art_0) {
          super(affeid_0, x_0, y_0, sprengkraft_0, durchschlagskraft_0, zielx_0, ziely_0, r_0, g_0, b_0, art_0);

            x = x_0;
            y = y_0;
            zielx = zielx_0;
            ziely = ziely_0;
            x1 = (zielx - x) / ((sqrt(sq(zielx - x))) + (sqrt(sq(ziely - y))));
            y1 = (ziely - y) / ((sqrt(sq(zielx - x))) + (sqrt(sq(ziely - y))));
            id = ag;
            affeid = affeid_0;
            art = art_0;
             
            //makes sure that the firebolts("feuerkugel") and magicbolts("magiekugel") have the attributes they should have
          if(art!="feuerkugel"){
            if (alleZaubereraffen[affeid].lv > 1&&art!="feuerkugel") {

                lv = 2;
                durchschlagskraft = 7;
                art = "magiekugel2";
            } else {
                lv = 1;
                durchschlagskraft = 2;
                art = "magiekugel1";
            }
            
         }
            if (art == "feuerkugel") {
                durchschlagskraft = 1;

            }
           
                 
          }
//this makes the firebolts explosion
        void explosion() {
            explodiert = 1;
            sprengradius = new Ellipse2D.Float(x - 30, y - 30, 30 * 2, 30 * 2);
            mApplet.fill(0, 0, 200);
          
            mApplet.ellipse(x, y, 30 * 2, 30 * 2);
          
             mAudioPlayer= minim.loadFile(mExecutor.mProject.getProjectPath() + File.separator + "snd" + File.separator + "boom.mp3");
             mAudioPlayer.play();

            for (Map.Entry<String, Targets> entry : alleZiele.entrySet()) {

                if (sprengradius.contains(entry.getValue().x, entry.getValue().y) == true) {

                    alleZaubereraffen[affeid].anzahlTreffer++;
                    geld++;
                    entry.getValue().lv--;
                }

            }

        }
         //this draws the bolts
        public void zeichne() {
          
      
  
            if (explodiert == 1) {//if the bolt is exploding it draws a explosion

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
                sprengkraft1 -= 2; 
            }
            if (art == "feuerkugel" && explodiert != 1) {//if not it draws a firebolt
                mApplet.fill(255, 0, 0);
                mApplet.ellipse(x, y + 8, 10, 10);
                

            } else {//if it is a magicbolt it draws a magicbolt whose appearance depends from it lv
                if (lv == 1) {
                    mApplet.fill(250, 240, 0);
                    mApplet.ellipse(x, y, 5, 5);

                } else {
                    mApplet.fill(0, 0, 150);
                    mApplet.ellipse(x, y, 8, 8);
                    mApplet.fill(250, 240, 0);
                    mApplet.ellipse(x, y, 5, 5);

                }
            }

        }
       //makes the bolt moves and hits target
        public void fliegen() {
 
            for (Map.Entry<String, PathBox> entry : alleWegBoxen.entrySet()) {
                PathBox wb = entry.getValue();

                Rectangle r = new Rectangle(wb.x - unschaerfe / 2, wb.y - unschaerfe / 2, wb.width + unschaerfe, wb.height + unschaerfe);

                if (r.contains((new Float(x).intValue()), (new Float(y).intValue()))) {

                  
                    if (wb.zielSpeicher.size() > 0) {
                        Targets z = null;
                     //if it hit a target its behavior depends on its kind("art") firebolts explode magicbolts not
                        switch (art) {
                           
                             case "magiekugel1":
                              
                            
                                for (Map.Entry<String, Targets> zielEintrag : wb.zielSpeicher.entrySet()) {
                                
                                    z = (z == null) ? zielEintrag.getValue() : z;
                                }
                                if (z != null&&durchschlagskraft>0) {
                                    alleZaubereraffen[affeid].anzahlTreffer++;
                                    durchschlagskraft--;
                                    geld++;
                                    z.lv--;
                                }
                                break; case "magiekugel2":
                               // hier kommt noch was besseres, wie das Ziel ausgewählt wird. Jetzt nehmen wir das erste
                                for (Map.Entry<String, Targets> zielEintrag : wb.zielSpeicher.entrySet()) {
                                    z = (z == null) ? zielEintrag.getValue() : z;
                                }
                                if (z != null&&durchschlagskraft>0) {
                                    alleZaubereraffen[affeid].anzahlTreffer++;
                                    durchschlagskraft--;
                                    geld++;
                                    z.lv--;
                                }
                                break;
                            case "feuerkugel":
                                
                                for (Map.Entry<String,Targets> zielEintrag : wb.zielSpeicher.entrySet()) {
                              
                                    z = (z == null) ? zielEintrag.getValue() : z;
                                }
                                if (z != null) {
                                    explosion();
                                }

                                break;
                        }
                    }
                }
            }
        // makes the bolt moves
            x += x1 * 10;
            y += y1 * 10;

            // delete the bolt if it is to far away, has no penetration power anymore or finished its explosion
            if (sqrt(sq(alleZaubereraffen[affeid].x - x) + sq(alleZaubereraffen[affeid].y - y)) > 180 * sqrt(geschossdurchschlagskraft) || durchschlagskraft <= 0 || sprengkraft1 <= 0) {
                alleZaubereraffen[affeid].geschossloeschen(id);
            }
        }
    }

    public void implantiereGedanken(String text) {
        mAnzeigeManager = new Visualisation(mApplet);
        gedanken = text;
    }

  
   //makes the tower shoots
    public void schiessen() {
        //targeting
        zielen();

        if (zielgefunden == true && 400 / feuerrate <= verzoegerung) {//if it has a target and finnished reloading it shoots

            verzoegerung = 0;
          
            if (lv > 3) {//if it has level 4 it may shoot a fireball in addition to a magic ball 
                  
                int i = (int) mApplet.random(5);
              
               if (i >= 3) {
                  
                    superGeschosse[ag] = new Magiekugel(id, x, y,0,0, ziel.x, ziel.y,0,0,0, "feuerkugel");
                 
                  
               }
                superGeschosse[ag] = new Magiekugel(id, x, y,0,0, ziel.x, ziel.y,0,0,0, "magiekugel");
            } else {
                superGeschosse[ag] = new Magiekugel(id, x, y,0,0, ziel.x, ziel.y,0,0,0, "magiekugel");
            }
           
        }

        verzoegerung++;
    }

   
    //draws the wizardmonkey
    public void zeichne() {

      

        //makes the wizard turn to its target
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
      //draw a wizardmonkey which appearence depends on its level
        switch (lv) {
            case 1:
           zeichneZaubereraffeLv1();
                break;
            case 2:
                zeichneZaubereraffeLv2();
               
                break;
            case 3:
          zeichneZaubereraffeLv3();
              
                break;
            case 4:
             zeichneZaubereraffeLv4();
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
                mApplet.color(255, 0, 0, 128);
                mApplet.line(x, y, superGeschosse[gc].x, superGeschosse[gc].y);
            }
        }

    }
// allows the player to upgrade the wizardmonkey and draw the upgrade menu, details see cannon
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
                            if (geld >= 225 * preismultiplikator) {

                                geld -= 225 * preismultiplikator;
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

        if (turmangeklickt == 1 && ausgewaehlt == 1) {
       
            mApplet.fill(125, 125, 255, 128);
            mApplet.ellipse(x, y, reichweite / 2, reichweite / 2);
             mApplet.fill(255);
            switch (lv) {
                case 1:
                    mApplet.rect(x + 12, y, 110, 10);
                    mApplet.fill(0);
                    mApplet.text("penetration+ " + (int) (225 * preismultiplikator), x + 13, y + 10);
                    break;
                case 2:
                    mApplet.rect(x + 12, y, 70, 10);
                    mApplet.fill(0);
                    mApplet.text("range+ " + (int) (225 * preismultiplikator), x + 13, y + 10);
                    break;
                case 3:
                    mApplet.rect(x + 12, y, 85, 10);
                    mApplet.fill(0);
                    mApplet.text("fireball " + (int) (225 * preismultiplikator), x + 13, y + 10);
                    break;

            }
             mApplet.fill(255);
        }
    }
    //
      public void geschossloeschen(int z) {
         if(z!=ag-1){
        superGeschosse[z] = superGeschosse[z++];
        }
        ag--;
         
    }
    //draw a wizardmonkey lv1
void zeichneZaubereraffeLv1(){
 

                mApplet.fill(0);
                mApplet.rect(x + 8, y - 15, 1, 10);
                mApplet.fill(200, 100, 0);
                mApplet.stroke(0);
                mApplet.ellipse(x + 13, y, 10, 10);
                mApplet.ellipse(x + -13, y, 10, 10);
                mApplet.ellipse(x + 0, y, 25, 20);
                mApplet.noStroke();
                mApplet.fill(200, 100, 0);
                mApplet.ellipse(x + 3, y + 19, 4, 4);
                mApplet.ellipse(x + 3, y + 17, 4, 4);
                mApplet.ellipse(x + 2, y + 15, 4, 4);
                mApplet.ellipse(x + 0, y + 14, 4, 4);
                mApplet.ellipse(x + 0, y + 12, 4, 4);
                mApplet.ellipse(x + 0, y + 10, 4, 4);
                mApplet.fill(25, 0, 150);
                mApplet.ellipse(x + 0, y, 20, 20);
                mApplet.fill(250, 250, 0);
                mApplet.ellipse(x + 0, y, 3, 3);

}
 //draw a wizardmonkey lv1 with another hat tip(which makes he became a wizardmonkey lv2)
               void zeichneZaubereraffeLv2(){
                mApplet.fill(0);
                mApplet.rect(x + 8, y - 15, 1, 10);
                mApplet.fill(200, 100, 0);
                mApplet.stroke(0);
                mApplet.ellipse(x + 13, y, 10, 10);
                mApplet.ellipse(x + -13, y, 10, 10);
                mApplet.ellipse(x + 0, y, 25, 20);
                mApplet.noStroke();
                mApplet.fill(200, 100, 0);
                mApplet.ellipse(x + 3, y + 19, 4, 4);
                mApplet.ellipse(x + 3, y + 17, 4, 4);
                mApplet.ellipse(x + 2, y + 15, 4, 4);
                mApplet.ellipse(x + 0, y + 14, 4, 4);
                mApplet.ellipse(x + 0, y + 12, 4, 4);
                mApplet.ellipse(x + 0, y + 10, 4, 4);
                mApplet.fill(25, 0, 150);
                mApplet.ellipse(x + 0, y, 20, 20);
                mApplet.fill(250, 0, 0);
                mApplet.ellipse(x + 0, y, 5, 5);
                mApplet.fill(250, 250, 0);
                mApplet.ellipse(x + 0, y, 3, 3);

} 
//draw a wizardmonkey lv2 with glasses(which makes he became a wizardmonkey lv3)
void zeichneZaubereraffeLv3(){
                mApplet.fill(0, 0);
                mApplet.stroke(0);
                mApplet.ellipse(x - 4, y - 10, 6, 2);
                mApplet.ellipse(x + 4, y - 10, 6, 2);
                mApplet.fill(0);
                mApplet.rect(x + 8, y - 15, 1, 10);
                mApplet.fill(200, 100, 0);
                mApplet.stroke(0);
                mApplet.ellipse(x + 13, y, 10, 10);
                mApplet.ellipse(x + -13, y, 10, 10);
                mApplet.ellipse(x + 0, y, 25, 20);
                mApplet.noStroke();
                mApplet.fill(200, 100, 0);
                mApplet.ellipse(x + 3, y + 19, 4, 4);
                mApplet.ellipse(x + 3, y + 17, 4, 4);
                mApplet.ellipse(x + 2, y + 15, 4, 4);
                mApplet.ellipse(x + 0, y + 14, 4, 4);
                mApplet.ellipse(x + 0, y + 12, 4, 4);
                mApplet.ellipse(x + 0, y + 10, 4, 4);
                mApplet.fill(25, 0, 150);
                mApplet.ellipse(x + 0, y, 20, 20);
                mApplet.fill(250, 0, 0);
                mApplet.ellipse(x + 0, y, 5, 5);
                mApplet.fill(250, 250, 0);
                mApplet.ellipse(x + 0, y, 3, 3);

}
//draw a wizardmonkey lv3 with a red hat (which makes he became a wizardmonkey lv4)
void zeichneZaubereraffeLv4(){
                mApplet.fill(0, 0);
                mApplet.stroke(0);
                mApplet.ellipse(x - 4, y - 10, 6, 2);
                mApplet.ellipse(x + 4, y - 10, 6, 2);
                mApplet.fill(255, 0, 0);
                mApplet.rect(x + 8, y - 15, 1, 10);
                mApplet.fill(200, 100, 0);
                mApplet.stroke(0);
                mApplet.ellipse(x + 13, y, 10, 10);
                mApplet.ellipse(x + -13, y, 10, 10);
                mApplet.ellipse(x + 0, y, 25, 20);
                mApplet.noStroke();
                mApplet.fill(200, 100, 0);
                mApplet.ellipse(x + 3, y + 19, 4, 4);
                mApplet.ellipse(x + 3, y + 17, 4, 4);
                mApplet.ellipse(x + 2, y + 15, 4, 4);
                mApplet.ellipse(x + 0, y + 14, 4, 4);
                mApplet.ellipse(x + 0, y + 12, 4, 4);
                mApplet.ellipse(x + 0, y + 10, 4, 4);
                mApplet.fill(255, 0, 0);
                mApplet.ellipse(x + 0, y, 20, 20);
                mApplet.fill(25, 0, 150);
                mApplet.ellipse(x + 0, y, 5, 5);
                mApplet.fill(250, 250, 0);
                mApplet.ellipse(x + 0, y, 3, 3);

}
//  
}
