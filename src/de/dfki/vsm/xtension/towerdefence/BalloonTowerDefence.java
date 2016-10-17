package de.dfki.vsm.xtension.towerdefence;

import ddf.minim.AudioPlayer;
import ddf.minim.Minim;
//import de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.Monkey;
import java.awt.Rectangle;
import java.awt.geom.Ellipse2D;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;
import processing.core.*;
import static processing.core.PApplet.concat;
import static processing.core.PApplet.print;

import static processing.core.PApplet.println;
import static processing.core.PApplet.sq;
import static processing.core.PApplet.sqrt;
import static processing.core.PConstants.BASELINE;
import static processing.core.PConstants.CENTER;
import static processing.core.PConstants.CLOSE;
import static processing.core.PConstants.LEFT;
import static processing.core.PConstants.MITER;
import static processing.core.PConstants.RIGHT;
import static processing.core.PConstants.SQUARE;

public class BalloonTowerDefence extends PApplet {

    static int ak = 0,/*amount of cannons*/ aw = 0, /*amount of pathboxes*/ aa = 0/*amount of monkeys*/, aza = 0/*amount of wizardmonkeys*/, az /*amount of targets*/,  geld = 950/*gold*/, leben = 50/*live*/, turmangeklickt = 0/*if a tower is selected*/;
   static double preismultiplikator = 0.7;//to manipulate the prices // float[] affex/* x koordinate eines affen*/, affey/* Y koordinate eines affen */, zielx, ziely, gzielx/*koordinaten des anvisierten zieles */, gziely, geschossx, geschossy, gtx/*bewegung des geschosses */, gty, ga/* ebenfalls zu welchem affen ein geschoss geh\u00f6rt?*/;
    static Monkey[] alleAffen;//an array for the monkeys
    static Cannon[] alleKanonen;//an array for the cannons
    static WizardMonkey[] alleZaubereraffen;//an array for the wizardmonkeys
    boolean pausiert = false;//is game paused?
   static HashMap<String, Targets> alleZiele = new HashMap<>();//all targets
   static HashMap<String, PathBox> alleWegBoxen = new HashMap<>();//all pathtiles
    public static boolean mDEBUG = false;
   static int j;//make sure that you place only one thing at the same time
 
  static  PApplet mMalgrund = null;
    Minim minim;
    AudioPlayer mAudioPlayer;

    // VSM Agent Handler
   static TowerDefenceExecutor mExecutor;

  
    int mUID = 0; // eindeutige gloable Nummer

    public BalloonTowerDefence(TowerDefenceExecutor executor) {
        mExecutor = executor;
    }

    public void setup() throws NullPointerException {
        size(900, 600);
        frameRate(240);
        mMalgrund = this;
        minim = new Minim(this);

        alleAffen = new Monkey[20];
        alleKanonen = new Cannon[20];
        alleZaubereraffen = new WizardMonkey[20];
     

         mAudioPlayer = minim.loadFile(mExecutor.mProject.getProjectPath() + File.separator + "snd" + File.separator + "boom.mp3");

        // here the path is constructed
      
        int ii = 0;
        while (ii < 49) {
            alleWegBoxen.put("wb" + ii, new PathBox(this, aw, ii * 10, 400, 1, 0));
            ii++;
        }
        while (ii < 54) {
            alleWegBoxen.put("wb" + ii, new PathBox(this, aw,490, 400 - ((ii - 49) * 10), 0, -1));
            ii++;
        }
        while (ii < 98) {
            alleWegBoxen.put("wb" + ii, new PathBox(this, aw, 490 - ((ii - 54) * 10), 350, -1, 0));
            ii++;
        }
        while (ii < 103) {
            alleWegBoxen.put("wb" + ii, new PathBox(this, aw,50, 350 - ((ii - 98) * 10), 0, -1));
            ii++;
        }
        while (ii < 188) {
            alleWegBoxen.put("wb" + ii, new PathBox(this, aw,50 + ((ii - 103) * 10), 300, 1, 0));
            ii++;
        }

    }

    public static boolean startSpiel = false;
//to start the game
    public static void startSpiel() {
        startSpiel = true;
    }
//to stop the game
    public static void stopSpiel() {
        startSpiel = false;
    }
//to make the towers say something
    public static void lasseAffeSprechen(int ape, String text) {
        alleAffen[ape].implantiereGedanken(text);
    }
//to ask the tower how many targets it has
    public static int frageAffeNachSeinenZielen(int ape) {
        return alleAffen[ape].potentielleZiele.size();
    }
// to ask the tower after his hits
    public static int frageAffeNachSeinenTreffern(int ape) {
        return alleAffen[ape].anzahlTreffer;
    }

    public void keyPressed() {
        if (key == 'b' || 'B' == key) {
            startSpiel = true;
            redraw();
        }
    }

    public void draw() {
        if (!startSpiel) {
            background(255, 0, 0);
            stroke(0, 0, 0);
            text("Dr\u00fccke \"B\" um zu beginnen", 100, 100);

        } else {
//this happens when the game started
            background(50, 200, 0);
            stroke(0);

            resetknopf();//the reset button
            pauseknopf();//the pause button
            //makes that targtes only appear if you have at least one tower
            if (aa == 0 && ak == 0 && aza == 0) {

                frameCount = 0;
            }
            if (frameCount > 24800) {
                frameCount = 24800;
            }
            if (frameCount > 24600 && az == 0) {//if you are in the last level and theire are no targets anymore you win
                textSize(48);
                text("you won", 440, 300);
            } else if (leben > 0) {//if you have not won nor loose

              textSize(12);
                turmsetzen();//to set the towers
             
                zeichneWegBoxen();// draw path
                //to deselect a tower
                if (turmangeklickt == 1 && mousePressed == true && mouseButton == RIGHT) {
                    turmangeklickt = 0;

                }

             
                if (!pausiert) {//if the game not paused 
                    zielsetzen();//sets targets
                    handlung(); // move targets and make tower shoots
                } else {//if the game is paused...
                    frameCount--; //time will be stopped

                }
             
                zieleBeseitigen();   // destroyed targets are here removed

                spielerEingabe();// allows to move a part of the world
                zeichne();//draw everything
            } else {//if you have no life anymore you loose
                textSize(48);
                text("you loosed", 440, 300);
            }
        }
    }
    // create an id
    int holeEindeutigeID() {
        mUID++;
        return mUID;
    }
//draw pathboxes
    void zeichneWegBoxen() {
        for (Entry<String, PathBox> entry : alleWegBoxen.entrySet()) {
            entry.getValue().zeichne();
        }
    }
//remove the targets
    void zieleBeseitigen() {
        ArrayList<String> kaputteZiele = new ArrayList<>();

        synchronized (alleZiele) {   

            for (Entry<String, Targets> entry : alleZiele.entrySet()) {
                if (entry.getValue().lv <= 0) {
         
                    kaputteZiele.add(entry.getKey());
                }
            }

            for (String key : kaputteZiele) {
                alleZiele.remove(key);
            }
        }

        //this delete the destroyed targets 
        for (Entry<String, PathBox> entry : alleWegBoxen.entrySet()) {
            HashMap<String, Targets> wegBoxZiele = entry.getValue().zielSpeicher;
            synchronized (wegBoxZiele) {
                kaputteZiele = new ArrayList<>();
                if (!wegBoxZiele.isEmpty()) {
                    for (Entry<String, Targets> toterZielEintrag : wegBoxZiele.entrySet()) {
                        Targets z = toterZielEintrag.getValue();
                        if (z.lv <= 0) {
                            kaputteZiele.add(toterZielEintrag.getKey());
                        }
                    }
                    for (String key : kaputteZiele) {
                        wegBoxZiele.remove(key);
                    }
                }
            }
        }

    }



//   
//draw a monkey lv1
  public void zeichneAffeLv1(float x, float y) { 
        fill(0);
        ellipse(x - 10, y - 8, 5, 5);
        fill(200, 100, 0);
        stroke(0);
        ellipse(x + 13, y, 10, 10);
        ellipse(x - 13, y, 10, 10);
        ellipse(x, y, 25, 20);
        noStroke();
        ellipse(x + 3, y + 19, 4, 4);
        ellipse(x + 3, y + 17, 4, 4);
        ellipse(x + 2, y + 15, 4, 4);
        ellipse(x, y + 14, 4, 4);
        ellipse(x, y + 12, 4, 4);
        ellipse(x, y + 10, 4, 4);

    }
//draw a cannon lv1
  void zeichneKanoneLv1(float x, float y){
  
        curveTightness(0.01f);
        fill(35, 35, 50);

        strokeWeight(1);
        stroke(0);
        beginShape();
        curveVertex(x - 10, y - 8);
        curveVertex(x - 10, y - 10);
        curveVertex(x + 10, y - 10);
        curveVertex(x + 12, y - 7);

        curveVertex(x + 15, y + 20);

        curveVertex(x + 15, y + 30);
        curveVertex(x + 6, y + 40);
        curveVertex(x - 6, y + 40);

        curveVertex(x - 15, y + 30);
        curveVertex(x - 15, y + 20);
        curveVertex(x - 10, y - 10);

        endShape(CLOSE);
        strokeWeight(0.45f);
        beginShape();
        vertex(x - 11, y - 5);
        bezierVertex(x - 20, y - 15, x + 20, y - 15, x + 13, y - 5);
        bezierVertex(x + 15, y - 12, x - 15, y - 12, x - 11, y - 5);

        endShape(CLOSE);
        strokeWeight(0.8f);
        ellipse(x, y + 45, 6, 6);
        fill(140, 100, 0);
        rect(x - 25, y + 22, 4, 7);
        rect(x - 21, y + 10, 6, 31);
        rect(x + 21, y + 22, 4, 7);
        rect(x + 15, y + 10, 6, 31);

}
//draw a wizardmonkey lv1
  void zeichneZaubererAffeLv1(float x,float y){
    fill(200, 100, 0);
        stroke(0);
        ellipse(x + 33, y, 10, 10);
        ellipse(x + 7, y, 10, 10);

        ellipse(x + 20, y, 25, 20);

        noStroke();
        fill(200, 100, 0);
        ellipse(x + 23, y + 19, 4, 4);
        ellipse(x + 23, y + 17, 4, 4);
        ellipse(x + 22, y + 15, 4, 4);
        ellipse(x + 20, y + 14, 4, 4);
        ellipse(x + 20, y + 12, 4, 4);
        ellipse(x + 20, y + 10, 4, 4);
        fill(25, 0, 150);
        ellipse(x + 20, y, 20, 20);

        fill(250, 250, 0);
        ellipse(x + 20, y, 3, 3);
  
  }
//draws the tower bar
    public void zeichneTurmleiste() {
        int x = 70, y = 20;
        fill(255);
        rect(x, 0, 40, 40);
        
          fill(0);
        text("" + (int) (170 * preismultiplikator), x + 15, y + 20);
        fill(0);
       
          x += 20;
          
        zeichneAffeLv1(x,y);
        x += 20;
     
      
        stroke(0);
        fill(255);
        rect(x, 0, 40, 40);
        x += 110;
        scale(0.5f);
        x += 40;

        zeichneKanoneLv1(x,y);
      
        fill(0);
        textSize(26);
        text("" + (int) (555 * preismultiplikator), x - 10, y + 60);
        textSize(13);
        scale(2);
        x -= 110;

        fill(255);
        rect(x, 0, 40, 40);
        fill(0);
        rect(x + 28, y - 15, 1, 10); fill(0);
        text("" + (int) (450 * preismultiplikator), x + 15, y + 20);
       
      
        zeichneZaubererAffeLv1(x,y);
       
    }

    public void handlung() {
        for (int i = 0; i < aa; i++) {
            alleAffen[i].schiessen();
            alleAffen[i].upgraden();
    

            for (int ii = 0; ii < alleAffen[i].ag; ii++) {

             ((Monkey.Geschoss)alleAffen[i].superGeschosse[ii]).fliegen();
            }

        }
        for (int i = 0; i < ak; i++) {
             
            alleKanonen[i].schiessen();
            alleKanonen[i].upgraden();
       

            for (int ii = 0; ii < alleKanonen[i].ag; ii++) {

             
                ((Cannon.Kugel)alleKanonen[i].superGeschosse[ii]).fliegen();
            }
        }
        for (int i = 0; i < aza; i++) {
            alleZaubereraffen[i].schiessen();
            alleZaubereraffen[i].upgraden();
           

            for (int ii = 0; ii < alleZaubereraffen[i].ag; ii++) {

                ((WizardMonkey.Magiekugel)alleZaubereraffen[i].superGeschosse[ii]).fliegen();
            }

        }
        for (Entry<String, Targets> entry : alleZiele.entrySet()) {
            entry.getValue().bewegen();
        }
    }
// call all the draw methods
    public void zeichne() {
        zeichneTurmleiste();
        zeicheLebenGeldLevel();
        zeichneAffen();
        zeichneKanonen();
        zeichneZauberaffen();
        zeichneZiele();

    }
// write the current lv, gold and live
    private void zeicheLebenGeldLevel() {
        fill(0);
        text("Level" + ((int) (frameCount / 1000) + 1) + "/25", 450, 20);
        text("Gold:" + geld, 10, 15);
        text("Live:" + leben, 10, 30);

    }
// draw all the monkeys
    private void zeichneAffen() {
    
        for (int i = 0; i < aa; i++) {
            alleAffen[i].zeichne();
            for (int ii = 0; ii < alleAffen[i].ag; ii++) {

                alleAffen[i].superGeschosse[ii].zeichne();
            }
        }
    }
// draw all the cannons
    private void zeichneKanonen() {
        for (int i = 0; i < ak; i++) {
            alleKanonen[i].zeichne();
            for (int ii = 0; ii < alleKanonen[i].ag; ii++) {

              
                ((Cannon.Kugel)alleKanonen[i].superGeschosse[ii]).zeichne();
            }
        }
    }
// draw all the wizardmonkeys
    private void zeichneZauberaffen() {
        for (int i = 0; i < aza; i++) {
            alleZaubereraffen[i].zeichne();
            for (int ii = 0; ii < alleZaubereraffen[i].ag; ii++) {

                alleZaubereraffen[i].superGeschosse[ii].zeichne();
            }
        }
    }
// draw all the targets
    private void zeichneZiele() {
        for (Entry<String, Targets> entry : alleZiele.entrySet()) {
            entry.getValue().zeichne();
        }

    }
    float b = 0;
    int h, h1;//b,h,h1 make the targets appear with a certain frequency, they are used only here

    public void zielsetzen() {//makes the targets appear

        if (b > h && az < 998 && (aa > 0 || ak > 0 || aza > 0) && frameCount < 24200) {
            if ((int) 1000 / (((int) (frameCount + 1/* +1 verhindert dass durch 0 geteilt wuerde*/) / 1000) + 1) > 2) {
                h = (int) 200 / (((int) (frameCount + 1) / 1000) + 1);
            }
            b = 0;
            int zuid = 0;
            if (frameCount > 5000) {//if framecount>5000 creates additional targets with lv 2
                zuid = holeEindeutigeID();
                alleZiele.put("" + zuid, new Targets(this,0, 2/*level*/, zuid));
            }
            if (frameCount > 10000) {//see above
                zuid = holeEindeutigeID();
                alleZiele.put("" + zuid, new Targets(this,0, 3, zuid));
            }
            if (frameCount > 15000) {//see above
                zuid = holeEindeutigeID();
                alleZiele.put("" + zuid, new Targets(this,0, 4, zuid));
            }
            if (frameCount > 20000) {//see above
                zuid = holeEindeutigeID();
                alleZiele.put("" + zuid, new Targets(this,0, 5, zuid));
            }

            zuid = holeEindeutigeID();//gets a id for the new target
            Targets neuesziel = new Targets(this,0, 1, zuid);//in every cas a target lv1 is created
            alleZiele.put("" + zuid, neuesziel);//put the new target to the targets

            az = alleZiele.size();// set the targets counter on its new value
         
        }
        b += 1;

    }
//allows the player to place a new tower
    void turmsetzen() {

        if (j == 0 && mousePressed == true && mouseButton == LEFT && mouseX < 190 && mouseX > 70 && mouseY < 40) {// checks wich tower you click on
            if (mouseX < 110) {

                j = 1;

            }
            if (mouseX > 110 && mouseX < 150) {
                j = 2;

            }
            if (mouseX > 150 && mouseX < 190) {
                j = 3;
            }
        }
        if (j != 0) {
            switch (j) {//if you select a tower this makes you see the picture of the tower where you mouse is
                case 1:
                    //monkey
                    noStroke();
                    fill(125, 125, 255, 128);
                    ellipse(mouseX, mouseY, 60, 60);
                    fill(0);
                    ellipse(mouseX - 10, mouseY - 8, 5, 5);
                    fill(200, 100, 0);
                    stroke(0);
                    ellipse(mouseX + 13, mouseY, 10, 10);
                    ellipse(mouseX - 13, mouseY, 10, 10);
                    ellipse(mouseX, mouseY, 25, 20);
                    noStroke();
                    ellipse(mouseX + 3, mouseY + 19, 4, 4);
                    ellipse(mouseX + 3, mouseY + 17, 4, 4);
                    ellipse(mouseX + 2, mouseY + 15, 4, 4);
                    ellipse(mouseX, mouseY + 14, 4, 4);
                    ellipse(mouseX, mouseY + 12, 4, 4);
                    ellipse(mouseX, mouseY + 10, 4, 4);
                    break;
                case 2:
                    //cannon
                    noStroke();
                    fill(125, 125, 255, 128);
                    ellipse(mouseX, mouseY, 80, 80);
                    scale(1);
                    curveTightness(0.01f);
                    strokeWeight(1);
                    stroke(0);
                    fill(0);
                    beginShape();
                    curveVertex(mouseX - 10, mouseY - 8);
                    curveVertex(mouseX - 10, mouseY - 10);
                    curveVertex(mouseX + 10, mouseY - 10);
                    curveVertex(mouseX + 12, mouseY - 7);
                    curveVertex(mouseX + 15, mouseY + 20);
                    curveVertex(mouseX + 15, mouseY + 30);
                    curveVertex(mouseX + 6, mouseY + 40);
                    curveVertex(mouseX - 6, mouseY + 40);
                    curveVertex(mouseX - 15, mouseY + 30);
                    curveVertex(mouseX - 15, mouseY + 20);
                    curveVertex(mouseX - 10, mouseY - 10);
                    endShape(CLOSE);
                    strokeWeight(0.45f);
                    beginShape();
                    vertex(mouseX - 11, mouseY - 5);
                    bezierVertex(mouseX - 20, mouseY - 15, mouseX + 20, mouseY - 15, mouseX + 13, mouseY - 5);
                    bezierVertex(mouseX + 15, mouseY - 12, mouseX - 15, mouseY - 12, mouseX - 11, mouseY - 5);
                    endShape(CLOSE);
                    strokeWeight(0.8f);
                    ellipse(mouseX, mouseY + 45, 6, 6);
                    fill(140, 100, 0);
                    rect(mouseX - 25, mouseY + 22, 4, 7);
                    rect(mouseX - 21, mouseY + 10, 6, 31);
                    rect(mouseX + 21, mouseY + 22, 4, 7);
                    rect(mouseX + 15, mouseY + 10, 6, 31);
                    break;
                case 3:
                    //wizardmonkey
                    noStroke();
                    fill(125, 125, 255, 128);
                    ellipse(mouseX, mouseY, 60, 60);   //Zauberstab
                    fill(0);
                    rect(mouseX + 8, mouseY - 15, 1, 10);  //Zaubereraffe
                    fill(200, 100, 0);
                    stroke(0);
                    ellipse(mouseX + 13, mouseY, 10, 10);
                    ellipse(mouseX + -13, mouseY, 10, 10);
                    ellipse(mouseX, mouseY, 25, 20);
                    noStroke();
                    fill(200, 100, 0);
                    ellipse(mouseX + 3, mouseY + 19, 4, 4);
                    ellipse(mouseX + 3, mouseY + 17, 4, 4);
                    ellipse(mouseX + 2, mouseY + 15, 4, 4);
                    ellipse(mouseX + 0, mouseY + 14, 4, 4);
                    ellipse(mouseX + 0, mouseY + 12, 4, 4);
                    ellipse(mouseX + 0, mouseY + 10, 4, 4);
                    fill(25, 0, 150);
                    ellipse(mouseX + 0, mouseY, 20, 20);
                    fill(250, 250, 0);
                    ellipse(mouseX + 0, mouseY, 3, 3);
                    break;
            }
        }
        //to deselect a tower
        if (mousePressed == true && mouseButton == RIGHT && turmangeklickt == 0 && j != 0 && mouseY > 40) {
            j = 0;
        }
        //to place the selected tower
        if (mousePressed == true && mouseButton == LEFT && turmangeklickt == 0 && j != 0 && mouseY > 40) {
            switch (j) {
                case 1:
                    if (geld >= 170 * preismultiplikator && aa < 20) {//only if you have still place and the gold the monkey is placed
                        geld -= 170 * preismultiplikator;
                        alleAffen[aa] = new Monkey(this,mouseX, mouseY,"Affe");
                    }
                    j = 0;
                    break;
                case 2:
                    if (geld >= 555 * preismultiplikator && ak < 20) {//only if you have still place and the gold the cannon is placed                 
                        geld -= 555 * preismultiplikator;
                        alleKanonen[ak] = new Cannon(this,mouseX, mouseY,"Kanone");
                    }
                    j = 0;
                    break;
                case 3:
                    if (geld >= 450 * preismultiplikator && aza < 20) {//only if you have still place and the gold the wizardmonkey is placed                 
                        geld -= 350 * preismultiplikator;
                        alleZaubereraffen[aza] = new WizardMonkey(this,mouseX, mouseY,"Zaubereaffe");
                    }
                    j = 0;
                    break;
            }
        }
    }

    int x = 0, y = 0;
//this allows you to move most of the world 
    public void spielerEingabe() {
        translate(x, y);
        if (keyPressed == true) {
            switch (key) {
                case 's':
                    y += 10;
                    break;
                case 'w':
                    y -= 10;
                    break;
                case 'd':
                    if (mDEBUG) {
                        mDEBUG = false;

                    } else {
                        mDEBUG = true;
                    }
                    break;
            }
        }
    }


//the pausebutton
    public void pauseknopf() {
        fill(255);
        rect(760, 10, 40, 20);
        fill(0);
        textSize(12);
        if (pausiert == false) {
            text("Pause", 765, 25);
        } else {
            text("Los", 765, 25);
        }
        if (mousePressed == true && mouseButton == LEFT && mouseX > 760 && mouseX < 800 && mouseY > 10 && mouseY < 30) {
            pause_an_aus();
        }

    }
//to pause/unpause the game
    public void pause_an_aus() {
        pausiert = !pausiert;

    }
//the resetbutton
    public void resetknopf() {
        fill(255);
        rect(800, 10, 40, 20);
        fill(0);
        textSize(12);
        text("reset", 805, 25);

        if (mousePressed == true && mouseButton == LEFT && mouseX > 800 && mouseX < 840 && mouseY > 10 && mouseY < 30) {
            reset();
        }
    }
//to reset the game
    public void reset() {
        alleZiele = new HashMap<>();
        alleWegBoxen = new HashMap<>();
        mUID = 0;
        alleAffen = new Monkey[20];
        alleKanonen = new Cannon[20];
        alleZaubereraffen = new WizardMonkey[20];
        int ii = 0;
        while (ii < 49) {
            alleWegBoxen.put("wb" + ii, new PathBox(this, aw, ii * 10, 400, 1, 0));
            ii++;
        }
        while (ii < 54) {
            alleWegBoxen.put("wb" + ii, new PathBox(this, aw, 490, 400 - ((ii - 49) * 10), 0, -1));
            ii++;
        }
        while (ii < 98) {
            alleWegBoxen.put("wb" + ii, new PathBox(this, aw, 490 - ((ii - 54) * 10), 350, -1, 0));
            ii++;
        }
        while (ii < 103) {
            alleWegBoxen.put("wb" + ii, new PathBox(this, aw, 50, 350 - ((ii - 98) * 10), 0, -1));
            ii++;
        }
        while (ii < 188) {
            alleWegBoxen.put("wb" + ii, new PathBox(this, aw, 50 + ((ii - 103) * 10), 300, 1, 0));
            ii++;
        }
        geld = 950;
        leben = 50;
        frameCount = 0;
        aa = 0;
        ak = 0;
        aza = 0;
    }

    static public void main(String[] passedArgs) {
        String[] appletArgs = new String[]{"affe_wirft_auf_ziel"};
        if (passedArgs != null) {
            PApplet.main(concat(appletArgs, passedArgs));
        } else {
            PApplet.main(appletArgs);
        }
    }
          
}
