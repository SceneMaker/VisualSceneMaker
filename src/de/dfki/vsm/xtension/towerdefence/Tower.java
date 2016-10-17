
package de.dfki.vsm.xtension.towerdefence;

import ddf.minim.AudioPlayer;
import ddf.minim.Minim;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.alleWegBoxen;
import static de.dfki.vsm.xtension.towerdefence.BalloonTowerDefence.mExecutor;
import java.awt.geom.Ellipse2D;
import java.io.File;
import java.util.ArrayList;
import java.util.Map;
import processing.core.PApplet;
import static processing.core.PApplet.sq;
import static processing.core.PApplet.sqrt;

/**
 *
 * @author Jan Stieling
 *
 */
public class Tower {
  private PApplet mApplet;
    private AudioPlayer mAudioPlayer;
    private Minim minim;
    float x, y;
    boolean zielgefunden;//if the tower has a target
    int feuerrate/*how fast it shoot*/, kugelsprengkraft/*explosion power of the projectile*/, lv = 1/*level of the tower*/, reichweite/*range*/, geschossdurchschlagskraft/*penetrationpower of the projectile*/, ag = 0/*amount of projectiles*/, aaw = 0/*how many pathtiles are in its range*/, id/*its identification number*/, verzoegerung = 0/*reload time*/, ausgewaehlt = 0/*if it is selected*/, anzahlTreffer = 0/*number of targets it has destroyed*/;
    GeschosseSuperklasse[] superGeschosse;//an array for its projectiles
    Targets ziel;//his choosed target
   Ellipse2D sichtweite;//
     ArrayList<String> inReichweiteWegBoxen = new ArrayList<>();// an ArrayList for the pathtiles in its range
    public ArrayList<Targets> potentielleZiele = new ArrayList<>();//an ArrayList for the targets in range
    String gedanken = "";//what it is thinking
    boolean habeExplosion = false;//if one of his projectiles expplodes
    Visualisation mAnzeigeManager;
    String art;//what kind of tower it is

    Tower(PApplet applet,float x_0, float y_0, String art_0) {
       mApplet=applet;
       minim=new Minim(mApplet);
      mAudioPlayer = minim.loadFile(mExecutor.mProject.getProjectPath() + File.separator + "snd" + File.separator + "boom.mp3");

    }
//a subclasse for it projectiles
    class GeschosseSuperklasse {

        float x/*x coordinate*/, x1/*moving in x direction*/, y/*y coordinate*/, y1/*moving in y direction*/, zielx/*the x coordinate of its target*/, ziely/*the y coordinate of its target*/;
        int id/*identification number*/, durchschlagskraft/*penetration*/, turmid/*identification number of the tower which created it*/, sprengkraft1/*explosion power*/, r/*color*/, g/*color*/, b/*color*/, unschaerfe = 10/*hit radius*/, explodiert = 0;/*if it is exploding*/
        Ellipse2D sprengradius;
        String art;/*kind of the projectile*/

        GeschosseSuperklasse(int turmid_0, float x_0, float y_0, int sprengkraft1_0, int durchschlagskraft_0, float zielx_0, float ziely_0, int r_0, int g_0, int b_0, String art) {
            x = x_0;
            y = y_0;
            zielx = zielx_0;
            ziely = ziely_0;
            x1 = (zielx - x) / ((sqrt(sq(zielx - x))) + (sqrt(sq(ziely - y)))); //x1 is calculated based on the towers coordinates and the targets coordinates
            y1 = (ziely - y) / ((sqrt(sq(zielx - x))) + (sqrt(sq(ziely - y))));//x1 is calculated based on the towers coordinates and the targets coordinates
            id = ag;//
            turmid = turmid_0;
            sprengkraft1 = sprengkraft1_0;
            durchschlagskraft = durchschlagskraft_0;
            sprengradius = new Ellipse2D.Float(x - sprengkraft1, y - sprengkraft1, sprengkraft1, sprengkraft1);
            r = r_0;
            g = g_0;
            b = b_0;
            ag++;//counter for the amount of projectiles is increased
        }
    }
     public void implantiereGedanken(String text) {
        mAnzeigeManager = new Visualisation(mApplet);
        gedanken = text;
    }
     //delete a projectile
     public void geschossloeschen(int z) {
      
        ag--;
         
    }
     //targeting
     public void zielen() {
        
        zielgefunden = false;
        ziel = null;
        potentielleZiele = new ArrayList<>();
     
                //checks of pathtiles in range ...
                for (String wbName : inReichweiteWegBoxen) {
                       //if they have at least on target within them
                    if (alleWegBoxen.get(wbName).zielSpeicher.size() > 0 && ziel == null) {
                    
                    
                 //if it hasn't already a targets it now checks the target in the pathtile
                        for (Map.Entry<String, Targets> zielEintrag : alleWegBoxen.get(wbName).zielSpeicher.entrySet()) {
                            potentielleZiele.add(zielEintrag.getValue());
                           //and choose it target
                            if (ziel == null) {
                                zielgefunden = true;
                                ziel = zielEintrag.getValue();
                              
                            }
                        }
                    }
          
        }
    }
}


