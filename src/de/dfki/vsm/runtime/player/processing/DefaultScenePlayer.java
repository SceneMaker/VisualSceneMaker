package de.dfki.vsm.runtime.player.processing;
import processing.core.*;
 
/**
 * @author Sergio Soto
 *
 */
public class DefaultScenePlayer extends PApplet {
    int pos_x = 150;
    int pos_y = 80;
    float size = 1;
    float headSize = 95 * size;

    int windowHeight = 400;
    int windowWidth = 300;

    @Override
    public void setup() {
        size(windowWidth, windowHeight);
    } 

    @Override
    public void draw () {
        background(181, 181, 255);


        stroke(255);
        fill (255);

        // head
        ellipse(pos_x, pos_y, headSize * size, headSize * size);

        // face
        stroke(181);
        strokeWeight(1);



        ellipse((float) (pos_x - headSize/4.5), pos_y - headSize/9 * size, headSize/5 * size,  headSize/5 * size);
        ellipse((float) (pos_x + headSize/4.5),pos_y - headSize/9 * size, headSize/5 * size,  headSize/5 * size);

        line(pos_x - headSize/4 ,pos_y + headSize/4, pos_x + headSize/4 , pos_y + headSize/4 );
        line(pos_x - headSize/4 , pos_y + headSize/4, pos_x ,  pos_y + headSize/3 );
        line(pos_x ,  pos_y + headSize/3, pos_x + headSize/4  , pos_y + headSize/4  );
        stroke(255);

        strokeWeight(4);
        // body
        line(pos_x, pos_y + (headSize * size)/2, pos_x, (pos_y*3) * size);
        rect ((float) (pos_x/1.1), (float) (pos_y + (headSize * size)/1.7), 30 * size, 115 * size);

        // legs
        line(pos_x, (pos_y*3) * size, pos_x - (headSize * size)/3, (float) ((pos_y*3) * 1.4* size));
        line(pos_x, (pos_y*3) * size, pos_x + (headSize * size)/3, (float) ((pos_y*3) * 1.4* size));

        // feet
        line((float) (pos_x - (headSize * size)/3), (float)((pos_y*3) * 1.4* size), pos_x - (headSize * size)/3 - 20* size, (float) ((pos_y*3) * 1.4* size));
        line((float) (pos_x + (headSize * size)/3), (float)((pos_y*3) * 1.4* size), pos_x + (headSize * size)/3 + 20* size, (float) ((pos_y*3) * 1.4* size));

        // arms
        line((float) (pos_x - headSize/(1.5/ size)), (pos_y+headSize *size), (float) (pos_x + headSize/(1.3/ size)), (pos_y+headSize  *size));

        // label
        fill (88,88,88);
        stroke(88,88,88);
        rect (0, windowHeight - 50, windowWidth, windowHeight);
        fill (255);
        text("Default ScenePlayer Messages", 10, windowHeight - 35);
    }

}



