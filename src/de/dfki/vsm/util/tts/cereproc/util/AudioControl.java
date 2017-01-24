package de.dfki.vsm.util.tts.cereproc.util;

import de.dfki.vsm.util.tts.cereproc.audioplayer.ClipPlayer;

import javax.sound.sampled.Clip;

/**
 * Created by alvaro on 1/23/17.
 */
public class AudioControl {
    private static  ClipPlayer player;
    private  static Clip clip;


    public static void setClip(Clip c){
        clip = c;
    }

    public static void setPlayer(ClipPlayer p){
        player  = p;
    }

    public static void stop(){
        if(clip != null) {
            player.stop(clip);
        }
    }
}
