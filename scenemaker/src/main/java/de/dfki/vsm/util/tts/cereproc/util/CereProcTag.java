/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.util.tts.cereproc.util;

import java.util.HashMap;

/**
 *
 * @author Robbie
 */
public class CereProcTag {
    public static final HashMap<String, String> vocalGesture = new HashMap<String, String>();
    static {
        vocalGesture.put("tutGesture", "<spurt audio=\"g0001_001\">tut</spurt>");        // kiss
        vocalGesture.put("tuttutGesture", "<spurt audio=\"g0001_002\">tut tut</spurt>"); // kiss 2 times
        vocalGesture.put("coughGesture", "<spurt audio=\"g0001_003\">cough</spurt>");
        vocalGesture.put("longcoughGesture", "<spurt audio=\"g0001_004\">cough</spurt>");
        vocalGesture.put("shortcoughGesture", "<spurt audio=\"g0001_005\">cough</spurt>");
        vocalGesture.put("clearthroatGesture", "<spurt audio=\"g0001_006\">clear throat</spurt>");
        vocalGesture.put("breathinGesture", "<spurt audio=\"g0001_007\">breath in</spurt>");
        vocalGesture.put("sharpintakeofbreathGesture", "<spurt audio=\"g0001_008\">sharp intake of breath</spurt>");
        vocalGesture.put("breathinthroughteethGesture", "<spurt audio=\"g0001_009\">breath in through teeth</spurt>");
        vocalGesture.put("sighhappyGesture", "<spurt audio=\"g0001_010\">sigh happy</spurt>");
        vocalGesture.put("sighsadGesture", "<spurt audio=\"g0001_011\">sigh sad</spurt>");
        vocalGesture.put("hmmquestionGesture", "<spurt audio=\"g0001_012\">hmm question</spurt>");
        vocalGesture.put("hmmyesGesture", "<spurt audio=\"g0001_013\">hmm yes</spurt>");
        vocalGesture.put("hmmthinkingGesture", "<spurt audio=\"g0001_014\">hmm thinking</spurt>");
        vocalGesture.put("longummGesture", "<spurt audio=\"g0001_015\">umm</spurt>");
        vocalGesture.put("ummGesture", "<spurt audio=\"g0001_016\">umm</spurt>");
        vocalGesture.put("errGesture", "<spurt audio=\"g0001_017\">err</spurt>");
        vocalGesture.put("longerrGesture", "<spurt audio=\"g0001_018\">err</spurt>");
        vocalGesture.put("giggleGesture", "<spurt audio=\"g0001_019\">giggle</spurt>");
        vocalGesture.put("shortgiggleGesture", "<spurt audio=\"g0001_020\">giggle</spurt>");
        vocalGesture.put("longlaughiGesture", "<spurt audio=\"g0001_021\">laugh</spurt>");
        vocalGesture.put("laughiGesture", "<spurt audio=\"g0001_022\">laugh</spurt>");
        vocalGesture.put("laughGesture", "<spurt audio=\"g0001_023\">laugh</spurt>");
        vocalGesture.put("longlaughGesture", "<spurt audio=\"g0001_024\">laugh</spurt>");
        vocalGesture.put("ahpositiveGesture", "<spurt audio=\"g0001_025\">ah positive</spurt>");
        vocalGesture.put("ahnegativeGesture", "<spurt audio=\"g0001_026\">ah negative</spurt>");
        vocalGesture.put("yeahquestionGesture", "<spurt audio=\"g0001_027\">yeah question</spurt>");
        vocalGesture.put("yeahpositiveGesture", "<spurt audio=\"g0001_028\">yeah positive</spurt>");
        vocalGesture.put("yeahresignedGesture", "<spurt audio=\"g0001_029\">yeah resigned</spurt>");
        vocalGesture.put("sniffGesture", "<spurt audio=\"g0001_030\">sniff</spurt>");
        vocalGesture.put("longsniffGesture", "<spurt audio=\"g0001_031\">sniff</spurt>");
        vocalGesture.put("arghupGesture", "<spurt audio=\"g0001_032\">argh</spurt>");
        vocalGesture.put("arghdownGesture", "<spurt audio=\"g0001_033\">argh</spurt>");
        vocalGesture.put("ughGesture", "<spurt audio=\"g0001_034\">ugh</spurt>");
        vocalGesture.put("ochtGesture", "<spurt audio=\"g0001_035\">ocht</spurt>");
        vocalGesture.put("yayGesture", "<spurt audio=\"g0001_036\">yay</spurt>");
        vocalGesture.put("ohpositiveGesture", "<spurt audio=\"g0001_037\">oh positive</spurt>");
        vocalGesture.put("ohnegativeGesture", "<spurt audio=\"g0001_038\">oh negative</spurt>");
        vocalGesture.put("sarcasticnoiseGesture", "<spurt audio=\"g0001_039\">sarcastic noise</spurt>");
        vocalGesture.put("yawnGesture", "<spurt audio=\"g0001_040\">yawn</spurt>");
        vocalGesture.put("longyawnGesture", "<spurt audio=\"g0001_041\">yawn</spurt>");
        vocalGesture.put("snoreGesture", "<spurt audio=\"g0001_042\">snore</spurt>");
        vocalGesture.put("snorephewGesture", "<spurt audio=\"g0001_043\">snore phew</spurt>");
        vocalGesture.put("zzzGesture", "<spurt audio=\"g0001_044\">zzz</spurt>");
        vocalGesture.put("raspberryGesture", "<spurt audio=\"g0001_045\">raspberry</spurt>");
        vocalGesture.put("longraspberryGesture", "<spurt audio=\"g0001_046\">raspberry</spurt>");
        vocalGesture.put("brrrcoldGesture", "<spurt audio=\"g0001_047\">brrr cold</spurt>");
        vocalGesture.put("snortGesture", "<spurt audio=\"g0001_048\">snort</spurt>");
        vocalGesture.put("hahaGesture", "<spurt audio=\"g0001_050\">ha ha</spurt>");
        vocalGesture.put("dohGesture", "<spurt audio=\"g0001_051\">doh</spurt>");
        vocalGesture.put("gaspGesture", "<spurt audio=\"g0001_052\">gasp</spurt>");        
    }
}
