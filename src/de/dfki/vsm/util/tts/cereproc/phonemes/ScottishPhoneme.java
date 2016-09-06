package de.dfki.vsm.util.tts.cereproc.phonemes;


import de.dfki.vsm.xtension.stickmantts.util.tts.sequence.Phoneme;

/**
 * Created by alvaro on 7/31/16.
 */
public class ScottishPhoneme extends Phoneme {
    public ScottishPhoneme(String value, long start, long end) {
        super(value, start, end);
    }

    @Override
    protected void initLipMap() {
        lipMap.put("aa", "TWO");
        lipMap.put("x", "TWO"); //Not found
        lipMap.put("o", "THREE");
        lipMap.put("oo", "THREE"); //Not scottish
        lipMap.put("uu", "SEVEN");
        lipMap.put("w", "SEVEN");
        lipMap.put("ii", "ONE");//SIX


        lipMap.put("a", "ONE");
        //lipMap.put("V", "ONE");
        lipMap.put("e", "FOUR");
        lipMap.put("i", "ONE");//SIX
        lipMap.put("u", "FOUR");

        lipMap.put("uh", "ONE");
        lipMap.put("r", "FIVE");//THIRTEEN

        lipMap.put("au", "NINE");
        lipMap.put("oi", "TEN");
        lipMap.put("ou", "FIVE");
        lipMap.put("ei", "FOUR");
        lipMap.put("ai", "NINE");
        lipMap.put("p", "Default"); //TWENTYONE
        lipMap.put("t", "ONE"); //NINETEEN
        lipMap.put("k", "THREE"); //TWENTY
        lipMap.put("b", "Default"); //TWENTY
        lipMap.put("d", "ONE"); //NINETEEN
        lipMap.put("g", "THREE"); //TWENTY


        lipMap.put("ch", "THREE"); //SIXTEEN?
        lipMap.put("jh", "THREE");//SIXTEEN?



        lipMap.put("f", "SIX"); //EIGHTTEEN?
        lipMap.put("v", "SIX"); //EIGHTTEEN?
        lipMap.put("th", "SIX"); //SEVENTEEN?
        lipMap.put("dh", "SIX"); //SEVENTEEN?
        lipMap.put("s", "SIX"); //FIFTEEN?
        lipMap.put("z", "SIX"); //FIFTEEN?
        lipMap.put("sh", "THREE"); //SIXTEEN?
        lipMap.put("zh", "THREE"); //SIXTEEN?

        lipMap.put("l", "ONE"); //FOURTEEN
        lipMap.put("m", "Default"); //FOURTEEN
        lipMap.put("n", "ONE"); //FOURTEEN
        lipMap.put("ng", "THREE"); //TWENTY
        lipMap.put("r", "FIVE"); //THIRTEEN
        lipMap.put("w", "SEVEN");
        //lipMap.put("j", "FIVE"); //FOURTEEN
    }
}
