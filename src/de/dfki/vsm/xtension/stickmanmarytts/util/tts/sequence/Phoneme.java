package de.dfki.vsm.xtension.stickmanmarytts.util.tts.sequence;


import java.util.HashMap;

/**
 * Created by alvaro on 3/28/16.
 */
public class Phoneme {
    private String mValue;
    private long mStart;
    private long mEnd;
    private HashMap<String, String> lipMap = new HashMap<>();


    public Phoneme(String value, long start, long end){
        mValue = value;
        mStart = start;
        mEnd = end;
        lipMap.put("A", "TWO");
        lipMap.put("O", "THREE");
        lipMap.put("u", "SEVEN");
        lipMap.put("w", "SEVEN");
        lipMap.put("i", "ONE");//SIX


        lipMap.put("{", "ONE");
        lipMap.put("V", "ONE");
        lipMap.put("E", "FOUR");
        lipMap.put("I", "ONE");//SIX
        lipMap.put("U", "FOUR");

        lipMap.put("@", "ONE");
        lipMap.put("r", "FIVE");//THIRTEEN

        lipMap.put("aU", "NINE");
        lipMap.put("OI", "TEN");
        lipMap.put("@U", "FIVE");
        lipMap.put("EI", "FOUR");
        lipMap.put("AI", "NINE");
        lipMap.put("p", "Default"); //TWENTYONE
        lipMap.put("t", "ONE"); //NINETEEN
        lipMap.put("k", "THREE"); //TWENTY
        lipMap.put("b", "Default"); //TWENTY
        lipMap.put("d", "ONE"); //NINETEEN
        lipMap.put("g", "THREE"); //TWENTY


        lipMap.put("tS", "THREE"); //SIXTEEN?
        lipMap.put("dZ", "THREE");//SIXTEEN?



        lipMap.put("f", "SIX"); //EIGHTTEEN?
        lipMap.put("v", "SIX"); //EIGHTTEEN?
        lipMap.put("T", "SIX"); //SEVENTEEN?
        lipMap.put("D", "SIX"); //SEVENTEEN?
        lipMap.put("s", "SIX"); //FIFTEEN?
        lipMap.put("z", "SIX"); //FIFTEEN?
        lipMap.put("S", "THREE"); //SIXTEEN?
        lipMap.put("Z", "THREE"); //SIXTEEN?

        lipMap.put("l", "ONE"); //FOURTEEN
        lipMap.put("m", "Default"); //FOURTEEN
        lipMap.put("n", "ONE"); //FOURTEEN
        lipMap.put("N", "THREE"); //TWENTY
        lipMap.put("r", "FIVE"); //THIRTEEN
        lipMap.put("w", "SEVEN");
        lipMap.put("j", "FIVE"); //FOURTEEN
        //lipMap.put("h", "THREE"); //SIXTEEN?












       // lipMap.put("s", Mouth.SHAPE.SIX);//TODO: Make this face

    }
    public String getmValue() {
        return mValue;
    }

    public void setmValue(String mValue) {
        this.mValue = mValue;
    }

    public long getmStart() {
        return mStart;
    }

    public void setmStart(long mStart) {
        this.mStart = mStart;
    }

    public long getmEnd() {
        return mEnd;
    }

    public void setmEnd(long mEnd) {
        this.mEnd = mEnd;
    }

    public String getLipPosition(String phoneme){
        if(lipMap.containsKey(phoneme)){
            return lipMap.get(phoneme);
        }
        else{
            return "Default";
        }
    }

    public String getLipPosition(){
        if(lipMap.containsKey(mValue)){
            return lipMap.get(mValue);
        }
        return null;
    }
}
