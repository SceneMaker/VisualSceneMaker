package de.dfki.vsm.xtension.stickmantts.util.tts.sequence;


import java.util.HashMap;

/**
 * Created by alvaro on 3/28/16.
 */
public abstract class Phoneme  {
    private String mValue;
    private long mStart;
    private long mEnd;
    protected HashMap<String, String> lipMap = new HashMap<>();


    public Phoneme(String value, long start, long end){
        mValue = value;
        mStart = start;
        mEnd = end;

        //lipMap.put("h", "THREE"); //SIXTEEN?
       // lipMap.put("s", Mouth.SHAPE.SIX);//TODO: Make this face
        initLipMap();

    }

    protected abstract void initLipMap();

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
