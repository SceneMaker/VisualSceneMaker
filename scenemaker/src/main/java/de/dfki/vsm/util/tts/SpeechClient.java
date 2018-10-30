package de.dfki.vsm.util.tts;


import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * Created by alvaro on 25/06/16.
 */
public abstract class SpeechClient {
    protected List wordQueue = Collections.synchronizedList(new LinkedList());
    protected String finalWord = "";
    public abstract void addWord(String s);
    public String getPhrase() {
        synchronized(wordQueue){
            if(finalWord.length() <=0){
                Iterator it = wordQueue.iterator();
                while (it.hasNext()) {
                    String word = (String) it.next();
                    finalWord += word + " ";
                }
            }
        }
        return finalWord;
    }

    public String getFinalWord(){
        return finalWord;
    }
    public void setText(String text){
        finalWord = text;
    }
}
