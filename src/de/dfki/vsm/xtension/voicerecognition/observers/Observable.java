package de.dfki.vsm.xtension.voicerecognition.observers;

/**
 * Created by alvaro on 6/5/17.
 */
public interface Observable  {
    void register (Observer observer);
    void unregister (Observer observer);
    void notifyAll(String message);
}
