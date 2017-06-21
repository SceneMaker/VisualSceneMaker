package de.dfki.vsm.xtension.baxter.utils;

/**
 * Created by alvaro on 22.09.16.
 */
public interface MessageObservable {
    void register(MessageObserver o);
    void unregister(MessageObserver o);
    void notifyAll(String message);
}
