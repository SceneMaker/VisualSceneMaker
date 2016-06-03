package de.dfki.vsm.xtesting.NewPropertyManager.util;

/**
 * Created by alvaro on 5/14/16.
 */
public interface TreeObservable {
    public void registerObserver(TreeObserver object);
    public void unregisterObserver(TreeObserver object);
    public void notifyObserver();
}
