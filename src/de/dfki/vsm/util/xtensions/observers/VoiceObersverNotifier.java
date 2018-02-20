package de.dfki.vsm.util.xtensions.observers;

import java.util.LinkedList;

/**
 * Created by alvaro on 6/5/17.
 */
public class VoiceObersverNotifier implements Observable {
    private LinkedList<Observer> observers = new LinkedList<>();

    @Override
    public void register(Observer observer) {
        observers.add(observer);
    }

    @Override
    public void unregister(Observer observer) {
        observers.remove(observer);
    }

    @Override
    public void notifyAll(String message) {
        for (Observer observer : observers) {
            observer.update(message);
        }
    }
}
