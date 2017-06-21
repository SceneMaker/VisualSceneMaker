package de.dfki.vsm.util.xtensions.starter;

import de.dfki.vsm.util.xtensions.observers.Observer;

import java.util.Observable;

/**
 * Created by alvaro on 6/20/17.
 */
public class ExecutableCmdStarter extends AbstractExecStarter{
    private final String OS = System.getProperty("os.name").toLowerCase();

    public ExecutableCmdStarter(String applicationPath, String applicationName) {
        super(applicationPath, applicationName);
    }


    @Override
    public boolean executeApplication() {
        return false;
    }

    @Override
    public String getApplicationName() {
        return null;
    }

    @Override
    public void register(Observer observer) {

    }

    @Override
    public void unregister(Observer observer) {

    }

    @Override
    public void notifyAll(String message) {

    }
}
