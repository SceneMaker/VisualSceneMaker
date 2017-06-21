package de.dfki.vsm.util.xtensions.starter;


/**
 * Created by alvaro on 6/20/17.
 */
public interface ExecutableStarter {
    boolean executeApplication() ;

    boolean execExists();
    boolean onlyOneInstanceAllowed();
    boolean applicationAlreadyStarted();
    String getApplicationName();
}
