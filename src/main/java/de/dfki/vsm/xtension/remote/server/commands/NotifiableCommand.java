package de.dfki.vsm.xtension.remote.server.commands;

/**
 * Created by alvaro on 5/10/17.
 */
public interface NotifiableCommand {
    void execute();
    void setValue(Object value);
}
