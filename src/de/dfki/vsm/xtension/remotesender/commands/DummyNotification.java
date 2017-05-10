package de.dfki.vsm.xtension.remotesender.commands;


import de.dfki.vsm.xtension.remotesender.server.notifications.DataNotification;
import de.dfki.vsm.xtension.remotesender.server.notifications.NotifiableCommand;

/**
 * Created by alvaro on 4/30/17.
 */
public class DummyNotification implements DataNotification {
    @Override
    public void execute() {

    }

    @Override
    public void setCommand(NotifiableCommand command) {

    }
}
