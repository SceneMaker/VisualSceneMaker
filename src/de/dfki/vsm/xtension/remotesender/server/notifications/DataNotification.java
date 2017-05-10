package de.dfki.vsm.xtension.remotesender.server.notifications;


public interface DataNotification {
    void execute();
    void setCommand(NotifiableCommand command);
}
