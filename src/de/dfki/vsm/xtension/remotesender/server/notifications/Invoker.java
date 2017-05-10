package de.dfki.vsm.xtension.remotesender.server.notifications;

/**
 * Created by alvaro on 5/10/17.
 */
public class Invoker implements DataNotification {
    protected NotifiableCommand command;

    @Override
    public void execute() {
        command.execute();
    }

    @Override
    public void setCommand(NotifiableCommand command) {
        this.command = command;
    }



}
