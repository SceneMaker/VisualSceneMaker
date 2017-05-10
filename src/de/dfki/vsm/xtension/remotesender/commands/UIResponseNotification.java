package de.dfki.vsm.xtension.remotesender.commands;


import de.dfki.vsm.xtension.remotesender.server.notifications.NotifiableCommand;

/**
 * Created by alvaro on 4/30/17.
 */
public class UIResponseNotification implements NotifiableCommand {

    private String selectedOption;

    @Override
    public void execute() {

    }

    public void setSelectedOption(String selectedOption) {
        this.selectedOption = selectedOption;
    }
}
