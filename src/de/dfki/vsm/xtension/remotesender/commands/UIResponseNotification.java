package de.dfki.vsm.xtension.remotesender.commands;


import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.remotesender.server.notifications.NotifiableCommand;

/**
 * Created by alvaro on 4/30/17.
 */
public class UIResponseNotification implements NotifiableCommand {

    private final RunTimeProject project;
    private final String variableName;
    private String selectedOption;


    public UIResponseNotification(RunTimeProject project, String variableName) {
        this.project = project;
        this.variableName = variableName;
    }

    @Override
    public void execute() {
        project.setVariable(variableName, selectedOption);
    }

    @Override
    public void setValue(Object value) {
        selectedOption = (String) value;
    }

    public void setSelectedOption(String selectedOption) {
        this.selectedOption = selectedOption;
    }
}
