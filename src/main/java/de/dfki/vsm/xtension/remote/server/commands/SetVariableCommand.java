package de.dfki.vsm.xtension.remote.server.commands;

import de.dfki.vsm.runtime.project.RunTimeProject;

/**
 * Created by alvaro on 6/13/17.
 */
public class SetVariableCommand implements NotifiableCommand {

    protected String data;
    protected final RunTimeProject project;
    protected final String variableName;

    public SetVariableCommand(String data, RunTimeProject project, String variableName) {
        this.data = data;
        this.project = project;
        this.variableName = variableName;
    }

    @Override
    public void execute() {

        if(project.hasVariable(variableName))
            project.setVariable(variableName, data);
    }

    @Override
    public void setValue(Object value) {
        data = (String) value;
    }
}
