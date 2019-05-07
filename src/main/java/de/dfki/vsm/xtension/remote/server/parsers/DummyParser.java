package de.dfki.vsm.xtension.remote.server.parsers;


import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.remote.server.commands.SetVariableCommand;
import de.dfki.vsm.xtension.remote.server.commands.NotifiableCommand;

/**
 * Created by alvaro on 4/30/17.
 */
public class DummyParser implements Parser {

    private final NotifiableCommand notification;

    public DummyParser(String data, RunTimeProject project, String variableName) {
        this.notification = new SetVariableCommand(data, project, variableName);
    }

    @Override
    public NotifiableCommand parse() {
        return notification;
    }
}
