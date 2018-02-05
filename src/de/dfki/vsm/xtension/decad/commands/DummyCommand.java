package de.dfki.vsm.xtension.decad.commands;

import de.dfki.vsm.runtime.activity.AbstractActivity;

import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

public class DummyCommand extends DecadCommand {
    public DummyCommand(AbstractActivity activity) {
        super(activity);
    }

    @Override
    protected String buildUrl() {
        return "";
    }

    @Override
    public boolean isBlocking() {
        return false;
    }

    @Override
    public void execute() {
        Logger.getLogger("VSM").log(new LogRecord(Level.INFO, "Executing dummy"));
    }
}
