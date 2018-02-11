package de.dfki.vsm.xtension.decad.commands;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import org.jetbrains.annotations.NotNull;

import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

public class DummyCommand extends DecadCommand {
    public DummyCommand(AbstractActivity activity) {
        super(activity);
    }

    @NotNull
    @Override
    protected String buildUrl() {
        return "";
    }

    @Override
    public void execute() {
        Logger.getLogger("VSM").log(new LogRecord(Level.INFO, "Executing dummy"));
    }
}
