package de.dfki.vsm.util.xtensions.starter;

import java.io.IOException;

/**
 * Created by alvaro on 6/20/17.
 */
public class Executor {
    private final CommandBuilder commandBuilder;

    public Executor(CommandBuilder commandBuilder) {
        this.commandBuilder = commandBuilder;
    }

    public Process execute() throws IOException {
        final String []command = commandBuilder.build();
        final ProcessBuilder processB = new ProcessBuilder(command);
        processB.redirectErrorStream(true);
        return  processB.start();
    }
}
