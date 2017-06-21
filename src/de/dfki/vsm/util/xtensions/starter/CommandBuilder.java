package de.dfki.vsm.util.xtensions.starter;

import java.util.LinkedList;
import java.util.List;

/**
 * Created by alvaro on 6/20/17.
 */

public abstract class CommandBuilder {
    private static final  String OS = System.getProperty("os.name").toLowerCase();
    protected final String applicationName;
    protected String applicationPath;
    protected List<String> command;



    protected CommandBuilder(String applicationPath, String applicationName){
        this.applicationPath = applicationPath;
        this.applicationName = applicationName;
        command = new LinkedList<>();
    }

    public static CommandBuilder getCommandBuilder(String applicationPath, String applicationName){
        CommandBuilder commandBuilder = null;
        if (isUnix() || isMac()) {
            return new UnixCommand(applicationPath, applicationName);
        }
        return new WindowsCommand(applicationPath, applicationName);
    }

    public abstract String[] build();
    public abstract void stopApplication();



    private static boolean isWindows() {
        return (OS.indexOf("win") >= 0);
    }

    private static boolean isMac() {
        return (OS.indexOf("mac") >= 0);
    }

    private static boolean isUnix() {
        return (OS.indexOf("nix") >= 0 || OS.indexOf("nux") >= 0 || OS.indexOf("aix") > 0);
    }
}
