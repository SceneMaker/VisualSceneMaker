package de.dfki.vsm.util.xtensions.starter;

import java.io.IOException;

/**
 * Created by alvaro on 6/20/17.
 */
public class WindowsCommand extends CommandBuilder {

    public static final String WINDOWS_CMD = "CMD";

    public WindowsCommand(String applicationPath, String applicationName) {
        super(applicationPath, applicationName);
    }

    @Override
    public String[] build() {
        applicationPath = applicationPath + ".bat";
        command.add(WINDOWS_CMD);
        command.add("/C");
        command.add(applicationPath);
        return command.toArray(new String[command.size()]);
    }

    @Override
    public void stopApplication() {
        String killCmd = "wmic Path win32_process Where \"CommandLine Like '%" + applicationName + "%'\" Call Terminate";
        killApplication(killCmd);

    }

    private void killApplication(String killCmd) {
        try {
            Process killer = Runtime.getRuntime().exec(killCmd);
            killer.waitFor();
        } catch (IOException | InterruptedException e) {
            e.printStackTrace();
        }
    }
}
