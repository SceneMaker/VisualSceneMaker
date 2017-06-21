package de.dfki.vsm.util.xtensions.starter;

import java.io.IOException;

/**
 * Created by alvaro on 6/20/17.
 */
public class UnixCommand extends CommandBuilder {

    public static final String BIN_BASH = "/bin/bash";

    public UnixCommand(String applicationPath, String applicationName) {
        super(applicationPath, applicationName);
    }

    @Override
    public String[] build() {
        command.add(BIN_BASH);
        command.add(applicationPath);
        return command.toArray(new String[command.size()]);
    }

    @Override
    public void stopApplication() {
        String killCmd = "ps aux | grep '" + applicationName + "' | awk '{print $2}' | xargs kill";
        String[] cmd = {"/bin/sh", "-c", killCmd};
        killApplication(cmd);

    }

    private void killApplication(String[] cmd) {
        try {
            Process killer = Runtime.getRuntime().exec(cmd);
            killer.waitFor();
        } catch (IOException | InterruptedException e) {
            e.printStackTrace();
        }
    }
}
