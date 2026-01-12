package de.dfki.vsm;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.web.WebUiServer;

import java.awt.Desktop;
import java.io.File;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;

/**
 * Web-only entry point for SceneMaker.
 */
public final class SceneMaker4 {

    private static final LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();

    public static void main(final String[] args) {
        boolean allowLan = false;
        boolean openBrowser = true;
        List<String> remaining = new ArrayList<>();
        for (String arg : args) {
            if ("--allow-lan".equalsIgnoreCase(arg) || "--allow-external".equalsIgnoreCase(arg)) {
                allowLan = true;
            } else if ("--no-browser".equalsIgnoreCase(arg)) {
                openBrowser = false;
            } else {
                remaining.add(arg);
            }
        }
        String[] effectiveArgs = remaining.toArray(new String[0]);
        try {
            WebUiServer server = WebUiServer.getInstance();
            server.setAllowExternal(allowLan);
            server.start();
            if (openBrowser) {
                openBrowser(server.getLocalUrl());
            }
        } catch (Exception exc) {
            sLogger.warning("Warning: Cannot start Web UI server: " + exc.getMessage());
        }
        if (effectiveArgs.length == 2 && "runtime".equalsIgnoreCase(effectiveArgs[0])) {
            final File file = new File(effectiveArgs[1]);
            if (file.exists()) {
                Core.runtime(file);
            } else {
                error(file);
            }
        }
    }

    private static void openBrowser(String url) {
        try {
            Desktop.getDesktop().browse(new URI(url));
        } catch (Exception exc) {
            sLogger.warning("Warning: Cannot open browser: " + exc.getMessage());
        }
    }

    private static void error(final File file) {
        sLogger.failure("Error: Cannot find file '" + file.getAbsolutePath() + "'");
    }
}
