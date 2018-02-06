package de.dfki.vsm.xtension.decad.utils;

import de.dfki.vsm.util.log.LOGDefaultLogger;

import java.awt.*;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

public class BrowserOpener {

    private static final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    private BrowserOpener() {

    }

    public static void openBrowserAt(String url) {
        if (Desktop.isDesktopSupported()) {
            tryToOpenBrowser(url);
        }
    }

    private static void tryToOpenBrowser(String url) {
        try {
            Desktop.getDesktop().browse(new URI(url));
        } catch (IOException | URISyntaxException e) {
            mLogger.failure("Could not open default browser");
            mLogger.failure(e.getMessage());
        }
    }
}
