package de.dfki.vsm;

import de.dfki.vsm.util.log.LOGDefaultLogger;
// Phase 8: Use core WebUiServer (Swing-free)
import de.dfki.vsm.web.WebUiServer;

import java.awt.Desktop;
import java.awt.Image;
import java.awt.Taskbar;
import java.io.File;
import java.io.InputStream;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import javax.imageio.ImageIO;

/**
 * Web-only entry point for SceneMaker.
 */
public final class SceneMaker4 {

    private static final LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();
    private static final String MAC_APP_NAME = "VSM Web";
    private static final String MAC_DOCK_ICON_PATH = "/app-icons/vsm-docicon.png";

    public static void main(final String[] args) {
        configureMacApp();
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
            if (Desktop.isDesktopSupported()) {
                Desktop.getDesktop().browse(new URI(url));
                return;
            }
        } catch (Exception exc) {
            sLogger.warning("Warning: Cannot open browser: " + exc.getMessage());
        }
        openBrowserFallback(url);
    }

    private static void openBrowserFallback(String url) {
        try {
            List<String> command = new ArrayList<>();
            if (isMacOs()) {
                command.add("open");
                command.add(url);
            } else if (isWindows()) {
                command.add("rundll32");
                command.add("url.dll,FileProtocolHandler");
                command.add(url);
            } else {
                command.add("xdg-open");
                command.add(url);
            }
            new ProcessBuilder(command).start();
        } catch (Exception exc) {
            sLogger.warning("Warning: Cannot open browser (fallback): " + exc.getMessage());
        }
    }

    private static void configureMacApp() {
        if (!isMacOs()) {
            return;
        }
        System.setProperty("apple.awt.application.name", MAC_APP_NAME);
        System.setProperty("com.apple.mrj.application.apple.menu.about.name", MAC_APP_NAME);
        try {
            if (Taskbar.isTaskbarSupported()) {
                Taskbar taskbar = Taskbar.getTaskbar();
                if (taskbar.isSupported(Taskbar.Feature.ICON_IMAGE)) {
                    Image icon = loadDockIcon();
                    if (icon != null) {
                        taskbar.setIconImage(icon);
                    }
                }
            }
        } catch (Exception exc) {
            sLogger.warning("Warning: Cannot set macOS dock icon: " + exc.getMessage());
        }
    }

    private static Image loadDockIcon() {
        try (InputStream stream = SceneMaker4.class.getResourceAsStream(MAC_DOCK_ICON_PATH)) {
            if (stream == null) {
                sLogger.warning("Warning: Dock icon resource not found: " + MAC_DOCK_ICON_PATH);
                return null;
            }
            return ImageIO.read(stream);
        } catch (Exception exc) {
            sLogger.warning("Warning: Cannot load dock icon: " + exc.getMessage());
            return null;
        }
    }

    private static boolean isMacOs() {
        String osName = System.getProperty("os.name");
        return osName != null && osName.toLowerCase().contains("mac");
    }

    private static boolean isWindows() {
        String osName = System.getProperty("os.name");
        return osName != null && osName.toLowerCase().contains("win");
    }

    private static void error(final File file) {
        sLogger.failure("Error: Cannot find file '" + file.getAbsolutePath() + "'");
    }
}
