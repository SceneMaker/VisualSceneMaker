package de.dfki.vsm.runtime;

import de.dfki.vsm.web.WebUiServer;

/**
 * Standalone runtime server entry point.
 *
 * This provides a headless runtime server that can:
 * - Load and execute VSM projects
 * - Expose a REST/WebSocket API for runtime control and monitoring
 * - Serve the Web UI for browser-based monitoring and control
 * - Run on any Java 17+ environment (Desktop, Android, embedded)
 *
 * Usage:
 *   java -jar runtime-server.jar [options]
 *
 * Options:
 *   --port=PORT       Server port (default: 8091)
 *   --allow-lan       Bind to 0.0.0.0 instead of localhost
 *   --project=PATH    Auto-load project on startup
 *   --autostart       Automatically start runtime after loading project
 *   --token=TOKEN     Set authentication token (default: auto-generated)
 *   --help            Show this help message
 */
public class RuntimeMain {

    private static final int DEFAULT_PORT = 8091;

    public static void main(String[] args) {
        int port = DEFAULT_PORT;
        boolean allowLan = false;
        String projectPath = null;
        boolean autoStart = false;
        String token = null;

        // Parse command-line arguments
        for (String arg : args) {
            if (arg.startsWith("--port=")) {
                try {
                    port = Integer.parseInt(arg.substring(7));
                } catch (NumberFormatException e) {
                    System.err.println("Invalid port number: " + arg.substring(7));
                    System.exit(1);
                }
            } else if (arg.equals("--allow-lan")) {
                allowLan = true;
            } else if (arg.startsWith("--project=")) {
                projectPath = arg.substring(10);
            } else if (arg.equals("--autostart")) {
                autoStart = true;
            } else if (arg.startsWith("--token=")) {
                token = arg.substring(8);
            } else if (arg.equals("--help") || arg.equals("-h")) {
                printHelp();
                System.exit(0);
            }
        }

        // Print banner
        System.out.println("==============================================");
        System.out.println("  VisualSceneMaker Runtime Server");
        System.out.println("==============================================");
        System.out.println();

        // Start the runtime server using WebUiServer in RUNTIME_ONLY mode
        WebUiServer server = WebUiServer.getInstance();
        String bindHost = allowLan ? "0.0.0.0" : "127.0.0.1";

        try {
            server.start(port, bindHost, token, WebUiServer.ServerMode.RUNTIME_ONLY);

            System.out.println("Runtime server started:");
            System.out.println("  URL:   " + server.getLocalUrl());
            System.out.println("  Token: " + server.getAuthToken());
            System.out.println("  Mode:  Runtime-only (no editing)");
            if (allowLan) {
                System.out.println("  WARNING: External connections allowed!");
            }
            System.out.println();

            // Load project if specified
            if (projectPath != null) {
                System.out.println("Loading project: " + projectPath);
                if (server.loadProject(projectPath)) {
                    System.out.println("Project loaded successfully.");

                    if (autoStart) {
                        System.out.println("Starting runtime...");
                        if (server.startRuntime()) {
                            System.out.println("Runtime started.");
                        } else {
                            System.err.println("Failed to start runtime.");
                        }
                    }
                } else {
                    System.err.println("Failed to load project: " + projectPath);
                }
            }

            System.out.println();
            System.out.println("Open Web UI at: " + server.getLocalUrl());
            System.out.println("Press Ctrl+C to stop the server.");

            // Keep the main thread alive
            Thread.currentThread().join();

        } catch (Exception e) {
            System.err.println("Failed to start runtime server: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }

    private static void printHelp() {
        System.out.println("VisualSceneMaker Runtime Server");
        System.out.println();
        System.out.println("Usage: java -jar runtime-server.jar [options]");
        System.out.println();
        System.out.println("Options:");
        System.out.println("  --port=PORT       Server port (default: 8091)");
        System.out.println("  --allow-lan       Bind to 0.0.0.0 (allow external connections)");
        System.out.println("  --project=PATH    Auto-load project on startup");
        System.out.println("  --autostart       Automatically start runtime after loading");
        System.out.println("  --token=TOKEN     Set authentication token (default: auto-generated)");
        System.out.println("  --help, -h        Show this help message");
        System.out.println();
        System.out.println("Examples:");
        System.out.println("  java -jar runtime-server.jar --port=8091");
        System.out.println("  java -jar runtime-server.jar --project=/path/to/project.xml --autostart");
        System.out.println("  java -jar runtime-server.jar --allow-lan --port=9000");
    }
}
