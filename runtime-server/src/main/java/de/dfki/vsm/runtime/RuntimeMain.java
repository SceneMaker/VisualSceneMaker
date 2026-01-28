package de.dfki.vsm.runtime;

import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.web.WebUiServer;

import java.io.File;

/**
 * Standalone runtime server entry point.
 *
 * This provides a headless runtime server that can:
 * - Load and execute VSM projects
 * - Expose a REST/WebSocket API for runtime control and monitoring
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
 *   --help            Show this help message
 */
public class RuntimeMain {

    private static final int DEFAULT_PORT = 8091;

    public static void main(String[] args) {
        int port = DEFAULT_PORT;
        boolean allowLan = false;
        String projectPath = null;
        boolean autoStart = false;

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

        // Start the web server
        WebUiServer server = WebUiServer.getInstance();
        String bindHost = allowLan ? "0.0.0.0" : "127.0.0.1";

        try {
            server.start(port, allowLan);

            System.out.println("Runtime server started:");
            System.out.println("  URL: http://" + (allowLan ? "0.0.0.0" : "localhost") + ":" + port);
            System.out.println("  Mode: Runtime-only (no editing)");
            if (allowLan) {
                System.out.println("  WARNING: External connections allowed!");
            }
            System.out.println();

            // Load project if specified
            if (projectPath != null) {
                File projectFile = new File(projectPath);
                if (projectFile.exists()) {
                    System.out.println("Loading project: " + projectPath);
                    RunTimeProject project = new RunTimeProject(projectFile);
                    if (project.launch()) {
                        System.out.println("Project loaded successfully.");

                        // Register with WebUiServer so it's accessible via API
                        String projectId = server.registerProject(project);
                        System.out.println("Project registered with ID: " + projectId);

                        if (autoStart) {
                            System.out.println("Starting runtime...");
                            if (project.start()) {
                                System.out.println("Runtime started.");
                                // Update the runtime state so Web UI sees it as running
                                server.setProjectRuntimeState(projectId, "running");
                            } else {
                                System.err.println("Failed to start runtime.");
                            }
                        }
                    } else {
                        System.err.println("Failed to load project: " + projectPath);
                    }
                } else {
                    System.err.println("Project file not found: " + projectPath);
                }
            }

            System.out.println();
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
        System.out.println("  --help, -h        Show this help message");
        System.out.println();
        System.out.println("Examples:");
        System.out.println("  java -jar runtime-server.jar --port=8091");
        System.out.println("  java -jar runtime-server.jar --project=/path/to/project.xml --autostart");
        System.out.println("  java -jar runtime-server.jar --allow-lan --port=9000");
    }
}
