package de.dfki.vsm.editor.connection;

import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.editor.service.EditorProjectService;
import de.dfki.vsm.util.log.LOGDefaultLogger;

import java.io.File;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

/**
 * Handles synchronization of projects between editor and remote runtime servers.
 *
 * Phase 1 Implementation: File-based synchronization
 * - Editor sends project path to runtime
 * - Runtime loads project from local filesystem
 * - Assumes shared filesystem or network share
 *
 * Future Enhancement: Network-based synchronization
 * - Serialize entire project to JSON
 * - Transfer over network
 * - Reconstruct on runtime side
 *
 * @author Phase 5 Refactoring - 2026-01-12
 */
public class ProjectSynchronizer {

    private static final LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();
    private static final long SYNC_TIMEOUT_MS = 10000; // 10 seconds

    private final EditorProjectService mProjectService;

    /**
     * Synchronization result.
     */
    public static class SyncResult {
        public final boolean success;
        public final String message;
        public final String projectPath;

        public SyncResult(boolean success, String message, String projectPath) {
            this.success = success;
            this.message = message;
            this.projectPath = projectPath;
        }

        public static SyncResult success(String projectPath) {
            return new SyncResult(true, "Project synchronized successfully", projectPath);
        }

        public static SyncResult failure(String message) {
            return new SyncResult(false, message, null);
        }
    }

    /**
     * Synchronization mode.
     */
    public enum SyncMode {
        /**
         * File-based: Send project path, runtime loads from filesystem.
         * Requires shared filesystem or network share.
         */
        FILE_BASED,

        /**
         * Network-based: Serialize and transfer entire project over network.
         * Not yet implemented.
         */
        NETWORK_BASED
    }

    public ProjectSynchronizer(EditorProjectService projectService) {
        this.mProjectService = projectService;
    }

    /**
     * Synchronizes a project to a remote runtime server.
     *
     * @param project EditorProject to synchronize
     * @param connection RuntimeConnection to sync to
     * @return SyncResult indicating success or failure
     */
    public SyncResult syncToRuntime(EditorProject project, RuntimeConnection connection) {
        return syncToRuntime(project, connection, SyncMode.FILE_BASED);
    }

    /**
     * Synchronizes a project to a remote runtime server using specified mode.
     *
     * @param project EditorProject to synchronize
     * @param connection RuntimeConnection to sync to
     * @param mode Synchronization mode
     * @return SyncResult indicating success or failure
     */
    public SyncResult syncToRuntime(EditorProject project, RuntimeConnection connection, SyncMode mode) {
        if (project == null) {
            return SyncResult.failure("Project is null");
        }

        if (connection == null) {
            return SyncResult.failure("Runtime connection is null");
        }

        if (!connection.isConnected()) {
            return SyncResult.failure("Runtime connection is not connected");
        }

        switch (mode) {
            case FILE_BASED:
                return syncFileBasedLegacy(project, connection);

            case NETWORK_BASED:
                return SyncResult.failure("Network-based sync not yet implemented");

            default:
                return SyncResult.failure("Unknown sync mode: " + mode);
        }
    }

    /**
     * File-based synchronization (Phase 1 implementation).
     *
     * Approach:
     * 1. Ensure project is saved to disk
     * 2. Get project path
     * 3. Send path to runtime server
     * 4. Runtime server loads from its filesystem
     *
     * Limitations:
     * - Requires shared filesystem or network share
     * - Path must be accessible from runtime server
     * - No validation that path exists on runtime side
     */
    private SyncResult syncFileBasedLegacy(EditorProject project, RuntimeConnection connection) {
        sLogger.message("Synchronizing project to runtime: " + connection.getName());

        // Step 1: Ensure project is saved
        if (project.isPending()) {
            return SyncResult.failure("Project has not been saved. Please save project first.");
        }

        // Check if project has unsaved changes
        if (mProjectService.isProjectDirty(project)) {
            sLogger.warning("Project has unsaved changes. Saving before sync...");

            if (!mProjectService.saveProject(project)) {
                return SyncResult.failure("Failed to save project before synchronization");
            }
        }

        // Step 2: Get project path
        String projectPath = project.getProjectPath();
        if (projectPath == null || projectPath.isEmpty()) {
            return SyncResult.failure("Project path is not set");
        }

        // Verify project directory exists
        File projectDir = new File(projectPath);
        if (!projectDir.exists() || !projectDir.isDirectory()) {
            return SyncResult.failure("Project directory does not exist: " + projectPath);
        }

        // Step 3: Send load command to runtime
        sLogger.message("Loading project on runtime server: " + projectPath);

        try {
            boolean loaded = connection.loadProject(projectPath);

            if (loaded) {
                // Wait a moment for runtime to fully load
                Thread.sleep(500);

                // Verify runtime loaded the project
                RuntimeConnection.RuntimeStatus status = connection.getStatus();
                if (status != null && !status.projectPath.isEmpty()) {
                    sLogger.message("Project synchronized successfully: " + status.projectName);
                    return SyncResult.success(projectPath);
                } else {
                    return SyncResult.failure("Runtime failed to load project (status check failed)");
                }
            } else {
                return SyncResult.failure("Runtime failed to load project");
            }

        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return SyncResult.failure("Synchronization interrupted");
        } catch (Exception e) {
            sLogger.failure("Error during synchronization: " + e.getMessage());
            return SyncResult.failure("Error: " + e.getMessage());
        }
    }

    /**
     * Synchronizes project asynchronously.
     *
     * @param project EditorProject to synchronize
     * @param connection RuntimeConnection to sync to
     * @return CompletableFuture with SyncResult
     */
    public CompletableFuture<SyncResult> syncToRuntimeAsync(EditorProject project, RuntimeConnection connection) {
        return CompletableFuture.supplyAsync(() -> syncToRuntime(project, connection))
            .orTimeout(SYNC_TIMEOUT_MS, TimeUnit.MILLISECONDS)
            .exceptionally(throwable -> {
                sLogger.failure("Async sync failed: " + throwable.getMessage());
                return SyncResult.failure("Timeout or error: " + throwable.getMessage());
            });
    }

    /**
     * Unloads project from runtime.
     *
     * @param connection RuntimeConnection to unload from
     * @return true if successful
     */
    public boolean unloadFromRuntime(RuntimeConnection connection) {
        if (connection == null || !connection.isConnected()) {
            return false;
        }

        sLogger.message("Unloading project from runtime: " + connection.getName());
        return connection.unload();
    }

    /**
     * Checks if a project path is accessible for file-based sync.
     *
     * @param projectPath Path to check
     * @return true if path exists and is a directory
     */
    public boolean isPathAccessible(String projectPath) {
        if (projectPath == null || projectPath.isEmpty()) {
            return false;
        }

        File dir = new File(projectPath);
        return dir.exists() && dir.isDirectory();
    }

    /**
     * Gets synchronization mode recommendation based on connection and project.
     *
     * @param project EditorProject to sync
     * @param connection RuntimeConnection to sync to
     * @return Recommended SyncMode
     */
    public SyncMode getRecommendedMode(EditorProject project, RuntimeConnection connection) {
        // For Phase 1, always recommend file-based
        // In future, could check if connection is local vs remote and recommend accordingly
        return SyncMode.FILE_BASED;
    }

    /**
     * Validates that a project can be synchronized.
     *
     * @param project EditorProject to validate
     * @return Validation result message (empty if valid)
     */
    public String validateProjectForSync(EditorProject project) {
        if (project == null) {
            return "Project is null";
        }

        if (project.isPending()) {
            return "Project has not been saved to disk";
        }

        String path = project.getProjectPath();
        if (path == null || path.isEmpty()) {
            return "Project path is not set";
        }

        if (!isPathAccessible(path)) {
            return "Project directory is not accessible: " + path;
        }

        return ""; // Valid
    }
}
