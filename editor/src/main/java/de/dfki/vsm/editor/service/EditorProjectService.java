package de.dfki.vsm.editor.service;

import de.dfki.vsm.Preferences;
import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;

import java.io.File;
import java.util.*;

/**
 * Headless service for managing EditorProject lifecycle.
 * Extracted from EditorInstance to remove Swing dependencies.
 *
 * @author Phase 2 Refactoring - 2026-01-11
 */
public class EditorProjectService {

    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    // Store for open projects (project ID -> EditorProject)
    private final Map<String, EditorProject> projects = new HashMap<>();

    // Recent projects list (in-memory cache)
    private final List<RecentProject> recentProjects = new ArrayList<>();

    // Maximum number of recent projects to track
    private static final int MAX_RECENT_PROJECTS = 10;

    public EditorProjectService() {
        // Load recent projects from preferences on initialization
        loadRecentProjects();
    }

    /**
     * Creates a new empty project.
     *
     * Extracted from: EditorInstance.newProject() - line 290
     *
     * @param name Project name
     * @return New EditorProject instance
     */
    public EditorProject createProject(String name) {
        mLogger.message("Creating new project: " + name);

        // Create new EditorProject with isNewProject = true
        EditorProject project = new EditorProject(true);

        // Set project name
        project.setProjectName(name);

        // Set default name for main superNode (from EditorConfig)
        String defaultSuperNodeName = project.getEditorConfig().sMAINSUPERNODENAME;
        project.getSceneFlow().setName(defaultSuperNodeName);

        // Generate unique project ID
        String projectId = UUID.randomUUID().toString();

        // Store in projects map
        projects.put(projectId, project);

        mLogger.message("Created new project with ID: " + projectId);

        return project;
    }

    /**
     * Opens an existing project from filesystem.
     *
     * Extracted from: EditorInstance.openProject(String path) - line 380
     *
     * @param path Path to project directory
     * @return Loaded EditorProject instance, or null if failed
     */
    public EditorProject openProject(String path) {
        if (path == null) {
            mLogger.failure("Error: Cannot open editor project from a bad path");
            return null;
        }

        mLogger.message("Opening project from: " + path);

        // Create new EditorProject
        EditorProject project = new EditorProject();

        // Try to parse/load it from the file
        if (project.parse(path)) {
            // Generate unique project ID
            String projectId = UUID.randomUUID().toString();

            // Store in projects map
            projects.put(projectId, project);

            // Update the recent project list
            addRecentProject(project.getProjectPath(), project.getProjectName());

            mLogger.message("Opened project: " + project.getProjectName());

            return project;
        } else {
            mLogger.failure("Error: Cannot load editor project from path: " + path);
            return null;
        }
    }

    /**
     * Saves project to its current location.
     *
     * Extracted from: EditorInstance.save(ProjectEditor editor) - line 427
     *
     * @param project Project to save
     * @return true if successful
     */
    public boolean saveProject(EditorProject project) {
        if (project == null) {
            mLogger.failure("Error: Cannot save a null project");
            return false;
        }

        // Check if the project is pending (not yet saved to a file)
        if (project.isPending()) {
            mLogger.warning("Warning: Project is pending (no file location). Use saveProjectAs instead.");
            return false;
        }

        mLogger.message("Saving project: " + project.getProjectName());

        // Try to write the editor project to its current location
        if (project.write()) {
            // Update recent project list
            addRecentProject(project.getProjectPath(), project.getProjectName());

            mLogger.message("Saved project: " + project.getProjectName());
            return true;
        } else {
            mLogger.failure("Error: Cannot write the editor project '" + project.getProjectName() + "'");
            return false;
        }
    }

    /**
     * Saves project to a new location (Save As).
     *
     * Extracted from: EditorInstance.saveAs(ProjectEditor editor) - line 541
     *
     * @param project Project to save
     * @param newPath New path for project (directory)
     * @return true if successful
     */
    public boolean saveProjectAs(EditorProject project, String newPath) {
        if (project == null) {
            mLogger.failure("Error: Cannot save a null project");
            return false;
        }

        if (newPath == null || newPath.isEmpty()) {
            mLogger.failure("Error: Cannot save project to invalid path");
            return false;
        }

        mLogger.message("Saving project as: " + newPath);

        // Create File from path
        File file = new File(newPath);

        // Try to write the editor project to new location
        if (project.write(file)) {
            // Update recent project list
            addRecentProject(project.getProjectPath(), project.getProjectName());

            mLogger.message("Saved project as: " + newPath);
            return true;
        } else {
            mLogger.failure("Error: Cannot write the editor project '" + project.getProjectName() + "' to " + newPath);
            return false;
        }
    }

    /**
     * Closes a project.
     *
     * Extracted from: EditorInstance.close(ProjectEditor editor) - line 606
     *
     * Note: This method does NOT prompt user to save changes. Caller should check
     * isProjectDirty() and handle save prompts before calling this method.
     *
     * @param projectId Project ID to close
     * @return true if successful
     */
    public boolean closeProject(String projectId) {
        EditorProject project = projects.get(projectId);

        if (project == null) {
            mLogger.warning("Warning: Project ID not found: " + projectId);
            return false;
        }

        mLogger.message("Closing project: " + project.getProjectName());

        // Unload runtime if it was launched
        if (project.wasExecuted()) {
            project.unload();
        }

        // Remove from projects map
        projects.remove(projectId);

        mLogger.message("Closed project: " + project.getProjectName());
        return true;
    }

    /**
     * Checks if project has unsaved changes.
     *
     * @param project Project to check
     * @return true if project has unsaved changes
     */
    public boolean isProjectDirty(EditorProject project) {
        if (project == null) {
            return false;
        }
        return project.hasChanged();
    }

    /**
     * Marks project as clean (no unsaved changes).
     *
     * Note: EditorProject handles dirty tracking automatically when parse() or write()
     * is called. This method is a no-op since the dirty flag is managed internally.
     *
     * @param project Project to mark clean
     */
    public void markProjectClean(EditorProject project) {
        // No-op: EditorProject automatically updates mInitialHash when write() is called
        // The dirty tracking is handled internally by comparing mInitialHash to getHashCode()
    }

    /**
     * Adds project to recent projects list.
     *
     * Extracted from: EditorInstance.updateRecentProjects() - line 744
     *
     * @param path Project path
     * @param name Project name
     */
    public void addRecentProject(String path, String name) {
        if (path == null || name == null) {
            return;
        }

        // Skip sample and tutorial projects
        if (path.contains("sample") || path.contains("tutorial")) {
            return;
        }

        // Check if project already exists in recent list
        RecentProject existing = null;
        for (RecentProject rp : recentProjects) {
            if (rp.path.equals(path)) {
                existing = rp;
                break;
            }
        }

        if (existing != null) {
            // Remove existing entry (we'll add it to the front)
            recentProjects.remove(existing);
        }

        // Add to beginning of list (most recent first)
        RecentProject recentProject = new RecentProject(path, name, System.currentTimeMillis());
        recentProjects.add(0, recentProject);

        // Limit to MAX_RECENT_PROJECTS
        while (recentProjects.size() > MAX_RECENT_PROJECTS) {
            recentProjects.remove(recentProjects.size() - 1);
        }

        // Save to preferences
        saveRecentProjects();

        mLogger.message("Updated recent projects with: " + name);
    }

    /**
     * Gets list of recent projects.
     * @return List of recent projects (most recent first)
     */
    public List<RecentProject> getRecentProjects() {
        return new ArrayList<>(recentProjects);
    }

    /**
     * Gets all currently open projects.
     * @return Map of project ID -> EditorProject
     */
    public Map<String, EditorProject> getOpenProjects() {
        return new HashMap<>(projects);
    }

    /**
     * Gets a specific open project by ID.
     * @param projectId Project ID
     * @return EditorProject or null if not found
     */
    public EditorProject getProject(String projectId) {
        return projects.get(projectId);
    }

    /**
     * Finds project ID by EditorProject instance.
     * @param project EditorProject to find
     * @return Project ID or null if not found
     */
    public String getProjectId(EditorProject project) {
        for (Map.Entry<String, EditorProject> entry : projects.entrySet()) {
            if (entry.getValue() == project) {
                return entry.getKey();
            }
        }
        return null;
    }

    /**
     * Loads recent projects from preferences.
     */
    private void loadRecentProjects() {
        recentProjects.clear();

        for (int i = 0; i < MAX_RECENT_PROJECTS; i++) {
            String path = Preferences.getProperty("recentproject." + i + ".path");
            String name = Preferences.getProperty("recentproject." + i + ".name");
            String dateStr = Preferences.getProperty("recentproject." + i + ".date");

            if (path != null && name != null) {
                long timestamp = 0;
                if (dateStr != null) {
                    try {
                        timestamp = Long.parseLong(dateStr);
                    } catch (NumberFormatException e) {
                        timestamp = System.currentTimeMillis();
                    }
                }
                recentProjects.add(new RecentProject(path, name, timestamp));
            }
        }
    }

    /**
     * Saves recent projects to preferences.
     */
    private void saveRecentProjects() {
        // Clear old entries
        for (int i = 0; i < MAX_RECENT_PROJECTS; i++) {
            Preferences.removeProperty("recentproject." + i + ".path");
            Preferences.removeProperty("recentproject." + i + ".name");
            Preferences.removeProperty("recentproject." + i + ".date");
        }

        // Save current recent projects
        for (int i = 0; i < recentProjects.size() && i < MAX_RECENT_PROJECTS; i++) {
            RecentProject rp = recentProjects.get(i);
            Preferences.setProperty("recentproject." + i + ".path", rp.path);
            Preferences.setProperty("recentproject." + i + ".name", rp.name);
            Preferences.setProperty("recentproject." + i + ".date", String.valueOf(rp.lastOpened));
        }

        // Save preferences to disk
        Preferences.save();
    }

    /**
     * Represents a recent project entry.
     */
    public static class RecentProject {
        public final String path;
        public final String name;
        public final long lastOpened;

        public RecentProject(String path, String name, long lastOpened) {
            this.path = path;
            this.name = name;
            this.lastOpened = lastOpened;
        }

        @Override
        public String toString() {
            return name + " (" + path + ")";
        }
    }
}
