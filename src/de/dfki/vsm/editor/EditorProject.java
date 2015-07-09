package de.dfki.vsm.editor;

import de.dfki.vsm.model.project.EditorConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import java.io.File;

/**
 * @author Not me
 */
public class EditorProject extends RunTimeProject {

    // The file of the project
    private File mProjectFile;
    // The hash of the project
    private int mInitialHash;
    // The editor configuration
    private final EditorConfig mEditorConfig
            = new EditorConfig();

    // Construct an editor project
    public EditorProject() {
        // Initialize the project file
        mProjectFile = null;
        // Initialize the initial hash
        mInitialHash = getHashCode();
    }

    // Load the editor project
    @Override
    public final boolean load(final File file) {
        // Get the absolute file 
        final File base = file.getAbsoluteFile();
        // Check if file exists
        if (!base.exists()) {
            // Print an error message
            mLogger.failure("Error: Cannot find editor project file '" + base + "'");
            // Return failure here
            return false;
        }
        // First set the project file 
        mProjectFile = base;
        // And then load the project
        return load();
    }

    // Save the editor project
    @Override
    public final boolean save(final File file) {
        // Get the absolute file 
        final File base = file.getAbsoluteFile();
        // Check if file exists
        if (!base.exists()) {
            // Print an error message
            mLogger.failure("Error: Cannot find editor project file '" + base + "'");
            // Return failure here
            return false;
        }
        // First set the project file 
        mProjectFile = base;
        // And then save the project
        return save();
    }

    // Load the project data
    public final boolean load() {
        // Check the project file
        if (mProjectFile == null) {
            return false;
        }
        // Load the project data
        if (super.load(mProjectFile)
                && mEditorConfig.load(mProjectFile)) {
            // Set the initial hash code
            mInitialHash = getHashCode();
            // Return true if project is saved
            return true;
        } else {
            // Return false when saving failed
            return false;
        }
    }

    // Save the project data
    public final boolean save() {
        // Check the project file
        if (mProjectFile == null) {
            return false;
        }
        // Save the project data
        if (super.save(mProjectFile)
                && mEditorConfig.save(mProjectFile)) {
            // Reset the initial hash code here
            mInitialHash = getHashCode();
            // Return true when project is saved
            return true;
        } else {
            // Return false when saving failed
            return false;
        }
    }

    public final File getProjectFile() {
        return mProjectFile.getAbsoluteFile();
    }

    public final void setProjectFile(final File file) {
        mProjectFile = file.getAbsoluteFile();
    }

    public final String getProjectPath() {
        return mProjectFile.getAbsolutePath();
    }

    public final EditorConfig getEditorConfig() {
        return mEditorConfig;
    }

    // Check if the hash code has changed
    public final boolean hasChanged() {
        return (mInitialHash != getHashCode());
    }
}
