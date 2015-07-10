package de.dfki.vsm.editor.project;

import de.dfki.vsm.model.project.EditorConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import java.io.File;

/**
 * @author Gregor Mehlmann
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
    public final boolean parse(final File file) {
        // Check if the file is null
        if (file == null) {
            // Print an error message
            mLogger.failure("Error: Cannot parse editor project from a bad file");
            // Return false at error
            return false;
        }
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
        return parse();
    }

    // Save the editor project
    @Override
    public final boolean write(final File file) {
        // Check if the file is null
        if (file == null) {
            // Print an error message
            mLogger.failure("Error: Cannot write editor project into a bad file");
            // Return false at error
            return false;
        }
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
        return write();
    }

    // Load the project data
    public final boolean parse() {
        // Check the project file
        if (mProjectFile == null) {
            // Print an error message
            mLogger.failure("Error: Cannot parse editor project from a bad file");
            // Return false at error
            return false;
        }
        // Load the project data
        if (super.parse(mProjectFile)
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
    public final boolean write() {
        // Check the project file
        if (mProjectFile == null) {
            // Print an error message
            mLogger.failure("Error: Cannot write editor project into a bad file");
            // Return false at error
            return false;
        }
        // Save the project data
        if (super.write(mProjectFile)
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

    // Get the project base directory
    public final File getProjectFile() {
        return mProjectFile.getAbsoluteFile();
    }

    // Set the project base directory
    public final void setProjectFile(final File file) {
        mProjectFile = file.getAbsoluteFile();
    }

    // Get the editor configuration
    public final EditorConfig getEditorConfig() {
        return mEditorConfig;
    }

    public final String getProjectPath() {
        return mProjectFile.getAbsolutePath();
    }

    // Check if the hash code has changed
    public final boolean hasChanged() {
        return (mInitialHash != getHashCode());
    }
}
