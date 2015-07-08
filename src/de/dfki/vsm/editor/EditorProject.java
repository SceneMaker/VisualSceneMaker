package de.dfki.vsm.editor;

import de.dfki.vsm.model.project.EditorConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import java.io.File;

/**
 *
 * @author Gregor Mehlmann
 */
public class EditorProject extends RunTimeProject {

    // The file of the project
    private File mProjectFile;
    // The hash of the project
    private int mInitialHash;
    // The project pending flag
    private boolean mIsPending;
    // The editor configuration
    private final EditorConfig mEditorConfig
            = new EditorConfig();

    public EditorProject(final File file) {
        // Initialize the pending flag
        mIsPending = false;
        // Initialize the project file
        mProjectFile = file.getAbsoluteFile();
        // Load the editor configuration
        mEditorConfig.load(file);
        // Initialize the project data
        super.load(file);
    }

    // Load the project data
    public final boolean load() {
        return super.load(mProjectFile)
                && mEditorConfig.load(mProjectFile);
    }

    // Load the project data
    @Override
    public final boolean load(final File file) {
        // First set the project file 
        setProjectFile(file);
        // And then load the project
        return load();
    }

    // Save the project data
    public final boolean save() {
        return super.save(mProjectFile)
                && mEditorConfig.save(mProjectFile);
    }

    // Save the project data
    @Override
    public final boolean save(final File file) {
        // First set the project file 
        setProjectFile(file);
        // And then save the project
        return save();
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

    public void setInitialHash(int value) {
        mInitialHash = value;
    }

    public final boolean isPending() {
        return mIsPending;
    }

    public final void setPending(final boolean state) {
        mIsPending = state;
    }

    // TODO: Does actually not work correct!
    public final boolean hasChanged() {
        boolean hasChanged = false;
        if (mInitialHash != getHashCode()) {
            hasChanged = true;
        }
        return hasChanged;
    }
}
