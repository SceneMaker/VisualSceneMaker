package de.dfki.vsm;

import de.dfki.vsm.api.aldebaran.NAOScenePlayer;
import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.model.project.ProjectData;
import javax.swing.SwingUtilities;

/**
 * @author Gregor Mehlmann
 */
public class DefaultEditor {

    // The Singelton Editor Instance
    private final static Editor sVSMEditor = Editor.getInstance();

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public static void main(final String[] args) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public final void run() {
                // Show Singelton Editor Instance
                show();
            }
        });
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public static void show() {
        // Show Singelton Editor Instance
        if (sVSMEditor != null) {
            sVSMEditor.setVisible(true);
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public static void show(final ProjectData data) {
        // Show Singelton Editor Instance
        if (sVSMEditor != null) {
            sVSMEditor.setVisible(true);
            sVSMEditor.openProject(data);
        }
    }
}
