package de.dfki.vsm.test;

import de.dfki.vsm.editor.EditorProject;
import de.dfki.vsm.runtime.project.RunTimeProject;
import java.io.File;

/**
 *
 * @author Gregor Mehlmann
 */
public class TestVSMProject {

    public static void main(String args[]) {

        // Create a new runtime project
        final RunTimeProject rproject = new RunTimeProject();
        rproject.load(new File(args[0]));
        rproject.save(new File("_runtime"));

        final EditorProject eproject = new EditorProject(new File(args[0]));
        eproject.save(new File("_editor1"));
        eproject.setProjectName("other");
        eproject.save(new File("_editor2"));
        

    }
}
