package de.dfki.vsm.test;

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.editor.project.ProjectEditor;
import java.io.File;

/**
 *
 * @author Not me
 */
public class TestVSMProject {

    public static void main(String args[]) {

        /*
         // Create a new runtime project
         final RunTimeProject rproject = new RunTimeProject();
         rproject.parse(new File(args[0]));
         RunTimeInstance.getInstance().launch(rproject);
         //RunTimeInstance.getInstance().launch(rproject);
         //RunTimeInstance.getInstance().unload(rproject);
         //RunTimeInstance.getInstance().unload(rproject);
         rproject.write(new File("_rt1"));
         rproject.setProjectName("lala");
         rproject.write(new File("_rt2"));
         */
        final EditorInstance instance = EditorInstance.getVisualizer();
        instance.setVisible(true);

        try {
            Thread.sleep(1000);
        } catch (Exception exc) {
        }

        final EditorProject project = new EditorProject();
        project.parse(new File("res/prj/test").getPath());

        final ProjectEditor editor = instance.showProject(project);

        try {
            Thread.sleep(10000);
        } catch (Exception exc) {
        }

        instance.hideProject(editor);

    }
}
