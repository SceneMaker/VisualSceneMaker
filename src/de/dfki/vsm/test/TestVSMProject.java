package de.dfki.vsm.test;

import de.dfki.vsm.editor.project.EditorProject;
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
     
         final EditorProject eproject = new EditorProject();
         eproject.parse(new File(args[0]).getPath());
         eproject.write(new File("_editor1"));
         eproject.setProjectName("other");
         eproject.write(new File("_editor2"));
       
    }
}
