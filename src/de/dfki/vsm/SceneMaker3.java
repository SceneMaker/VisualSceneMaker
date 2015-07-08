package de.dfki.vsm;

import de.dfki.vsm.editor.EditorProject;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.runtime.RunTimeInstance;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.io.File;
import javax.swing.SwingUtilities;

/**
 * @author Gregor Mehlmann
 */
public class SceneMaker3 {

    // The logger instance of SceneMaker3
    private final static LOGDefaultLogger sLogger
            = LOGDefaultLogger.getInstance();

    // Start SceneMaker3 in a specific mode
    public static void main(final String[] args) {
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public final void run() {

                if (args.length == 2) {
                    // Read the command line arguments
                    final String mode = args[0];
                    final String name = args[1];
                    // Create a project configuration 
                    final File file = new File(name);
                    if (file.exists()) {
                        // Check the option from the aguments
                        if (mode.equalsIgnoreCase("runtime")) {
                            //
                            final RunTimeProject data = new RunTimeProject(file);
                            //
                            final RunTimeInstance sRunTime = RunTimeInstance.getInstance();

                            sRunTime.launch(data);
                            // Do Something
                            sRunTime.unload(data);
                        } else if (mode.equalsIgnoreCase("editor")) {
                            //
                            //final EditorProject data = new EditorProject(file);
                            //
                            final EditorInstance sEditor = EditorInstance.getInstance();
                            // Show the singelton editor instance
                            sEditor.setVisible(true);
                            //sEditor.openProject(data);
                        } else {
                            // Print an error message in this case
                            sLogger.failure("Error: Unknown mode specification");
                        }
                    }
                } else {
                    // Print an error message in this case
                    sLogger.failure("Error: Wrong number of arguments");
                }
            }
        });
    }
}
