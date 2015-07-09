package de.dfki.vsm;

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.runtime.RunTimeInstance;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.io.File;
import javax.swing.SwingUtilities;

/**
 * @author Not me
 */
public class SceneMaker3 {

    // The logger instance of SceneMaker3
    private final static LOGDefaultLogger sLogger
            = LOGDefaultLogger.getInstance();

    // Start SceneMaker3 in a specific mode
    public static void main(final String[] args) {
        // Let Java Swing do the work for us
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public final void run() {
                // Check if we have at least one argument
                if (args.length > 0) {
                    // Read the first command line argument
                    final String mode = args[0];
                    // Check the options from this agument
                    if (mode.equals("runtime")) {
                        // In runtime mode we need a project file
                        if (args.length == 2) {
                            // Get project file name agrument
                            final String name = args[1];
                            // Create the project configuration 
                            final File file = new File(name);
                            if (file.exists()) {
                                startRunTime(file);
                            } else {
                                fileError(file);
                            }
                        } else {
                            usageError();
                        }
                    } else if (mode.equalsIgnoreCase("editor")) {
                        if (args.length == 2) {
                            // Get project file name agrument
                            final String name = args[1];
                            // Create the project configuration 
                            final File file = new File(name);
                            if (file.exists()) {
                                startEditor(file);
                            } else {
                                fileError(file);
                            }
                        } else {
                            usageError();
                        }
                    } else {
                        usageError();
                    }
                } else {
                    startEditor();
                }
            }
        });
    }

    // Start the editor without a project
    private static void startEditor() {
        // Get the singelton editor instance
        final EditorInstance sEditor = EditorInstance.getInstance();
        // Show the singelton editor instance
        sEditor.setVisible(true);
    }

    // Start the editor with some project
    private static void startEditor(final File file) {
        // Get the singelton editor instance
        final EditorInstance sEditor = EditorInstance.getInstance();
        // Get an editor project from file 
        sEditor.openProject(file);
        // Show the singelton editor instance
        sEditor.setVisible(true);
    }

    // Start the runtime with some project
    private static void startRunTime(final File file) {
        // Get an editor project from file 
        final RunTimeProject data = new RunTimeProject(file);
        // Get the singelton runtime instance
        final RunTimeInstance sRunTime = RunTimeInstance.getInstance();
        // Launch the runtime with the project
        sRunTime.launch(data);
        // Unload the project from the runtime  
        sRunTime.unload(data);
    }

    // Print usage when usage error happened
    private static void usageError() {
        sLogger.failure("Error: Usage: [runtime|editor] [filename]");
    }

    // Print error when a file error happened
    private static void fileError(final File file) {
        sLogger.failure("Error: Cannot find file '" + file.getAbsolutePath() + "'");
    }
}
