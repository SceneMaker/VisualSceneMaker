package de.dfki.vsm;

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.runtime.instance.RunTimeInstance;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.io.File;
import java.io.IOException;
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
                            // Get the project file name agrument
                            final String name = args[1];
                            // Create the project configuration 
                            final File file = new File(name);
                            if (file.exists()) {
                                runtime(file);
                            } else {
                                error(file);
                            }
                        } else {
                            usage();
                        }
                    } else if (mode.equalsIgnoreCase("editor")) {
                        if (args.length == 2) {
                            // Get the project file name agrument
                            final String name = args[1];
                            // Create the project configuration 
                            final File file = new File(name);
                            if (file.exists()) {
                                editor(file);
                            } else {
                                error(file);
                            }
                        } else {
                            usage();
                        }
                    } else {
                        usage();
                    }
                } else {
                    editor();
                }
            }
        });
    }

    // Start the editor without a project
    private static void editor() {
        // Get the singelton editor instance
        final EditorInstance sEditor = EditorInstance.getInstance();
        // Show the singelton editor instance
        sEditor.setVisible(true);
    }

    // Start the editor with some project
    private static void editor(final File file) {
        // Get the singelton editor instance
        final EditorInstance sEditor = EditorInstance.getInstance();
        // Get an editor project from file 
        sEditor.openProject(file);
        // Show the singelton editor instance
        sEditor.setVisible(true);
    }

    // Start the runtime with some project
    private static void runtime(final File file) {
        // Get an editor project from file 
        final RunTimeProject data = new RunTimeProject(file);
        // Get the singelton runtime instance
        final RunTimeInstance sRunTime = RunTimeInstance.getInstance();
        // Launch the runtime with the project
        if (sRunTime.launch(data)) {
            // Start the runtime with the project
            if (sRunTime.start(data)) {
                // Wait until user aborts execution
                System.err.println("Press Key To Abort ...");
                // TODO: Stop waiting if execution
                // has been aborted in another way
                try {
                    final int in = System.in.read();
                    if (in != -1) {
                        // Aborting the execution now
                    }
                } catch (final IOException exc) {
                    // Do nothing
                } finally {
                    // Abort the runtime with the project
                    sRunTime.abort(data);
                    // Unload the project from the runtime  
                    sRunTime.unload(data);
                }

            }
        }
    }

// Print usage when usage error happened
    private static void usage() {
        sLogger.failure("Error: Usage: [runtime|editor] [filename]");
    }

    // Print error when a file error happened
    private static void error(final File file) {
        sLogger.failure("Error: Cannot find file '" + file.getAbsolutePath() + "'");
    }
}
