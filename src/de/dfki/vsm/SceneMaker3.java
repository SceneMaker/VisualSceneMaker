package de.dfki.vsm;

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.runtime.RunTimeInstance;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.io.File;
import java.io.IOException;
import javax.swing.SwingUtilities;

/**
 * @author Gregor Mehlmann
 */
public final class SceneMaker3 {

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
        //
        sLogger.message("Starting VSM editor with file '" + file + "'");
        // Get the singelton runtime instance
        final RunTimeInstance sRunTime = RunTimeInstance.getInstance();
        // Get the singelton editor instance
        final EditorInstance sEditor = EditorInstance.getInstance();
        // Get an editor project from file 
        sEditor.openProject(file.getPath());
        // Show the singelton editor instance
        sEditor.setVisible(true);
//  
//         // Do something for Patrick ...
//         final Thread control = new Thread() {
//         // Termination flag
//         private boolean mDone = false;
//         // Get the input
//         private final BufferedReader mReader = new BufferedReader(
//         new InputStreamReader(System.in));
//         // Get the project 
//         private final EditorProject mProject
//         = sEditor.getSelectedProjectEditor().getEditorProject();
//
//         // Set some variable on the current project
//         @Override
//         public void run() {
//         while (!mDone) {
//         // Wait until user aborts execution
//         System.err.println("Enter Command ...");
//         try {
//         final String in = mReader.readLine();
//         // Wait until user aborts execution
//         System.err.println("Your Command Is '" + in + "'");
//         if (in != null) {
//         if (in.equals("play")) {
//         // Maybe we first start the execution now ?
//         SwingUtilities.invokeLater(new Runnable() {
//
//         @Override
//         public void run() {
//         sEditor.play();
//         }
//         });
//         } else if (in.equals("stop")) {
//         // And then we stop the execution later on?
//         SwingUtilities.invokeLater(new Runnable() {
//
//         @Override
//         public void run() {
//         sEditor.stop();
//         }
//         });
//         } else if (in.equals("exit")) {
//         mDone = true;
//         } else {
//         if (sRunTime.hasVariable(mProject, "in")) {
//         // Fancy programmatic variable setting
//         sRunTime.setVariable(mProject, "in", in);
//         }
//         }
//         }
//         } catch (final IOException exc) {
//         // Do nothing
//         }
//         }
//         // Print some information
//         System.err.println("Stopping Editor Mode ...");
//         // And then cleanly exit the editor
//         SwingUtilities.invokeLater(new Runnable() {
//
//         @Override
//         public void run() {
//         // Close all projects
//         sEditor.closeAll();
//         // Dispose the editor
//         sEditor.dispose();
//         }
//         });
//         }
//         };
//         // Start the control thread
//         control.start();
//         // Print some information
//         System.err.println("Starting Editor Mode ...");

    }

    // Start the runtime with some project
    private static void runtime(final File file) {
        // Get an editor project from file 
        final RunTimeProject data = new RunTimeProject(file);
        // Get the singelton runtime instance
        final RunTimeInstance sRunTime = RunTimeInstance.getInstance();
        //
        if (sRunTime.load(data)) {
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
