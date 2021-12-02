package de.dfki.vsm;

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.util.log.LOGDefaultLogger;

import javax.swing.*;
import java.io.File;
import java.net.SocketException;
import java.net.UnknownHostException;


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
        SwingUtilities.invokeLater(() -> {
            // Check if we have at least one argument
            if (args.length > 0) {
                if (args.length == 2) {
                    // Read the first command line argument
                    final String mode = args[0];
                    // Get the project file name argument
                    final String name = args[1];
                    // Create the project configuration
                    final File file = new File(name);
                    // Check the options from this argument
                    if (file.exists()) {
                        if (mode.equals("runtime")) {
                            try {
                                Core.runtime(file);
                            } catch (SocketException e) {
                                e.printStackTrace();
                            } catch (UnknownHostException e) {
                                e.printStackTrace();
                            }
                        } else if (mode.equalsIgnoreCase("editor")) {
                            editor(file);
                        } else {
                            usage();
                        }
                    } else {
                        error(file);
                    }
                }
            } else {
                editor();
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
        //final RunTimeInstance sRunTime = RunTimeInstance.getInstance();
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

    // Print usage when usage error happened
    private static void usage() {
        sLogger.failure("Error: Usage: [runtime|editor] [filename]");
    }

    // Print error when a file error happened
    private static void error(final File file) {
        sLogger.failure("Error: Cannot find file '" + file.getAbsolutePath() + "'");
    }
}
