package de.dfki.vsm;

import de.dfki.vsm.runtime.CoreRuntime;
import de.dfki.vsm.runtime.project.RunTimeProject;

import java.io.File;
import java.io.IOException;

public class Core {
    // Start the runtime with some project
    public static void runtime(final File file) {
        final CoreRuntime runtime = new CoreRuntime(new RunTimeProject(file));
        if (runtime.launch()) {
            // Start the runtime with the project
            if (runtime.start()) {
                // Wait until user aborts execution
                System.err.println("Press Key To Abort ...");
                // TODO: Stop waiting if execution
                // has been aborted in another way
                try {
                    final int in = System.in.read();
                    if (in != -1) {
                        // Aborting the execution now
                    } else { // For the case there is no access to System.in
                        runtime.waitTillFinished();
                    }

                } catch (final IOException | InterruptedException exc) {
                    // Do nothing
                } finally {
                    runtime.shutdown();
                }

            }
        }
    }
    public static void main(String[] args){
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
                    Core.runtime(file);
                }
            }
        }
    }
}
