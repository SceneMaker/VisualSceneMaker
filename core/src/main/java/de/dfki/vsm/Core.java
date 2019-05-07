package de.dfki.vsm;

import de.dfki.vsm.runtime.project.RunTimeProject;

import java.io.File;
import java.io.IOException;

public class Core {
    // Start the runtime with some project
    public static void runtime(final File file) {
        // Get an editor project from file
        final RunTimeProject runTimeProject = new RunTimeProject(file);
        // Get the singleton runtime instance
        //final RunTimeInstance sRunTime = RunTimeInstance.getInstance();
        // Launch the runtime with the project
        if (/*sRunTime.launch(runTimeProject)*/runTimeProject.launch()) {
            // Start the runtime with the project
            if (/*sRunTime.start(runTimeProject)*/runTimeProject.start()) {
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
                    //sRunTime.abort(runTimeProject);
                    runTimeProject.abort();
                    // Unload the project from the runtime
                    //sRunTime.unload(runTimeProject);
                    runTimeProject.unload();
                }

            }
        }
    }
}
