package de.dfki.vsm;


import de.dfki.vsm.event.EventDispatcher;
import de.dfki.vsm.event.EventListener;
import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.event.event.ForceShutdownEvent;
import de.dfki.vsm.runtime.project.RunTimeProject;

import java.io.File;
import java.io.IOException;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.concurrent.atomic.AtomicBoolean;

public class Core {
    // Start the runtime with some project
    public static void runtime(final File file) throws SocketException, UnknownHostException {
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
                    } else { // For the case there is no access to System.in
                        ProjectTerminationWaiter waiter = new ProjectTerminationWaiter(runTimeProject);
                        waiter.waitTillFinished();
                    }

                } catch (final IOException | InterruptedException exc) {
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


    private static class ProjectTerminationWaiter implements EventListener {
        private final RunTimeProject runTimeProject;
        private final AtomicBoolean isRunning = new AtomicBoolean(true);

        private ProjectTerminationWaiter(RunTimeProject runTimeProject){
            EventDispatcher.getInstance().register(this);
            this.runTimeProject = runTimeProject;
        }
        @Override
        public synchronized void update(EventObject event) {
            if(event instanceof ForceShutdownEvent) {
                isRunning.set(false);
            }
        }

        public void waitTillFinished() throws InterruptedException {
            while (isRunning.get()) {
                Thread.sleep(200);
                isRunning.compareAndSet(true, runTimeProject.isRunning());
            }
        }
    }
    public static void main(String[] args) throws SocketException, UnknownHostException {
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
