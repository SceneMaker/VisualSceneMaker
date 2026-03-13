package de.dfki.vsm.runtime;

import de.dfki.vsm.event.EventListener;
import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.event.event.ForceShutdownEvent;
import de.dfki.vsm.runtime.project.RunTimeProject;

import java.io.File;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Platform-neutral runtime facade. Platform-specific launchers should compose this type
 * instead of calling desktop-style entry points directly.
 */
public class CoreRuntime {

    private final RunTimeProject mRunTimeProject;

    public CoreRuntime(final File projectFile) {
        this(new RunTimeProject(Objects.requireNonNull(projectFile, "projectFile")));
    }

    public CoreRuntime(final RunTimeProject runTimeProject) {
        mRunTimeProject = Objects.requireNonNull(runTimeProject, "runTimeProject");
    }

    public RunTimeProject getRunTimeProject() {
        return mRunTimeProject;
    }

    public boolean launch() {
        return mRunTimeProject.launch();
    }

    public boolean start() {
        return mRunTimeProject.start();
    }

    public boolean isRunning() {
        return mRunTimeProject.isRunning();
    }

    public void abort() {
        mRunTimeProject.abort();
    }

    public void unload() {
        mRunTimeProject.unload();
    }

    public void shutdown() {
        abort();
        unload();
    }

    public void waitTillFinished() throws InterruptedException {
        final ProjectTerminationWaiter waiter = new ProjectTerminationWaiter(mRunTimeProject);
        waiter.waitTillFinished();
    }

    private static final class ProjectTerminationWaiter implements EventListener {
        private final RunTimeProject mRunTimeProject;
        private final AtomicBoolean mIsRunning = new AtomicBoolean(true);

        private ProjectTerminationWaiter(final RunTimeProject runTimeProject) {
            runTimeProject.getEventDispatcher().register(this);
            mRunTimeProject = runTimeProject;
        }

        @Override
        public synchronized void update(final EventObject event) {
            if (event instanceof ForceShutdownEvent) {
                mIsRunning.set(false);
            }
        }

        private void waitTillFinished() throws InterruptedException {
            while (mIsRunning.get()) {
                Thread.sleep(200);
                mIsRunning.compareAndSet(true, mRunTimeProject.isRunning());
            }
        }
    }
}
