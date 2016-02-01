package de.dfki.vsm.runtime;

import de.dfki.vsm.runtime.interpreter.Interpreter;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.runtime.values.AbstractValue;
import de.dfki.vsm.runtime.values.BooleanValue;
import de.dfki.vsm.runtime.values.FloatValue;
import de.dfki.vsm.runtime.values.IntValue;
import de.dfki.vsm.runtime.values.StringValue;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.util.HashMap;
import java.util.concurrent.locks.ReentrantLock;

/**
 * @author Gregor Mehlmann
 */
public final class RunTimeInstance {

    // The singelton runtime instance
    private static RunTimeInstance sInstance = null;

    // The singelton logger instance
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    // The map of maintainted projects
    private final HashMap<RunTimeProject, Interpreter> mProjectMap;

    // Construct the runtime instance
    private RunTimeInstance() {
        // Initialize the map of projects
        mProjectMap = new HashMap<>();
    }

    // Initialize the runtime instance
    public static /* synchronized */ RunTimeInstance getInstance() {
        if (sInstance == null) {
            sInstance = new RunTimeInstance();
        }
        // Return the singelton instance
        return sInstance;
    }

    public ReentrantLock getLock(final RunTimeProject project) {
        if (!mProjectMap.containsKey(project)) {
            // Print an error message
            mLogger.failure("Failure: There is no interpreter registered for project '" + project + "'");
            // Return false at error
            return null;
        }
        return mProjectMap.get(project).getLock();
    }

    public final /* synchronized */ boolean load(final RunTimeProject project) {

        // Check if the project is already registered
        if (mProjectMap.containsKey(project)) {
            // Print an error message
            mLogger.failure("Failure: Cannot register a new interpreter for project '" + project + "'");
            // Return false at error
            return false;
        }
        // Try to load all runtime objects
        if (!project.load()) {
            // Print an error message
            mLogger.failure("Failure: Cannot load runtime objects of project '" + project + "'");
            // Return false at error
            return false;
        } else {
            // Print some information 
            //mLogger.message("Loaded runtime objects of project '" + project + "'");
        }
        // Return true at success
        return true;
    }

    // Launch a runtime project
    public final /* synchronized */ boolean launch(final RunTimeProject project) {

        // Check if the project is already registered
        if (mProjectMap.containsKey(project)) {
            // Print an error message
            mLogger.failure("Failure: Cannot register a new interpreter for project '" + project + "'");
            // Return false at error
            return false;
        }
        // Try to load all runtime objects
//        if (!project.load()) {
//            // Print an error message
//            mLogger.failure("Failure: Cannot load runtime objects of project '" + project + "'");
//            // Return false at error
//            return false;
//        } else {
//            // Print some information 
//            //mLogger.message("Loaded runtime objects of project '" + project + "'");
//        }
        // Try to launch all runtime objects
        if (!project.launch()) {
            // Print an error message
            mLogger.failure("Failure: Cannot launch runtime objects of project '" + project + "'");
            // Return false at error
            return false;
        } else {
            // Print some information 
            // mLogger.message("Launched runtime objects of project '" + project + "'");
        }
        // Create a new runtime interpreter
        final Interpreter interpreter = new Interpreter(project);
        // Register project with interpreter        
        mProjectMap.put(project, interpreter);
        // Print some information 
        // mLogger.message("Registered the new interpreter '" + interpreter + "' for project '" + project + "'");
        // Return true at success
        return true;
    }

    // Unload a runtime project
    public final /* synchronized */ boolean unload(final RunTimeProject project) {
        // Check if the project is already registered
        if (!mProjectMap.containsKey(project)) {
            // Print an error message
            mLogger.failure("Failure: There is no interpreter registered for project '" + project + "'");
            // Return false at error
            return false;
        }
        // Try to unload all runtime objects
        if (!project.unload()) {
            // Print an error message
            mLogger.failure("Failure: Cannot unload runtime objects of project '" + project + "'");
            // Return false at error
            return false;
        } else {
            // Print some information 
            //mLogger.message("Unloaded runtime objects of project '" + project + "'");
        }

        // Remove project and interpreter
        final Interpreter interpreter = mProjectMap.remove(project);
        // Print some information 
        //mLogger.message("Deregistered the interpreter '" + interpreter + "' with project '" + project + "'");
        // Return true at success
        return true;
    }

    // Start the execution of the project
    public final /* synchronized */ boolean start(final RunTimeProject project) {
        if (!mProjectMap.containsKey(project)) {
            // Print an error message
            mLogger.failure("Failure: There is no interpreter registered for project '" + project + "'");
            // Return false at error
            return false;
        }
        // Start the project with the interpreter
        return mProjectMap.get(project).start();
    }

    // Abort the execution of the project
    public final /* synchronized */ boolean abort(final RunTimeProject project) {
        if (!mProjectMap.containsKey(project)) {
            // Print an error message
            mLogger.failure("Failure: There is no interpreter registered for project '" + project + "'");
            // Return false at error
            return false;
        }
        // Abort the project with the interpreter
        return mProjectMap.get(project).abort();
    }

    // Pause the execution of the project
    public final /* synchronized */ boolean pause(final RunTimeProject project) {
        if (!mProjectMap.containsKey(project)) {
            // Print an error message
            mLogger.failure("Failure: There is no interpreter registered for project '" + project + "'");
            // Return false at error
            return false;
        }
        // Pause the project with the interpreter
        return mProjectMap.get(project).pause();
    }

    // Proceed the execution of the project
    public final /* synchronized */ boolean proceed(final RunTimeProject project) {
        if (!mProjectMap.containsKey(project)) {
            // Print an error message
            mLogger.failure("Failure: There is no interpreter registered for project '" + project + "'");
            // Return false at error
            return false;
        }
        // Proceed the project with the interpreter
        return mProjectMap.get(project).proceed();
    }

    // Check activity status of the project
    public final /* synchronized */ boolean isRunning(final RunTimeProject project) {
        //mLogger.message("Check if running");
        if (!mProjectMap.containsKey(project)) {
            // Print an error message
            //mLogger.warning("Warning: There is no interpreter registered for project '" + project + "'");
            // Return false at error
            return false;
        }
        //mLogger.message("Check really running '" + this + "'");
        // Check activity status with the interpreter
        return mProjectMap.get(project).isRunning();
    }

    // Check activity status of the project
    public final /* synchronized */ boolean wasExecuted(final RunTimeProject project) {
        //mLogger.message("Check if running");
        if (!mProjectMap.containsKey(project)) {
            // Print an error message
            //mLogger.warning("Warning: There is no interpreter registered for project '" + project + "'");
            // Return false at error
            return false;
        }
        //mLogger.message("Check really running '" + this + "'");
        // Check activity status with the interpreter
        return mProjectMap.get(project).wasExecuted();
    }

    // Check paused status of the project
    public final /* synchronized */ boolean isPaused(final RunTimeProject project) {
        if (!mProjectMap.containsKey(project)) {
            // Print an error message
            mLogger.warning("Warning: There is no interpreter registered for project '" + project + "'");
            // Return false at error
            return false;
        }
        // Check paused status with the interpreter
        return mProjectMap.get(project).isPaused();
    }

    public final /* synchronized */ boolean setVariable(final RunTimeProject project, final String name, final int value) {
        if (project != null) {
            if (mProjectMap.containsKey(project)) {
                return mProjectMap.get(project).setVariable(name, new IntValue(value));
            }
        }

        return false;
    }

    public final /* synchronized */ boolean setVariable(final RunTimeProject project, final String name, final int index, final int value) {
        if (project != null) {
            if (mProjectMap.containsKey(project)) {
                return mProjectMap.get(project).setVariable(name, index, new IntValue(value));
            }
        }

        return false;
    }

    public final /* synchronized */ boolean setVariable(final RunTimeProject project, final String name, final String member, final int value) {
        if (project != null) {
            if (mProjectMap.containsKey(project)) {
                return mProjectMap.get(project).setVariable(name, member, new IntValue(value));
            }
        }

        return false;
    }

    public final /* synchronized */ boolean setVariable(final RunTimeProject project, final String name, float value) {
        if (project != null) {
            if (mProjectMap.containsKey(project)) {
                return mProjectMap.get(project).setVariable(name, new FloatValue(value));
            }
        }

        return false;
    }

    public final /* synchronized */ boolean setVariable(final RunTimeProject project, final String name, final int index, float value) {
        if (project != null) {
            if (mProjectMap.containsKey(project)) {
                return mProjectMap.get(project).setVariable(name, index, new FloatValue(value));
            }
        }

        return false;
    }

    public final /* synchronized */ boolean setVariable(final RunTimeProject project, final String name, final String member, float value) {
        if (project != null) {
            if (mProjectMap.containsKey(project)) {
                return mProjectMap.get(project).setVariable(name, member, new FloatValue(value));
            }
        }

        return false;
    }

    public final /* synchronized */ boolean setVariable(final RunTimeProject project, final String name, boolean value) {
        if (project != null) {
            if (mProjectMap.containsKey(project)) {
                return mProjectMap.get(project).setVariable(name, new BooleanValue(value));
            }
        }

        return false;
    }

    public final /* synchronized */ boolean setVariable(final RunTimeProject project, final String name, final int index, boolean value) {
        if (project != null) {
            if (mProjectMap.containsKey(project)) {
                return mProjectMap.get(project).setVariable(name, index, new BooleanValue(value));
            }
        }

        return false;
    }

    public final /* synchronized */ boolean setVariable(final RunTimeProject project, final String name, final String member, boolean value) {
        if (project != null) {
            if (mProjectMap.containsKey(project)) {
                return mProjectMap.get(project).setVariable(name, member, new BooleanValue(value));
            }
        }

        return false;
    }

    public final /* synchronized */ boolean setVariable(final RunTimeProject project, final String name, final String value) {
        if (project != null) {
            if (mProjectMap.containsKey(project)) {
                return mProjectMap.get(project).setVariable(name, new StringValue(value));
            }
        }

        return false;
    }

    public final /* synchronized */ boolean setVariable(final RunTimeProject project, final String name, final int index, final String value) {
        if (project != null) {
            if (mProjectMap.containsKey(project)) {
                return mProjectMap.get(project).setVariable(name, index, new StringValue(value));
            }
        }

        return false;
    }

    public final /* synchronized */ boolean setVariable(final RunTimeProject project, final String name, final String member, final String value) {
        if (project != null) {
            if (mProjectMap.containsKey(project)) {
                return mProjectMap.get(project).setVariable(name, member, new StringValue(value));
            }
        }

        return false;
    }

    public final /* synchronized */ boolean setVariable(final RunTimeProject project, final String name, final AbstractValue value) {
        if (project != null) {
            if (mProjectMap.containsKey(project)) {
                return mProjectMap.get(project).setVariable(name, value);
            }
        }

        return false;
    }

    public final /* synchronized */ boolean setVariable(final RunTimeProject project, final String name, final int index, final AbstractValue value) {
        if (project != null) {
            if (mProjectMap.containsKey(project)) {
                return mProjectMap.get(project).setVariable(name, index, value);
            }
        }

        return false;
    }

    public final /* synchronized */ boolean setVariable(final RunTimeProject project, final String name, final String member, final AbstractValue value) {
        if (project != null) {
            if (mProjectMap.containsKey(project)) {
                return mProjectMap.get(project).setVariable(name, member, value);
            }
        }

        return false;
    }

    public final /* synchronized */ boolean hasVariable(final RunTimeProject project, final String name) {
        if (project != null) {
            if (mProjectMap.containsKey(project)) {
                return mProjectMap.get(project).hasVariable(name);
            }
        }

        return false;
    }

    public final /* synchronized */ boolean hasVariable(final RunTimeProject project, final String name, final int index) {
        if (project != null) {
            if (mProjectMap.containsKey(project)) {
                return mProjectMap.get(project).hasVariable(name, index);
            }
        }

        return false;
    }

    public final /* synchronized */ boolean hasVariable(final RunTimeProject project, final String name, final String member) {
        if (project != null) {
            if (mProjectMap.containsKey(project)) {
                return mProjectMap.get(project).hasVariable(name, member);
            }
        }

        return false;
    }

    public final /* synchronized */ AbstractValue getValueOf(final RunTimeProject project, final String name) {
        if (project != null) {
            if (mProjectMap.containsKey(project)) {
                return mProjectMap.get(project).getValueOf(name);
            }
        }
        return null;
    }

    public final /* synchronized */ AbstractValue getValueOf(final RunTimeProject project, final String name, final int index) {
        if (project != null) {
            if (mProjectMap.containsKey(project)) {
                return mProjectMap.get(project).getValueOf(name, index);
            }
        }

        return null;
    }

    public final /* synchronized */ AbstractValue getValueOf(final RunTimeProject project, final String name, final String member) {
        if (project != null) {
            if (mProjectMap.containsKey(project)) {
                return mProjectMap.get(project).getValueOf(name, member);
            }
        }

        return null;
    }
}
