package de.dfki.vsm.runtime;

import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.runtime.player.Player;
import de.dfki.vsm.runtime.value.AbstractValue;
import de.dfki.vsm.runtime.value.BooleanValue;
import de.dfki.vsm.runtime.value.FloatValue;
import de.dfki.vsm.runtime.value.IntValue;
import de.dfki.vsm.runtime.value.StringValue;
import java.util.HashMap;

/**
 * @author Not me
 */
public final class RunTimeInstance {

    // The singelton runtime instance
    private static RunTimeInstance sInstance = null;

    // The map of maintainted projects
    private final HashMap<RunTimeProject, Interpreter> mProjectMap;

    // Construct the runtime instance
    private RunTimeInstance() {
        // Initialize the map of projects
        mProjectMap = new HashMap<>();
    }

    // Initialize the runtime instance
    public static synchronized RunTimeInstance getInstance() {
        if (sInstance == null) {
            sInstance = new RunTimeInstance();
        }
        // Return the singelton instance
        return sInstance;
    }

    // Launch a runtime project
    public final synchronized void launch(final RunTimeProject project) {
        final Player player = project.getDefaultScenePlayer();
        final Player dialog = project.getDefaultDialogPlayer();

        // Launch the player
        player.launch();
        //dialog.launch();

        // Launch The Player List
        project.launchPlayerList();

        // Launch The Plugin List
        project.launchPluginList();

        // Construct the interpreter
        final Interpreter interpreter = new Interpreter(project);

        // Register project and interpreter
        if (!mProjectMap.containsKey(project)) {
            mProjectMap.put(project, interpreter);
        }
    }

    public final synchronized void unload(RunTimeProject project) {
        //SceneFlow sceneFlow = project.getSceneFlow();

        if (mProjectMap.containsKey(project)) {
            mProjectMap.remove(project);

            // Unload the sceneplayer
            project.unloadScenePlayer();

            // Unload the plugin list
            project.unloadPluginList();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void start(final RunTimeProject sceneflow) {
        if (mProjectMap.containsKey(sceneflow)) {
            mProjectMap.get(sceneflow).start();
        }
    }

    public synchronized void abort(final RunTimeProject sceneflow) {
        if (mProjectMap.containsKey(sceneflow)) {
            mProjectMap.get(sceneflow).stop();
        }
    }

    public synchronized void pause(final RunTimeProject sceneflow) {
        if (mProjectMap.containsKey(sceneflow)) {
            mProjectMap.get(sceneflow).pause();
        }
    }

    public synchronized boolean isActive(final RunTimeProject sceneflow) {
        if (mProjectMap.containsKey(sceneflow)) {
            return mProjectMap.get(sceneflow).isRunning();
        }
        return false;
    }

    public synchronized boolean isPaused(final RunTimeProject sceneflow) {
        if (mProjectMap.containsKey(sceneflow)) {
            return mProjectMap.get(sceneflow).isPaused();
        }
        return false;
    }

    public synchronized void proceed(final RunTimeProject sceneflow) {
        if (mProjectMap.containsKey(sceneflow)) {
            mProjectMap.get(sceneflow).proceed();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    //
    ////////////////////////////////////////////////////////////////////////////
    public final synchronized boolean setVariable(final RunTimeProject sceneFlow, String varName, int value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, new IntValue(value));
            }
        }

        return false;
    }

    public final synchronized boolean setVariable(final RunTimeProject sceneFlow, String varName, int index, int value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, index, new IntValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(final RunTimeProject sceneFlow, String varName, String member, int value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, member, new IntValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(final RunTimeProject sceneFlow, String varName, float value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, new FloatValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(final RunTimeProject sceneFlow, String varName, int index, float value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, index, new FloatValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(final RunTimeProject sceneFlow, String varName, String member, float value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, member, new FloatValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(final RunTimeProject sceneFlow, String varName, boolean value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, new BooleanValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(final RunTimeProject sceneFlow, String varName, int index, boolean value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, index, new BooleanValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(final RunTimeProject sceneFlow, String varName, String member, boolean value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, member, new BooleanValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(final RunTimeProject sceneFlow, String varName, String value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, new StringValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(final RunTimeProject sceneFlow, String varName, int index, String value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, index, new StringValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(final RunTimeProject sceneFlow, String varName, String member, String value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, member, new StringValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(final RunTimeProject sceneFlow, String varName, AbstractValue value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, value);
            }
        }

        return false;
    }

    public synchronized boolean setVariable(final RunTimeProject sceneFlow, String varName, int index, AbstractValue value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, index, value);
            }
        }

        return false;
    }

    public synchronized boolean setVariable(final RunTimeProject sceneFlow, String varName, String member, AbstractValue value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, member, value);
            }
        }

        return false;
    }

    
    
    
    
    
    
    public synchronized boolean hasVariable(final RunTimeProject sceneFlow, String varName) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).hasVariable(varName);
            }
        }

        return false;
    }

    public synchronized boolean hasVariable(final RunTimeProject sceneFlow, String varName, int index) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).hasVariable(varName, index);
            }
        }

        return false;
    }

    public synchronized boolean hasVariable(final RunTimeProject sceneFlow, String varName, String member) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).hasVariable(varName, member);
            }
        }

        return false;
    }

    public synchronized AbstractValue getValueOf(final RunTimeProject sceneFlow, String varName) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).getValueOf(varName);
            }
        }

        return null;
    }

    public synchronized AbstractValue getValueOf(final RunTimeProject sceneFlow, String varName, int index) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).getValueOf(varName, index);
            }
        }

        return null;
    }

    public synchronized AbstractValue getValueOf(final RunTimeProject sceneFlow, String varName, String member) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).getValueOf(varName, member);
            }
        }

        return null;
    }
}
