package de.dfki.vsm.runtime;

import de.dfki.vsm.model.project.ProjectData;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.runtime.player.Player;
import de.dfki.vsm.runtime.value.AbstractValue;
import de.dfki.vsm.runtime.value.BooleanValue;
import de.dfki.vsm.runtime.value.FloatValue;
import de.dfki.vsm.runtime.value.IntValue;
import de.dfki.vsm.runtime.value.StringValue;
import java.util.HashMap;

/**
 * @author Gregor Mehlmann
 */
public final class RunTime {

    // The Singelton RunTime Instance
    private static RunTime sInstance = null;

    // The Map Of Maintained Projects
    private final HashMap<SceneFlow, Interpreter> mProjectMap;

    ////////////////////////////////////////////////////////////////////////////
    private RunTime() {
        // Initialize The Project Map
        mProjectMap = new HashMap<>();
    }

    ////////////////////////////////////////////////////////////////////////////
    public static synchronized RunTime getInstance() {
        if (sInstance == null) {
            sInstance = new RunTime();
        }
        // Return The Singelton Instance
        return sInstance;
    }

    ////////////////////////////////////////////////////////////////////////////
    public final synchronized void launch(final ProjectData project) {

        // TODO: clean
        // Load the sceneplayer
        project.launchScenePlayer();
        // TODO: clean
        // Load the dialogue act player
        //project.loadDialogPlayer();

        // Launch The Player List
        project.launchPlayerList();

        // Launch The Plugin List
        project.launchPluginList();

        // Get The Project Data
        final SceneFlow sceneflow = project.getSceneFlow();
        final Player sceneplayer = project.getScenePlayer();
        final Player dialogplayer = project.getDialogPlayer();
        // Construct the Interpreter
        Interpreter interpreter = new Interpreter(sceneflow, sceneplayer, dialogplayer);

        // Register The Project
        if (!mProjectMap.containsKey(sceneflow)) {
            mProjectMap.put(sceneflow, interpreter);
        }
    }

    public final synchronized void unload(final ProjectData project) {
        SceneFlow sceneFlow = project.getSceneFlow();

        if (mProjectMap.containsKey(sceneFlow)) {
            mProjectMap.remove(sceneFlow);

            // Unload the sceneplayer
            project.unloadScenePlayer();

            // Unload the plugin list
            project.unloadPluginList();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    public synchronized void start(final SceneFlow sceneflow) {
        if (mProjectMap.containsKey(sceneflow)) {
            mProjectMap.get(sceneflow).start();
        }
    }

    public synchronized void abort(final SceneFlow sceneflow) {
        if (mProjectMap.containsKey(sceneflow)) {
            mProjectMap.get(sceneflow).stop();
        }
    }

    public synchronized void pause(final SceneFlow sceneflow) {
        if (mProjectMap.containsKey(sceneflow)) {
            mProjectMap.get(sceneflow).pause();
        }
    }

    public synchronized boolean isActive(final SceneFlow sceneflow) {
        if (mProjectMap.containsKey(sceneflow)) {
            return mProjectMap.get(sceneflow).isRunning();
        }
        return false;
    }

    public synchronized boolean isPaused(final SceneFlow sceneflow) {
        if (mProjectMap.containsKey(sceneflow)) {
            return mProjectMap.get(sceneflow).isPaused();
        }
        return false;
    }

    public synchronized void proceed(final SceneFlow sceneflow) {
        if (mProjectMap.containsKey(sceneflow)) {
            mProjectMap.get(sceneflow).proceed();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    //
    ////////////////////////////////////////////////////////////////////////////
    public synchronized boolean execute(SceneFlow sceneFlow, String nodeId, Command cmd) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).execute(nodeId, cmd);
            }
        }

        return false;
    }

    public synchronized AbstractValue evaluate(SceneFlow sceneFlow, String nodeId, Expression exp) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).evaluate(nodeId, exp);
            }
        }

        return null;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String nodeId, String varName, Expression exp) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(nodeId, varName, exp);
            }
        }

        return false;
    }

    ////////////////////////////////////////////////////////////////////////////
    //
    ////////////////////////////////////////////////////////////////////////////
    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, int value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, new IntValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, int index, int value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, index, new IntValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, String member, int value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, member, new IntValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, float value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, new FloatValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, int index, float value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, index, new FloatValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, String member, float value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, member, new FloatValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, boolean value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, new BooleanValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, int index, boolean value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, index, new BooleanValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, String member, boolean value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, member, new BooleanValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, String value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, new StringValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, int index, String value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, index, new StringValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, String member, String value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, member, new StringValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, AbstractValue value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, value);
            }
        }

        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, int index, AbstractValue value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, index, value);
            }
        }

        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, String member, AbstractValue value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setVariable(varName, member, value);
            }
        }

        return false;
    }

//  public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, Object[] value) {
//      if (sceneFlow != null) {
//          if (mSceneFlowMap.containsKey(sceneFlow)) {
//              return mSceneFlowMap.get(sceneFlow).setVariable(varName, value);
//          }
//
//      }
//      return false;
//  }
    public synchronized boolean setLocalVariable(SceneFlow sceneFlow, String nodeId, String varName, int value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setLocalVariable(nodeId, varName, new IntValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setLocalVariable(SceneFlow sceneFlow, String nodeId, String varName, String value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setLocalVariable(nodeId, varName, new StringValue(value));
            }
        }

        return false;
    }

    public synchronized boolean setLocalVariable(SceneFlow sceneFlow, String nodeId, String varName, boolean value) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).setLocalVariable(nodeId, varName, new BooleanValue(value));
            }
        }

        return false;
    }

    public synchronized boolean hasLocaleVariable(SceneFlow sceneFlow, String nodeId, String varName) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).hasLocalVariable(nodeId, varName);
            }
        }

        return false;
    }

    public synchronized boolean hasVariable(SceneFlow sceneFlow, String varName) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).hasVariable(varName);
            }
        }

        return false;
    }

    public synchronized boolean hasVariable(SceneFlow sceneFlow, String varName, int index) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).hasVariable(varName, index);
            }
        }

        return false;
    }

    public synchronized boolean hasVariable(SceneFlow sceneFlow, String varName, String member) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).hasVariable(varName, member);
            }
        }

        return false;
    }

    public synchronized AbstractValue getValueOf(SceneFlow sceneFlow, String varName) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).getValueOf(varName);
            }
        }

        return null;
    }

    public synchronized AbstractValue getValueOf(SceneFlow sceneFlow, String varName, int index) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).getValueOf(varName, index);
            }
        }

        return null;
    }

    public synchronized AbstractValue getValueOf(SceneFlow sceneFlow, String varName, String member) {
        if (sceneFlow != null) {
            if (mProjectMap.containsKey(sceneFlow)) {
                return mProjectMap.get(sceneFlow).getValueOf(varName, member);
            }
        }

        return null;
    }
}
