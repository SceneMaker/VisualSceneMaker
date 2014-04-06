package de.dfki.vsm.runtime;

import de.dfki.vsm.model.project.ProjectData;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.runtime.player.ScenePlayer;
import de.dfki.vsm.runtime.value.BooleanValue;
import de.dfki.vsm.runtime.value.FloatValue;
import de.dfki.vsm.runtime.value.IntValue;
import de.dfki.vsm.runtime.value.StringValue;
import de.dfki.vsm.runtime.value.AbstractValue;
import java.util.HashMap;

/**
 * @author Gregor Mehlmann
 */
public class RunTime {

    private static RunTime sInstance = null;
    private HashMap<SceneFlow, Interpreter> mSceneFlowMap
            = new HashMap<SceneFlow, Interpreter>();

    private RunTime() {
    }

    public static synchronized RunTime getInstance() {
        if (sInstance == null) {
            sInstance = new RunTime();
        }
        return sInstance;
    }

    ////////////////////////////////////////////////////////////////////////////
    // REGISTER & UNREGISTER
    ////////////////////////////////////////////////////////////////////////////
    public synchronized void register(ProjectData project) {
        // Load the sceneplayer
        project.loadScenePlayer();
        // Load the service list
        project.loadServiceList();
        // Load the request list
        project.loadRequestList();
        // Load the plugin list
        project.loadPluginList();
        // Get the sceneflow and the scenepayer of that
        // project and Create a new interpreter for them
        SceneFlow sceneFlow = project.getSceneFlow();
        ScenePlayer scenePlayer = project.getScenePlayer();
        Interpreter interpreter = new Interpreter(sceneFlow, scenePlayer);
        // If this sceneflow is not yet registered then register it
        if (!mSceneFlowMap.containsKey(sceneFlow)) {
            mSceneFlowMap.put(sceneFlow, interpreter);
        }
    }

    public synchronized void unregister(ProjectData project) {
        SceneFlow sceneFlow = project.getSceneFlow();
        if (mSceneFlowMap.containsKey(sceneFlow)) {
            mSceneFlowMap.remove(sceneFlow);
            // Unload the sceneplayer
            project.unloadScenePlayer();
            // Unload the service list
            project.unloadServiceList();
            // Unload the request list
            project.unloadRequestList();
            // Unload the plugin list
            project.unloadPluginList();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    // START / STOP / PAUSE / PROCEED
    ////////////////////////////////////////////////////////////////////////////
    public synchronized void startSceneFlow(SceneFlow sceneflow) {
        if (mSceneFlowMap.containsKey(sceneflow)) {
            mSceneFlowMap.get(sceneflow).start();
        }
    }

    public synchronized void stopSceneFlow(SceneFlow sceneflow) {
        if (mSceneFlowMap.containsKey(sceneflow)) {
            mSceneFlowMap.get(sceneflow).stop();
        }
    }

    public synchronized boolean isSceneFlowRunnning(SceneFlow sceneflow) {
        if (mSceneFlowMap.containsKey(sceneflow)) {
            return mSceneFlowMap.get(sceneflow).isRunning();
        }
        return false;
    }

    public synchronized void pauseSceneFlow(SceneFlow sceneflow) {
        if (mSceneFlowMap.containsKey(sceneflow)) {
            mSceneFlowMap.get(sceneflow).pause();
        }
    }

    public synchronized boolean isSceneFlowPaused(SceneFlow sceneflow) {
        if (mSceneFlowMap.containsKey(sceneflow)) {
            return mSceneFlowMap.get(sceneflow).isPaused();
        }
        return false;
    }

    public synchronized void proceedSceneFlow(SceneFlow sceneflow) {
        if (mSceneFlowMap.containsKey(sceneflow)) {
            mSceneFlowMap.get(sceneflow).proceed();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    //
    ////////////////////////////////////////////////////////////////////////////
    public synchronized boolean execute(SceneFlow sceneFlow, String nodeId, Command cmd) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).execute(nodeId, cmd);
            }

        }
        return false;
    }

    public synchronized AbstractValue evaluate(SceneFlow sceneFlow, String nodeId, Expression exp) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).evaluate(nodeId, exp);
            }

        }
        return null;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String nodeId, String varName, Expression exp) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).setVariable(nodeId, varName, exp);
            }
        }
        return false;
    }

    ////////////////////////////////////////////////////////////////////////////
    //
    ////////////////////////////////////////////////////////////////////////////
    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, int value) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).setVariable(varName, new IntValue(value));
            }
        }
        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, int index, int value) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).setVariable(varName, index, new IntValue(value));
            }
        }
        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, String member, int value) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).setVariable(varName, member, new IntValue(value));
            }
        }
        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, float value) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).setVariable(varName, new FloatValue(value));
            }
        }
        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, int index, float value) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).setVariable(varName, index, new FloatValue(value));
            }
        }
        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, String member, float value) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).setVariable(varName, member, new FloatValue(value));
            }
        }
        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, boolean value) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).setVariable(varName, new BooleanValue(value));
            }
        }
        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, int index, boolean value) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).setVariable(varName, index, new BooleanValue(value));
            }
        }
        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, String member, boolean value) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).setVariable(varName, member, new BooleanValue(value));
            }
        }
        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, String value) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).setVariable(varName, new StringValue(value));
            }
        }
        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, int index, String value) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).setVariable(varName, index, new StringValue(value));
            }
        }
        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, String member, String value) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).setVariable(varName, member, new StringValue(value));
            }
        }
        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, AbstractValue value) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).setVariable(varName, value);
            }
        }
        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, int index, AbstractValue value) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).setVariable(varName, index, value);
            }
        }
        return false;
    }

    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, String member, AbstractValue value) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).setVariable(varName, member, value);
            }
        }
        return false;
    }

//    public synchronized boolean setVariable(SceneFlow sceneFlow, String varName, Object[] value) {
//        if (sceneFlow != null) {
//            if (mSceneFlowMap.containsKey(sceneFlow)) {
//                return mSceneFlowMap.get(sceneFlow).setVariable(varName, value);
//            }
//
//        }
//        return false;
//    }
    public synchronized boolean setLocalVariable(SceneFlow sceneFlow, String nodeId, String varName, int value) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).setLocalVariable(nodeId, varName, new IntValue(value));
            }
        }
        return false;
    }

    public synchronized boolean setLocalVariable(SceneFlow sceneFlow, String nodeId, String varName, String value) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).setLocalVariable(nodeId, varName, new StringValue(value));
            }
        }
        return false;
    }

    public synchronized boolean setLocalVariable(SceneFlow sceneFlow, String nodeId, String varName, boolean value) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).setLocalVariable(nodeId, varName, new BooleanValue(value));
            }
        }
        return false;

    }

    public synchronized boolean hasLocaleVariable(SceneFlow sceneFlow, String nodeId, String varName) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).hasLocalVariable(nodeId, varName);
            }
        }
        return false;
    }

    public synchronized boolean hasVariable(SceneFlow sceneFlow, String varName) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).hasVariable(varName);
            }
        }
        return false;
    }

    public synchronized boolean hasVariable(SceneFlow sceneFlow, String varName, int index) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).hasVariable(varName, index);
            }
        }
        return false;
    }

    public synchronized boolean hasVariable(SceneFlow sceneFlow, String varName, String member) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).hasVariable(varName, member);
            }

        }
        return false;
    }

    public synchronized AbstractValue getValueOf(SceneFlow sceneFlow, String varName) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).getValueOf(varName);
            }

        }
        return null;
    }

    public synchronized AbstractValue getValueOf(SceneFlow sceneFlow, String varName, int index) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).getValueOf(varName, index);
            }

        }
        return null;
    }

    public synchronized AbstractValue getValueOf(SceneFlow sceneFlow, String varName, String member) {
        if (sceneFlow != null) {
            if (mSceneFlowMap.containsKey(sceneFlow)) {
                return mSceneFlowMap.get(sceneFlow).getValueOf(varName, member);
            }

        }
        return null;
    }
}
