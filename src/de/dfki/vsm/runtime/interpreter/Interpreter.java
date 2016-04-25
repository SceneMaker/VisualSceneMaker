package de.dfki.vsm.runtime.interpreter;

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.model.sceneflow.BasicNode;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.runtime.interpreter.error.InterpreterError;
import de.dfki.vsm.runtime.interpreter.event.TerminationEvent;
import de.dfki.vsm.runtime.interpreter.Configuration.State;
import de.dfki.vsm.runtime.player.RunTimePlayer;
import de.dfki.vsm.runtime.player.ReactivePlayer;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.runtime.interpreter.symbol.SymbolTable;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.tpl.TPLTriple;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

/**
 * @author Gregor Mehlmann
 */
public class Interpreter {

    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final EventDispatcher mEventMulticaster = EventDispatcher.getInstance();
    private final SceneFlow mSceneFlow;
    private final Interruptor mEventObserver;
    private final Configuration mConfiguration;
    private final SystemHistory mSystemHistory;
    private final Evaluator mEvaluator;
    private final TimeoutManager mTimeoutManager;
    private final ReentrantLock mLock;
    private final Condition mPauseCondition;
    //private final RunTimePlayer mScenePlayer;
    private final RunTimePlayer mScenePlayer;
    //private final RunTimePlayer mDialogPlayer;
    //private final RunTimePlayer mDialogPlayer;
    private final RunTimeProject mRunTimeProject;
    private Process mSceneFlowThread;

    // Construct an interpreter with a project
    public Interpreter(final RunTimeProject project) {
        // Initialize the runtime project
        mRunTimeProject = project;
        // Initialize the sceneflow object
        mSceneFlow = mRunTimeProject.getSceneFlow();
        // TODO: We want only one scene player
        mScenePlayer = mRunTimeProject.getRunTimePlayer();
        //mDialogPlayer = new DefaultPlayer(project);
        mLock = new ReentrantLock(true);
        mPauseCondition = mLock.newCondition();
        mConfiguration = new Configuration();
        mSystemHistory = new SystemHistory();
        mTimeoutManager = new TimeoutManager(this);
        mEventObserver = new Interruptor(this);
        mEvaluator = new Evaluator(this);
    }

    public ReentrantLock getLock() {
        return mLock;
    }

    public void lock() {
        //mLogger.message("REQUEST (" + Thread.currentThread() + "," + mLock.getHoldCount()+ ")");

        //mLogger.message("IS LOCKED : " +mLock.isLocked());
        //mLogger.message("IS LOCKED BY CURRENTTHREAD : " + mLock.isHeldByCurrentThread());
        mLock.lock();

        //mLogger.message("ACQUIRE (" + Thread.currentThread() + "," + mLock.getHoldCount()+ ")");
    }

    public void unlock() {
        mLock.unlock();

        //mLogger.message("RELEASE (" + Thread.currentThread() + "," + mLock.getHoldCount()+ ")");
    }

    public void await() {
        mPauseCondition.awaitUninterruptibly();
    }

    public void signalAll() {
        mPauseCondition.signalAll();
    }

    public SceneFlow getSceneFlow() {
        try {
            lock();

            return mSceneFlow;
        } finally {
            unlock();
        }
    }

    public Evaluator getEvaluator() {
        try {
            lock();

            return mEvaluator;
        } finally {
            unlock();
        }
    }

    // Get the scene player
    public final RunTimePlayer getScenePlayer() {
        try {
            lock();
            return mScenePlayer;
        } finally {
            unlock();
        }
    }

    /*
     public  DefaultPlayer getDialogPlayer() {
     try {
     lock();

     return mDialogPlayer;
     } finally {
     unlock();
     }
     }
     */
    public Configuration getConfiguration() {
        try {
            lock();

            return mConfiguration;
        } finally {
            unlock();
        }
    }

    public TimeoutManager getTimeoutManager() {
        try {
            lock();

            return mTimeoutManager;
        } finally {
            unlock();
        }
    }

    public Interruptor getEventObserver() {
        try {
            lock();

            return mEventObserver;
        } finally {
            unlock();
        }
    }

    public SystemHistory getSystemHistory() {
        try {
            lock();

            return mSystemHistory;
        } finally {
            unlock();
        }
    }

    // Start the execution of the project
    public final boolean start() {

        // TODO: This is insecure, cause the thread could die in the meantime
        // alive is not the right condition
        // PathLogger.startLogging();
        if ((mSceneFlowThread == null) || (!mSceneFlowThread.isAlive())) {

            // Print some information 
            //mLogger.message("Starting execution of project '" + mRunTimeProject + "' with interpreter '" + this + "'");
            // Create a new thread
            mSceneFlowThread = new Process(mSceneFlow.getId(), null, // TODO: choose an adquate thread group and check if this group has died before
                    mSceneFlow, new Environment(), 0, null, this);

            // Lock the interpreter
            try {
                lock();

                try {
                    mSceneFlowThread.handleStart();
                    mEventObserver.update();
                } catch (InterpreterError e) {
                    mEventMulticaster.convey(new TerminationEvent(this, e));
                    mSceneFlowThread.requestTermination();

                    // Wait here until terminated and clear data structures
                }
            } finally {
                unlock();
            }
        }

        return true;
    }

    public boolean abort() {
        if ((mSceneFlowThread != null) && (mSceneFlowThread.isAlive())) {    // TODO: This is insecure, cause the thread could start in the meantime

            // Print some information 
            //mLogger.message("Aborting execution of project '" + mRunTimeProject + "' with interpreter '" + this + "'");
            try {
                lock();

                mSceneFlowThread.requestTermination();
            } finally {
                unlock();
            }

            // Wait here until terminated and clear data structures
            // Clean up the data structures of the interpreter
            // mSystemHistory.clear();
            // mConfiguration.clear();
            // mTimeoutManager.clear();
            /**
             * Notification
             */
        } else {
            mLogger.warning("Interpreter '" + this + "' cannot abort the execution of '" + mSceneFlow.getId() + "'");
        }

        // mLogger.message("Stopping EventCaster and TimeoutManager");
        mTimeoutManager.cancel();
        mEventMulticaster.cancel();

        return true;
        // PathLogger.stopLogging();
    }

    public boolean pause() {
        lock();

        if ((mSceneFlowThread != null) && (mSceneFlowThread.isAlive())) {
            mSceneFlowThread.requestPause();
        }

        unlock();

        return true;
    }

    public boolean proceed() {
        lock();

        if ((mSceneFlowThread != null) && (mSceneFlowThread.isAlive())) {
            mSceneFlowThread.requestProceed();
        }

        unlock();

        return true;
    }

    public boolean isPaused() {
        try {
            lock();
            if (mSceneFlowThread != null) {
                return mSceneFlowThread.isPauseRequested();
            }
        } finally {
            unlock();
        }
        return false;
    }

    public boolean isRunning() {
        try {
            lock();
            if (mSceneFlowThread != null) {
                return mSceneFlowThread.isRunning();
            }
        } finally {
            unlock();
        }
        return false;
    }

    public boolean wasExecuted() {
        try {
            lock();
            if (mSceneFlowThread != null) {
                return mSceneFlowThread.wasExecuted();
            }
        } finally {
            unlock();
        }
        return false;
    }

    ////////////////////////////////////////////////////////////////////////////
    //
    ////////////////////////////////////////////////////////////////////////////
    public boolean execute(String nodeId, Command cmd) {
        try {
            lock();
            mEvaluator.execute(cmd, mConfiguration.getState(nodeId).getThread().getEnvironment());

            return true;
        } catch (InterpreterError e) {
            return false;
        } finally {
            unlock();
        }
    }

    public AbstractValue evaluate(String nodeId, Expression exp) {
        try {
            lock();

            return mEvaluator.evaluate(exp, mConfiguration.getState(nodeId).getThread().getEnvironment());
        } catch (InterpreterError e) {
            return null;
        } finally {
            unlock();
        }
    }

    public boolean setVariable(String nodeId, String varName, Expression exp) {
        try {
            lock();

            AbstractValue value = mEvaluator.evaluate(exp,
                    mConfiguration.getState(nodeId).getThread().getEnvironment());

            setVariable(varName, value);

            return true;
        } catch (InterpreterError e) {
            return false;
        } finally {
            unlock();
        }
    }

    public boolean setVariable(String varName, AbstractValue value) {
        try {

            lock();
            
            mConfiguration.getState(mSceneFlow).getThread().getEnvironment().write(varName, value);
            
            
            //BasicNode currentNode = EditorInstance.getInstance().getSelectedProjectEditor().getSceneFlowEditor().getSceneFlowManager().getCurrentActiveSuperNode();
            //mConfiguration.getState(currentNode).getThread().getEnvironment().write(varName, value);
            
            mEventObserver.update();

            return true;
        } catch (InterpreterError e) {
            e.printStackTrace();

            return false;
        } finally {
            unlock();
        }
    }

    public boolean setVariable(String varName, int index, AbstractValue value) {
        try {
            lock();
            mConfiguration.getState(mSceneFlow).getThread().getEnvironment().write(varName, index, value);
            mEventObserver.update();

            return true;
        } catch (InterpreterError e) {
            return false;
        } finally {
            unlock();
        }
    }

    public boolean setVariable(String varName, String member, AbstractValue value) {
        try {
            lock();
            mConfiguration.getState(mSceneFlow).getThread().getEnvironment().write(varName, member, value);
            mEventObserver.update();

            return true;
        } catch (InterpreterError e) {
            return false;
        } finally {
            unlock();
        }
    }

    public boolean hasLocalVariable(String nodeId, String varName) {
        try {
            lock();

            return mConfiguration.getState(nodeId).getThread().getEnvironment().getActiveSymbolTable().contains(
                    varName);
        } catch (InterpreterError e) {
            return false;
        } finally {
            unlock();
        }
    }

    public boolean setLocalVariable(String nodeId, String varName, AbstractValue value) {
        try {
            lock();
            mConfiguration.getState(nodeId).getThread().getEnvironment().write(varName, value);

            return true;
        } catch (InterpreterError e) {
            return false;
        } finally {
            unlock();
        }
    }

    public boolean hasVariable(String varName) {
        try {
            lock();
            mConfiguration.getState(mSceneFlow).getThread().getEnvironment().read(varName);

            return true;
        } catch (InterpreterError e) {
            e.printStackTrace();
            return false;
        } finally {
            unlock();
        }
    }

    public boolean hasVariable(String varName, int index) {
        try {
            lock();
            mConfiguration.getState(mSceneFlow).getThread().getEnvironment().read(varName, index);

            return true;
        } catch (InterpreterError e) {
            return false;
        } finally {
            unlock();
        }
    }

    public boolean hasVariable(String varName, String member) {
        try {
            lock();
            mConfiguration.getState(mSceneFlow).getThread().getEnvironment().read(varName, member);

            return true;
        } catch (InterpreterError e) {

            return false;
        } finally {
            unlock();
        }
    }

    public AbstractValue getValueOf(String varName) {
        try {
            lock();

            return mConfiguration.getState(mSceneFlow).getThread().getEnvironment().read(varName);
        } catch (InterpreterError e) {
            return null;
        } finally {
            unlock();
        }
    }

    // TODO let the evaluator do this over getValueOf(...)
    public AbstractValue getValueOf(String varName, int index) {
        try {
            lock();

            return mConfiguration.getState(mSceneFlow).getThread().getEnvironment().read(varName, index);
        } catch (InterpreterError e) {
            return null;
        } finally {
            unlock();
        }
    }

    public AbstractValue getValueOf(String varName, String member) {
        try {
            lock();

            return mConfiguration.getState(mSceneFlow).getThread().getEnvironment().read(varName, member);
        } catch (InterpreterError e) {
            return null;
        } finally {
            unlock();
        }
    }

    /*
     public ArrayList listActiveStates() {

     try {
     // First lock the interpreter
     lock();
     // Create the result list
     final ArrayList list = new ArrayList();
     // List all active state 
     final Object[] states = mConfiguration.getOrderedStates();
     //
     for (final Object object : states) {
     final State state = (State) object;
     //
     final String pname = state.getThread().getName();
     final String nname = state.getNode().getName();
     //
     final Environment env = state.getThread().getEnvironment();
     final SymbolTable table = env.getActiveSymbolTable();
     final HashMap map = table.copySymbolTable();
     //
     final TPLTriple triple = new TPLTriple(pname, nname, map);
     //
     list.add(triple);
     }
     // Return the result list
     return list;
     //return new ArrayList(Arrays.asList(Arrays.copyOf(states, states.length)));
     } finally {
     // Finally unlock the interpreter
     unlock();
     }
     }*/
}
