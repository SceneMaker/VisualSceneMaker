package de.dfki.vsm.runtime;

import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.runtime.error.RunTimeException;
import de.dfki.vsm.runtime.event.AbortEvent;
import de.dfki.vsm.runtime.player.Player;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.runtime.value.AbstractValue;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

/**
 * @author Gregor Mehlmann
 */
public class Interpreter {

    private final LOGDefaultLogger mLogger;
    private final EventDispatcher mEventMulticaster;
    private final SceneFlow mSceneFlow;
    private final EventObserver mEventObserver;
    private final Configuration mConfiguration;
    private final SystemHistory mSystemHistory;
    private final Evaluator mEvaluator;
    private final TimeoutManager mTimeoutManager;
    private final ReentrantLock mLock;
    private final Condition mPauseCondition;
    private final Player mScenePlayer;
    private final Player mDialogPlayer;
    private final RunTimeProject mRunTimeProject;
    private Process mSceneFlowThread;

    // Construct an interpreter with a project
    public Interpreter(final RunTimeProject project) {
        // Initialize the runtime project
        mRunTimeProject = project;
        // Initialize the sceneflow object
        mSceneFlow = mRunTimeProject.getSceneFlow();
        // TODO: We want only one scene player
        mScenePlayer = mRunTimeProject.getPlayer("sceneplayer");
        mDialogPlayer = mRunTimeProject.getPlayer("dialogplayer");
        //
        mLogger = LOGDefaultLogger.getInstance();
        mEventMulticaster = EventDispatcher.getInstance();
        mLock = new ReentrantLock(true);
        mPauseCondition = mLock.newCondition();
        mConfiguration = new Configuration();
        mSystemHistory = new SystemHistory();
        mTimeoutManager = new TimeoutManager(this);
        mEventObserver = new EventObserver(this);
        mEvaluator = new Evaluator(this);
    }

    public void lock() {

        // mLogger.message("REQUEST (" + Thread.currentThread() + ")");
        mLock.lock();

        // mLogger.message("AQUIRE (" + Thread.currentThread() + ")");
    }

    public void unlock() {
        mLock.unlock();

        // mLogger.message("RELEASE (" + Thread.currentThread() + ")");
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
    public final Player getScenePlayer() {
        try {
            lock();

            return mScenePlayer;
        } finally {
            unlock();
        }
    }

    public Player getDialoguePlayer() {
        try {
            lock();

            return mDialogPlayer;
        } finally {
            unlock();
        }
    }

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

    public EventObserver getEventObserver() {
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
            mLogger.message("Starting execution of project '" + mRunTimeProject + "' with interpreter '" + this + "'");

            // Create a new thread
            mSceneFlowThread = new Process(mSceneFlow.getId(), null, // TODO: choose an adquate thread group and check if this group has died before
                    mSceneFlow, new Environment(), 0, null, this);

            // Lock the interpreter
            try {
                lock();

                try {
                    mSceneFlowThread.handleStart();
                    mEventObserver.update();
                } catch (RunTimeException e) {
                    mEventMulticaster.convey(new AbortEvent(this, e));
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
            mLogger.message("Aborting execution of project '" + mRunTimeProject + "' with interpreter '" + this + "'");

            try {
                lock();

                // mLogger.warning("Stopping execution of interpreter");
                mSceneFlowThread.requestTermination();
            } finally {
                unlock();
            }

            // Wait here until terminated and clear data structures
            // /** Clean up the data structures of the interpreter */
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

            return mSceneFlowThread.isPauseRequested();
        } finally {
            unlock();
        }

    }

    public boolean isRunning() {
        try {
            lock();

            return mSceneFlowThread.isRunning();
        } finally {
            unlock();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    //
    ////////////////////////////////////////////////////////////////////////////
    public boolean execute(String nodeId, Command cmd) {
        try {
            lock();
            mEvaluator.execute(cmd, mConfiguration.getState(nodeId).getThread().getEnvironment());

            return true;
        } catch (RunTimeException e) {
            return false;
        } finally {
            unlock();
        }
    }

    public AbstractValue evaluate(String nodeId, Expression exp) {
        try {
            lock();

            return mEvaluator.evaluate(exp, mConfiguration.getState(nodeId).getThread().getEnvironment());
        } catch (RunTimeException e) {
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
        } catch (RunTimeException e) {
            return false;
        } finally {
            unlock();
        }
    }

    public boolean setVariable(String varName, AbstractValue value) {
        try {
            lock();
            mConfiguration.getState(mSceneFlow).getThread().getEnvironment().write(varName, value);
            mEventObserver.update();

            return true;
        } catch (RunTimeException e) {
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
        } catch (RunTimeException e) {
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
        } catch (RunTimeException e) {
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
        } catch (RunTimeException e) {
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
        } catch (RunTimeException e) {
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
        } catch (RunTimeException e) {
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
        } catch (RunTimeException e) {
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
        } catch (RunTimeException e) {
            return false;
        } finally {
            unlock();
        }
    }

    public AbstractValue getValueOf(String varName) {
        try {
            lock();

            return mConfiguration.getState(mSceneFlow).getThread().getEnvironment().read(varName);
        } catch (RunTimeException e) {
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
        } catch (RunTimeException e) {
            return null;
        } finally {
            unlock();
        }
    }

    public AbstractValue getValueOf(String varName, String member) {
        try {
            lock();

            return mConfiguration.getState(mSceneFlow).getThread().getEnvironment().read(varName, member);
        } catch (RunTimeException e) {
            return null;
        } finally {
            unlock();
        }
    }
}
