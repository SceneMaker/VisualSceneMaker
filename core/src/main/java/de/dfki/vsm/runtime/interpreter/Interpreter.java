package de.dfki.vsm.runtime.interpreter;

import de.dfki.vsm.event.EventDispatcher;
import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.runtime.interpreter.error.InterpreterError;
import de.dfki.vsm.runtime.interpreter.event.TerminationEvent;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.player.RunTimePlayer;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;

import java.util.HashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

/**
 * @author Gregor Mehlmann
 */
public class Interpreter {

    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final EventDispatcher mDispatcher;
    private final SceneFlow mSceneFlow;
    private final Interruptor mEventObserver;
    private final Configuration mConfiguration;
    private final SystemHistory mSystemHistory;
    private final Evaluator mEvaluator;
    private final TimeoutManager mTimeoutManager;
    private final ReentrantReadWriteLock mRwLock;
    private final Lock mLock;       // write lock — used by Process, Evaluator, TimeoutManager
    private final Lock mReadLock;   // read lock — used by getters and status queries
    private final Condition mPauseCondition;
    private final Condition mStateChangeCondition;
    private final RunTimePlayer mScenePlayer;
    private Process mSceneFlowThread;

    // Per-variable generation counters for guard dependency tracking (Priority 8).
    // Incremented on each write to the named variable. Used by Process to determine
    // whether a cached guard result is still valid.
    private final HashMap<String, Long> mVarGenerations = new HashMap<>();
    // Global state generation counter. Incremented on every signalStateChange().
    // Guards with opaque dependencies (function calls, InStateQuery, etc.)
    // are invalidated whenever this counter changes.
    private long mStateGeneration = 0;

    // Construct an interpreter with a project
    public Interpreter(final RunTimeProject project) {
        mDispatcher = project.getEventDispatcher();
        // Initialize the runtime project
        // Initialize the sceneflow object
        mSceneFlow = project.getSceneFlow();
        // TODO: We want only one scene player
        mScenePlayer = project.getRunTimePlayer();
        //mDialogPlayer = new DefaultPlayer(project);
        mRwLock = new ReentrantReadWriteLock(true);
        mLock = mRwLock.writeLock();
        mReadLock = mRwLock.readLock();
        mPauseCondition = mRwLock.writeLock().newCondition();
        mStateChangeCondition = mRwLock.writeLock().newCondition();
        mConfiguration = new Configuration();
        mSystemHistory = new SystemHistory();
        mTimeoutManager = new TimeoutManager(this);
        mEventObserver = new Interruptor(this);
        mEvaluator = new Evaluator(this);
    }

    public Lock getLock() {
        return mLock;
    }

    EventDispatcher getDispatcher() {
        return mDispatcher;
    }

    void lock() {
        mLock.lock();
    }

    void unlock() {
        mLock.unlock();
    }

    boolean isWriteLockHeldByCurrentThread() {
        return mRwLock.isWriteLockedByCurrentThread();
    }

    private void readLock() {
        mReadLock.lock();
    }

    private void readUnlock() {
        mReadLock.unlock();
    }

    void await() {
        mPauseCondition.awaitUninterruptibly();
    }

    void signalAll() {
        boolean lockedHere = false;
        if (!isWriteLockHeldByCurrentThread()) {
            mLock.lock();
            lockedHere = true;
        }
        try {
            mPauseCondition.signalAll();
        } finally {
            if (lockedHere) {
                mLock.unlock();
            }
        }
    }

    // Wait for a state change (variable write, timeout, etc.) or until timeoutMs elapses.
    // Must be called while holding the lock. Atomically releases lock, waits, re-acquires.
    void awaitStateChange(long timeoutMs) {
        try {
            if (timeoutMs > 0) {
                mStateChangeCondition.await(timeoutMs, TimeUnit.MILLISECONDS);
            } else {
                mStateChangeCondition.await(200, TimeUnit.MILLISECONDS);
            }
        } catch (InterruptedException e) {
            // Interrupted — checkStatus() will detect termination/interruption requests
            Thread.currentThread().interrupt();
        }
    }

    // Signal all threads waiting for state changes (edge guard re-evaluation).
    // Must be called while holding the lock.
    void signalStateChange() {
        boolean lockedHere = false;
        if (!isWriteLockHeldByCurrentThread()) {
            mLock.lock();
            lockedHere = true;
        }
        try {
            mStateGeneration++;
            mStateChangeCondition.signalAll();
        } finally {
            if (lockedHere) {
                mLock.unlock();
            }
        }
    }

    // Record that the named variable was written. Must be called while holding the lock.
    // This increments the per-variable generation counter so that guard caches
    // depending on this variable are invalidated.
    void notifyVariableChanged(String varName) {
        mVarGenerations.merge(varName, 1L, Long::sum);
    }

    long getStateGeneration() {
        return mStateGeneration;
    }

    long getVarGeneration(String varName) {
        return mVarGenerations.getOrDefault(varName, 0L);
    }

    // Mark the interruptor dirty so that next update() re-evaluates interrupt conditions.
    void markInterruptorDirty() {
        mEventObserver.markDirty();
    }

    public SceneFlow getSceneFlow() {
        try {
            readLock();
            return mSceneFlow;
        } finally {
            readUnlock();
        }
    }

    Evaluator getEvaluator() {
        try {
            readLock();
            return mEvaluator;
        } finally {
            readUnlock();
        }
    }

    // Get the scene player
    final RunTimePlayer getScenePlayer() {
        try {
            readLock();
            return mScenePlayer;
        } finally {
            readUnlock();
        }
    }

    public Configuration getConfiguration() {
        try {
            readLock();
            return mConfiguration;
        } finally {
            readUnlock();
        }
    }

    TimeoutManager getTimeoutManager() {
        try {
            readLock();
            return mTimeoutManager;
        } finally {
            readUnlock();
        }
    }

    Interruptor getEventObserver() {
        try {
            readLock();
            return mEventObserver;
        } finally {
            readUnlock();
        }
    }

    SystemHistory getSystemHistory() {
        try {
            readLock();
            return mSystemHistory;
        } finally {
            readUnlock();
        }
    }

    // Start the execution of the project
    public final boolean start() {

        // TODO: This is insecure, cause the thread could die in the meantime
        // alive is not the right condition
        // PathLogger.startLogging();
        if ((mSceneFlowThread == null) || (!mSceneFlowThread.isAlive())) {
            // Create a new thread
            mSceneFlowThread = new Process(mSceneFlow.getId(), null, // TODO: choose an adquate thread group and check if this group has died before
                    mSceneFlow, new Environment(mDispatcher), 0, null, this);

            // Lock the interpreter
            try {
                lock();

                try {
                    mSceneFlowThread.handleStart();
                    mEventObserver.update();
                } catch (InterpreterError e) {
                    mDispatcher.convey(new TerminationEvent(this, e));
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
            try {
                lock();

                mSceneFlowThread.requestTermination();
            } finally {
                unlock();
            }
        } else {
            mLogger.warning("Interpreter '" + this + "' cannot abort the execution of '" + mSceneFlow.getId() + "'");
        }
        mTimeoutManager.abort();
        //mDispatcher.abort();
        return true;
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
            readLock();
            if (mSceneFlowThread != null) {
                return mSceneFlowThread.isPauseRequested();
            }
        } finally {
            readUnlock();
        }
        return false;
    }

    public boolean isRunning() {
        try {
            readLock();
            if (mSceneFlowThread != null) {
                return mSceneFlowThread.isRunning();
            }
        } finally {
            readUnlock();
        }
        return false;
    }

    public boolean wasExecuted() {
        try {
            readLock();
            if (mSceneFlowThread != null) {
                return mSceneFlowThread.wasExecuted();
            }
        } finally {
            readUnlock();
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
            readLock();
            return mEvaluator.evaluate(exp, mConfiguration.getState(nodeId).getThread().getEnvironment());
        } catch (InterpreterError e) {
            return null;
        } finally {
            readUnlock();
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
            notifyVariableChanged(varName);
            mEventObserver.markDirty();
            mEventObserver.update();
            signalStateChange();
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
            notifyVariableChanged(varName);
            mEventObserver.markDirty();
            mEventObserver.update();
            signalStateChange();
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
            notifyVariableChanged(varName);
            mEventObserver.markDirty();
            mEventObserver.update();
            signalStateChange();
            return true;
        } catch (InterpreterError e) {
            return false;
        } finally {
            unlock();
        }
    }

    public boolean hasLocalVariable(String nodeId, String varName) {
        try {
            readLock();
            return mConfiguration.getState(nodeId).getThread().getEnvironment().getActiveSymbolTable().contains(
                    varName);
        } catch (InterpreterError e) {
            return false;
        } finally {
            readUnlock();
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
            readLock();
            mConfiguration.getState(mSceneFlow).getThread().getEnvironment().read(varName);
            return true;
        } catch (InterpreterError e) {
            return false;
        } finally {
            readUnlock();
        }
    }

    public boolean hasVariable(String varName, int index) {
        try {
            readLock();
            mConfiguration.getState(mSceneFlow).getThread().getEnvironment().read(varName, index);
            return true;
        } catch (InterpreterError e) {
            return false;
        } finally {
            readUnlock();
        }
    }

    public boolean hasVariable(String varName, String member) {
        try {
            readLock();
            mConfiguration.getState(mSceneFlow).getThread().getEnvironment().read(varName, member);
            return true;
        } catch (InterpreterError e) {
            return false;
        } finally {
            readUnlock();
        }
    }

    public AbstractValue getValueOf(String varName) {
        try {
            readLock();
            return mConfiguration.getState(mSceneFlow).getThread().getEnvironment().read(varName);
        } catch (InterpreterError e) {
            return null;
        } finally {
            readUnlock();
        }
    }

    // TODO let the evaluator do this over getValueOf(...)
    public AbstractValue getValueOf(String varName, int index) {
        try {
            readLock();
            return mConfiguration.getState(mSceneFlow).getThread().getEnvironment().read(varName, index);
        } catch (InterpreterError e) {
            return null;
        } finally {
            readUnlock();
        }
    }

    public AbstractValue getValueOf(String varName, String member) {
        try {
            readLock();
            return mConfiguration.getState(mSceneFlow).getThread().getEnvironment().read(varName, member);
        } catch (InterpreterError e) {
            return null;
        } finally {
            readUnlock();
        }
    }
}
