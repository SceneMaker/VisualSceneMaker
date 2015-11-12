/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players;

import de.dfki.vsm.players.action.Action;
import de.dfki.vsm.players.action.ActionListener;
import de.dfki.vsm.players.stickman.StickmanStage;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.Semaphore;
import java.util.concurrent.ThreadFactory;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class ActionPlayer extends Thread {

	static ScheduledExecutorService mActionScheduler;
	static List<Action> mActionList;
	static Semaphore mActionPlaySync;

	// for components that are interested in what's happening here
	static final ArrayList<ActionListener> mActionListeners = new ArrayList<>();

	boolean mRunning = true;
	static final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

	protected ActionPlayer() {
		initialize();
	}

	public final void initialize() {
		mActionList = Collections.synchronizedList(new ArrayList());
		mActionScheduler = Executors.newScheduledThreadPool(10, new ActionThreadFactory());
		mActionPlaySync = new Semaphore(0);
	}

	public void addAction(Action a) {
		// tell the action which player executes it
		a.mActionPlayer = this;
		// add action to the list of to be executed actions
		mActionList.add(a);
	}

	public void play() {
		mActionPlaySync.release();
	}

	public static void actionEnded(Action a) {
		synchronized (mActionList) {
			notifyListenersAboutAction(a, ActionListener.STATE.ACTION_FINISHED);
			mActionList.remove(a);

			// if all actions are ended - reset ScheduledActionPlayer
			if (mActionList.isEmpty()) {
				mActionPlaySync.release();
			}
		}
	}

	public void addListener(ActionListener al) {
		mActionListeners.add(al);
	}

	public void removeListener(ActionListener al) {
		if (mActionListeners.contains(al)) {
			mActionListeners.remove(al);
		}
	}

	public static void notifyListenersAboutAction(Action a, ActionListener.STATE state) {
		synchronized (mActionListeners) {
			mActionListeners.stream().forEach((al) -> {
				al.update(a, state);
			});
		}
	}

	void notifyListenersAllActionsFinished() {
		synchronized (mActionListeners) {
			mActionListeners.stream().forEach((al) -> {
				al.update(ActionListener.STATE.ALL_ACTIONS_FINISHED);
			});
		}
	}

	class ActionThreadFactory implements ThreadFactory {

		@Override
		public Thread newThread(Runnable r) {
			return new Thread(r, "ActionPlayer Thread" + r.getClass().getSimpleName());
		}
	}
}
