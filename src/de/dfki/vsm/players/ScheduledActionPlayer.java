package de.dfki.vsm.players;

import de.dfki.vsm.players.action.Action;
import de.dfki.vsm.players.action.ActionListener;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.Semaphore;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class ScheduledActionPlayer extends Thread {

	private ScheduledExecutorService mActionScheduler;
	private static List<Action> mActionList;
	private static Semaphore mActionPlaySync;

	// for components that are interested in what's happening here
	private static final ArrayList<ActionListener> mActionListeners = new ArrayList<>();

	private boolean mRunning = true;
	private static final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

	public ScheduledActionPlayer() {
		this.setName("VisualSceneMaker Action Player");
		mActionList = Collections.synchronizedList(new ArrayList());
		mActionScheduler = Executors.newScheduledThreadPool(10, new ActionThreadFactory());
		mActionPlaySync = new Semaphore(0);
	}

	public void addAction(Action a) {
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

	public synchronized void end() {
		mRunning = false;

		List<Runnable> scheduledTasks = mActionScheduler.shutdownNow();

		mActionPlaySync.release(2);
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
		for (ActionListener al : mActionListeners) {
			al.update(a, state);
		}
	}

	private void notifyListenersAllActionsFinished() {
		for (ActionListener al : mActionListeners) {
			al.update(ActionListener.STATE.ALL_ACTIONS_FINISHED);
		}
	}

	@Override
	public void run() {
		while (mRunning) {
			try {
				// wait for go
				mActionPlaySync.acquire();

				// now shedule all actions
				if (mRunning) {
					for (Action a : mActionList) {
						mActionScheduler.schedule(a, a.mStartTime, TimeUnit.MILLISECONDS);
					}
				}

				// wait for all actions ended
				mActionPlaySync.acquire();
			} catch (InterruptedException ex) {
				mLogger.warning("ActionPlayer got interrupted " + ex.getMessage());
			}

			notifyListenersAllActionsFinished();
		}
	}

	class ActionThreadFactory implements ThreadFactory {

		@Override
		public Thread newThread(Runnable r) {
			return new Thread(r, "ActionPlayer Thread" + r.getClass().getSimpleName());
		}
	}
}
