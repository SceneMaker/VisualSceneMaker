/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players;

import de.dfki.vsm.players.action.Action;
import de.dfki.vsm.players.action.ActionListener;
import de.dfki.vsm.players.server.TCPActionServer;
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

	static ScheduledExecutorService sActionScheduler;
	static List<Action> sActionList;
	static Semaphore sActionPlaySync;
	static TCPActionServer sActionServer;
	static ActionPlayer sInstance;

	// for components that are interested in what's happening here
	static final ArrayList<ActionListener> mActionListeners = new ArrayList<>();

	public boolean mRunning = true;
	static final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
	private static long sID = 0;
	
	public static boolean mActionServerRunning = false;

	ActionPlayer() {
		initialize();
	}

	public String getNextID() {
		sID++;
		return "action" + sID;
	}

	public final void initialize() {
		sActionList = Collections.synchronizedList(new ArrayList());
		sActionScheduler = Executors.newScheduledThreadPool(10, new ActionThreadFactory());
		sActionPlaySync = new Semaphore(0);

		sActionServer = TCPActionServer.getInstance();
		sActionServer.start();
	}

	public void addAction(Action a) {
		// tell the action which player executes it
		a.mActionPlayer = this;
		// give unique id;
		a.mID = getNextID();
		// add action to the list of to be executed actions
		sActionList.add(a);
	}

	public void play() {
		//mLogger.message("Releasing ... Actions in Queue");

//		for (Action ac : sActionList) {
//			mLogger.message("\t" + ac.mName);
//		}

		sActionPlaySync.release();

		//mLogger.message("Released ...");
	}

	public void sendCmd(String cmd) {
		sActionServer.send(cmd);
	}

	public synchronized void end() {
		sActionServer.end();
		sActionServer = null;
		mRunning = false;
		sInstance = null;

		List<Runnable> scheduledTasks = sActionScheduler.shutdownNow();

		sActionPlaySync.release(2);
	}

	public static void actionEnded(Action a) {
		synchronized (sActionList) {
			//mLogger.message("Action " + a.mName + " with id (" + a.mID + ") has ended. " + sActionList.size() + " in queue...");
			Action aToRemove = null;
			for (Action ac : sActionList) {
				//mLogger.message("\tchecking if Action " + ac.mName + " with id (" + ac.mID + ") is in queue ...");
				aToRemove = (ac.mID == a.mID) ? ac : null;
				if (aToRemove != null) {
					//mLogger.message("\t\tYes! Wonderful!");
					sActionList.remove(aToRemove);
					break;
				}
			}

			// if all actions are ended - reset ActionPlayer
			if (sActionList.isEmpty()) {
				//mLogger.message("All actions played ... ending action sequence");
				sActionPlaySync.release();
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
