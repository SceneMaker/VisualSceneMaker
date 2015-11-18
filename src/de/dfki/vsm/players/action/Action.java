package de.dfki.vsm.players.action;

import de.dfki.vsm.players.ActionPlayer;
import java.util.concurrent.Semaphore;

/**
 *
 * @author Patrick Gebhard
 *
 */
public abstract class Action implements Runnable {

	// the name of the action
	public String mName;
	
	// the unique id 
	public String mID = "none";

	// the action layer that actually executes this action
	public ActionPlayer mActionPlayer;

	// needed for scheduled actions
	public int mStartTime;

	// a time mark as contraint when to execute an action
	public String mTimeMark;

	// needed for controlled end of action
	public Semaphore mActionEndSync = new Semaphore(0);

	public void setTimeMark(String mark) {
		mTimeMark = mark;
	}

	public void end() {
		mActionEndSync.release();
	}

	@Override
	public void run() {
	}
}
