package de.dfki.vsm.players.action;

import java.util.concurrent.Semaphore;

/**
 *
 * @author Patrick Gebhard
 *
 */
public abstract class Action implements Runnable {

	public int mStartTime;
	public Semaphore mActionEndSync = new Semaphore(0);

	public void end() {
		mActionEndSync.release();
	}

	@Override
	public void run() {
	}
}
