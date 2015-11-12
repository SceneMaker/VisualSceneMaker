package de.dfki.vsm.players;

import static de.dfki.vsm.players.ActionPlayer.mActionScheduler;
import de.dfki.vsm.players.action.Action;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class EventActionPlayer extends ActionPlayer {

	private static EventActionPlayer sInstance;

	private EventActionPlayer() {
		super();
		super.setName("VisualSceneMaker Event Action Player");
	}

	public static EventActionPlayer getInstance() {
		if (sInstance == null) {
			sInstance = new EventActionPlayer();
		}
		return sInstance;
	}

	public void addTimeMarkAction(Action a) {
		// tell the action which player executes it
		a.mActionPlayer = this;
		// add action to the list of to be executed actions
		mActionList.add(a);
	}

	public void runActionAtTimeMark(String timemark) {
//		mLogger.message("Executing actions at timemark " + timemark);
//		mLogger.message("Actions in queue " + mActionList.size());
//		for (Action a : mActionList) {
//			mLogger.message("\taction " + a.mName);
//			mLogger.message("\ttimemark " + a.mTimeMark);
//		}

		for (Action a : mActionList) {
			// if the timestamp is reached, play all actions that should be played at that timemark immediately
			if (a.mTimeMark != null) {
				if (a.mTimeMark.equalsIgnoreCase(timemark)) {
					a.mStartTime = 0;
					//mLogger.message("\taction " + a.toString());
					mActionScheduler.schedule(a, 0, TimeUnit.MILLISECONDS);
				}
			}
		}
	}

	public synchronized void end() {
		mRunning = false;
		sInstance = null;

		List<Runnable> scheduledTasks = mActionScheduler.shutdownNow();

		mActionPlaySync.release(2);
	}

	@Override
	public void run() {
		while (mRunning) {
			try {
				// wait for go
				mActionPlaySync.acquire();

				// now schedule all existing actions - actions that shoudl be played directly;
				// one of them should contain information for the player about timestamps when to trigger other actions
				if (mRunning) {
					mActionList.stream().forEach((a) -> {
						// start all actions  which should be started at starttime 0
						if (a.mStartTime != -1) {
							mActionScheduler.schedule(a, a.mStartTime, TimeUnit.MILLISECONDS);
						}
					});
				}

				// wait for all actions ended
				mActionPlaySync.acquire();
				mLogger.warning("Event ActionPlayer played all actions");
			} catch (InterruptedException ex) {
				mLogger.warning("Event ActionPlayer got interrupted " + ex.getMessage());
			}

			notifyListenersAllActionsFinished();
		}
	}
}
