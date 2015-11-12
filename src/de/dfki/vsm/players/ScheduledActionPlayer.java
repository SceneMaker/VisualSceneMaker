package de.dfki.vsm.players;

import de.dfki.vsm.players.action.Action;
import java.util.concurrent.TimeUnit;


/**
 *
 * @author Patrick Gebhard
 *
 */
public class ScheduledActionPlayer extends ActionPlayer {


	public ScheduledActionPlayer() {
		super.setName("VisualSceneMaker Scheduled Action Player");

	}

	@Override
	public void run() {
		while (mRunning) {
			try {
				// wait for go
				mActionPlaySync.acquire();

				// now schedule all actions
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
}
