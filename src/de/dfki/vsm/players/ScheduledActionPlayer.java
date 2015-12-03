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
		super();
		super.setName("VisualSceneMaker Scheduled Action Player");
	}

	public static ScheduledActionPlayer getInstance() {
		if (sInstance == null) {
			sInstance = new ScheduledActionPlayer();
		}
		return (ScheduledActionPlayer) sInstance;
	}

	@Override
	public void run() {
		while (mRunning) {
			try {
				// wait for go
				sActionPlaySync.acquire();

				if (sActionList.size() > 0) {
					// now schedule all actions
					if (mRunning) {
						for (Action a : sActionList) {
							sActionScheduler.schedule(a, a.mStartTime, TimeUnit.MILLISECONDS);
						}
					}

					// wait for all actions ended
					sActionPlaySync.acquire();
				}
			} catch (InterruptedException ex) {
				mLogger.warning("ActionPlayer got interrupted " + ex.getMessage());
			}

			notifyListenersAllActionsFinished();
		}
	}
}
