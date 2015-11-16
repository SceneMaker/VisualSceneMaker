package de.dfki.vsm.players.stickman.action;

import de.dfki.vsm.players.action.ActionListener;
import de.dfki.vsm.players.action.EventAction;
import de.dfki.vsm.players.action.sequence.WordTimeMarkSequence;
import de.dfki.vsm.players.stickman.Stickman;
import de.dfki.vsm.players.stickman.animationlogic.Animation;
import de.dfki.vsm.players.stickman.animationlogic.listener.AnimationListener;
import java.util.ArrayList;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class StickmanEventAction extends EventAction implements AnimationListener {

	private final Stickman mStickman;
	int mDuration;
	public WordTimeMarkSequence mWTS;
	boolean mBlocking;
	public Animation mAnimation;

	public StickmanEventAction(Stickman stickman, int starttime, String name, int dur, WordTimeMarkSequence wts, boolean block) {
		Thread.currentThread().setName("Stickman event action " + name);
		mStickman = stickman;
		mStartTime = starttime;
		mName = name;
		mDuration = dur;
		mWTS = wts;
		mBlocking = block;
		//timestamps when to call other actions
		mSynchronizedActionTimeMarks = new ArrayList<>();
	}

	@Override
	public void update(Animation a) {
		if (a.equals(mAnimation)) {
			mActionEndSync.release();
		}
	}

	@Override
	public void run() {
		try {
			mActionPlayer.notifyListenersAboutAction(this, ActionListener.STATE.ACTION_STARTED);

			// mStickman.mLogger.info("Starting event action - animation" + mType + ".event." + mName);
			mAnimation = mStickman.doEventFeedbackAnimation(mName, mDuration, mWTS, mBlocking);

			if (mAnimation == null) {
				mStickman.mLogger.severe("animation" + mName + " is not known by Stickman ...");
				mActionPlayer.notifyListenersAboutAction(this, ActionListener.STATE.ACTION_UNKNOWN);
			} else {
				// tell Stickman to update Action about the animation status
				mStickman.addListener(this);
				// wait for action end   
				mActionEndSync.acquire();

				mStickman.removeListener(this);
			}

			// notify Action Player
			mActionPlayer.actionEnded(this);
		} catch (InterruptedException ex) {
			mStickman.mLogger.warning("Action " + mName + " got interrupted");
		}
	}
}
