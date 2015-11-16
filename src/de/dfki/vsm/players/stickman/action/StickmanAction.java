package de.dfki.vsm.players.stickman.action;

import de.dfki.vsm.players.action.ActionListener;
import de.dfki.vsm.players.action.Action;
import de.dfki.vsm.players.stickman.Stickman;
import de.dfki.vsm.players.stickman.animationlogic.Animation;
import de.dfki.vsm.players.stickman.animationlogic.listener.AnimationListener;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class StickmanAction extends Action implements AnimationListener {

	private final Stickman mStickman;
	public String mType;
	int mDuration;
	public String mText;
	boolean mBlocking;
	public Animation mAnimation;

	public StickmanAction(Stickman stickman, int starttime, String type, String name, int dur, String text, boolean block) {
		Thread.currentThread().setName("Stickman action " + name);
		mStickman = stickman;
		mStartTime = starttime;
		mType = type;
		mName = name;
		mDuration = dur;
		mText = text;
		mBlocking = block;
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

			mAnimation = mStickman.doAnimation(mType, mName, mDuration, mText, mBlocking);

			if (mAnimation == null) {
				mStickman.mLogger.severe("animation " + mType + " " + mName + " is not known by Stickman ...");
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
