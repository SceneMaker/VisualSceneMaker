package de.dfki.vsm.players.stickman.action;

import static de.dfki.vsm.players.ActionPlayer.notifyListenersAboutAction;
import de.dfki.vsm.players.action.ActionListener;
import de.dfki.vsm.players.action.Action;
import de.dfki.vsm.players.stickman.Stickman;
import de.dfki.vsm.players.stickman.animationlogic.Animation;
import de.dfki.vsm.players.stickman.animationlogic.AnimationLoader;
import de.dfki.vsm.players.stickman.animationlogic.listener.AnimationListener;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class StickmanAction extends Action implements AnimationListener {

	private final Stickman mStickman;

	int mDuration;
	Object mParameter;
	boolean mBlocking;
	public Animation mAnimation;

	public StickmanAction(Stickman stickman, int starttime, String name, int dur, Object param, boolean block) {
		Thread.currentThread().setName("Stickman action " + name);
		mStickman = stickman;
		mStartTime = starttime;
		mName = name;
		mDuration = dur;
		mParameter = param;
		mBlocking = block;
		mAnimation = AnimationLoader.getInstance().loadAnimation(mStickman, mName, mDuration, mBlocking);
		mAnimation.mParameter = mParameter;
	}
	
	@Override
	public void update(Animation a) {
//		mStickman.mLogger.info("Action (" + mID + ") - Animation hat holds " + a.getName() + " (" + a.mID + ") got update ...");
//		mStickman.mLogger.info("\twaiting for update from animation with id " + mAnimation.mID);

//		if (a.equals(mAnimation)) {
//			mActionEndSync.release();
//		}
		if (a.mID.equalsIgnoreCase(mAnimation.mID)) {
			//mStickman.mLogger.info("\tsuccessfully ended ...");
			mActionEndSync.release();
		}

	}

	@Override
	public void run() {
		try {
			mActionPlayer.notifyListenersAboutAction(this, ActionListener.STATE.ACTION_STARTED);

			if (mAnimation == null) {
				mStickman.mLogger.severe("animation " + mName + " is not known by Stickman ...");
				mActionPlayer.notifyListenersAboutAction(this, ActionListener.STATE.ACTION_UNKNOWN);
			} else {
				mStickman.playAnimation(mAnimation);

				// tell Stickman to update Action about the animation status
				mStickman.addListener(this);
				// wait for action end   
				mActionEndSync.acquire();

				mStickman.removeListener(this);
			}

			// notify Action Player
			//mStickman.mLogger.info("\ttelling action player action (" + mID + ") has ended ...");
			
			notifyListenersAboutAction(this, ActionListener.STATE.ACTION_FINISHED);
			mActionPlayer.actionEnded(this);
		} catch (InterruptedException ex) {
			mStickman.mLogger.warning("Action " + mName + " got interrupted");
		}
	}
}
