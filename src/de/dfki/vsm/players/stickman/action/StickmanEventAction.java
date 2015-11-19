package de.dfki.vsm.players.stickman.action;

import de.dfki.vsm.players.action.ActionListener;
import de.dfki.vsm.players.action.EventAction;
import de.dfki.vsm.players.stickman.Stickman;
import de.dfki.vsm.players.stickman.animationlogic.Animation;
import de.dfki.vsm.players.stickman.animationlogic.AnimationLoader;
import de.dfki.vsm.players.stickman.animationlogic.listener.AnimationListener;
import java.util.ArrayList;

import static de.dfki.vsm.players.ActionPlayer.notifyListenersAboutAction;
import static de.dfki.vsm.players.ActionPlayer.actionEnded;
import de.dfki.vsm.players.server.TCPActionServer;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class StickmanEventAction extends EventAction implements AnimationListener {

	private final Stickman mStickman;
	int mDuration;
	Object mParameter;
	boolean mBlocking;
	public Animation mAnimation;

	public StickmanEventAction(Stickman stickman, int starttime, String name, int dur, Object param, boolean block) {
		Thread.currentThread().setName("Stickman event action " + name);
		mStickman = stickman;
		mStartTime = starttime;
		mName = name;
		mDuration = dur;
		mParameter = param;
		mBlocking = block;
		//timestamps when to call other actions
		mSynchronizedActionTimeMarks = new ArrayList<>();

		mAnimation = AnimationLoader.getInstance().loadEventAnimation(mStickman, mName, mDuration, mBlocking);
		mAnimation.mParameter = mParameter;
	}

	@Override
	public void update(String animationId) {
//		mStickman.mLogger.info("Event Action (" + mID + ") that holds " + a.getName() + " (" + a.mID + ") got update ...");
//		mStickman.mLogger.info("\twaiting for update from animation with id " + mAnimation.mID);

		if (animationId.equalsIgnoreCase(mAnimation.mID)) {
			//mStickman.mLogger.info("\tsuccessfully ended ...");
			mActionEndSync.release();
		}

//		if (a.equals(mAnimation)) {
//			mActionEndSync.release();
//		}
	}

	@Override
	public void run() {
		try {
			notifyListenersAboutAction(this, ActionListener.STATE.ACTION_STARTED);
			//mStickman.mLogger.severe("Action " + mName + " started");

			if (mAnimation == null) {
				//mStickman.mLogger.severe("animation" + mName + " is not known by Stickman ...");
				notifyListenersAboutAction(this, ActionListener.STATE.ACTION_UNKNOWN);
			} else {
				// Here we have 2 possibilities 
				// 1) Send command over tcp connection
				// 2) api call

				//mStickman.mLogger.info("Action " + mName + " play");
				// TCP Cmd EventActionPlayer.getInstance().sendCmd(mAnimation.toString());
				mStickman.playAnimation(mAnimation);

				// tell Stickman to update Action about the animation status
				//mStickman.addListener(this);
				TCPActionServer.getInstance().addListener(this);

				// wait for action end   
				mActionEndSync.acquire();

				//mStickman.removeListener(this);
				TCPActionServer.getInstance().removeListener(this);
			}
			//mStickman.mLogger.info("\ttelling action player event action (" + mID + ") has ended ...");

			notifyListenersAboutAction(this, ActionListener.STATE.ACTION_FINISHED);

			// notify Action Player
			actionEnded(this);

		} catch (InterruptedException ex) {
			mStickman.mLogger.warning("Action " + mName + " got interrupted");
		}
	}
}
