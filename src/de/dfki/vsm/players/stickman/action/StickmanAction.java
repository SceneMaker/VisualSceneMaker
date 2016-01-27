package de.dfki.vsm.players.stickman.action;

import de.dfki.vsm.players.ActionPlayer;
import static de.dfki.vsm.players.ActionPlayer.notifyListenersAboutAction;
import de.dfki.vsm.players.action.ActionListener;
import de.dfki.vsm.players.action.Action;
import de.dfki.vsm.players.server.TCPActionServer;
import de.dfki.vsm.players.stickman.Stickman;
import de.dfki.vsm.players.stickman.animationlogic.Animation;
import de.dfki.vsm.players.stickman.animationlogic.AnimationLoader;
import de.dfki.vsm.players.stickman.animationlogic.listener.AnimationListener;
import de.dfki.vsm.runtime.events.AbortionEvent;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLUtilities;
import java.io.ByteArrayOutputStream;
import java.io.UnsupportedEncodingException;

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
		try {
			mAnimation = AnimationLoader.getInstance().loadAnimation(mStickman, mName, mDuration, mBlocking);
		}
		catch (Exception e){
			String msg = "animation " + mName + " is not known by Stickman ...";
			EventDispatcher.getInstance().convey(new AbortionEvent(this, new ClassNotFoundException(msg)));
		}
		if(mAnimation != null ) {
			//Show an error dialog

			mAnimation.mParameter = mParameter;

		}
	}
	
	@Override
	public void update(String animationId) {
		//mStickman.mLogger.info("Action (" + mID + ") - that holds Animation " + mAnimation + " with id (" + mAnimation.mID + ") got update from Animation with ID " + animationId + " ...");
		
//		if (a.equals(mAnimation)) {
//			mActionEndSync.release();
//		}
		if (animationId.equalsIgnoreCase(mAnimation.mID)) {
			//mStickman.mLogger.info("\tsuccessfully ended ...");
			mActionEndSync.release();
		}

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
				if (ActionPlayer.mUseNetwork) {
					ByteArrayOutputStream out = new ByteArrayOutputStream();
					IOSIndentWriter iosw = new IOSIndentWriter(out);
					boolean r = XMLUtilities.writeToXMLWriter(mAnimation, iosw);

					try {
						TCPActionServer.getInstance().sendToClient("StickmanStage", new String(out.toByteArray(), "UTF-8"));
					} catch (UnsupportedEncodingException ex) {
						mStickman.mLogger.warning(ex.getMessage());
					}
					// tell TCPActionServer to update Action about the animation status
					TCPActionServer.getInstance().addListener(this);
				} else {
					mStickman.playAnimation(mAnimation);
					// tell Stickman to update Action about the animation status
					mStickman.addListener(this);
				}

				// wait for action end   
				mActionEndSync.acquire();

				if (ActionPlayer.mUseNetwork) {
					TCPActionServer.getInstance().removeListener(this);
				} else {
					mStickman.removeListener(this);
				}
			}
			//mStickman.mLogger.info("\ttelling action player event action (" + mID + ") has ended ...");

			notifyListenersAboutAction(this, ActionListener.STATE.ACTION_FINISHED);

			// notify Action Player
			mActionPlayer.actionEnded(this);
		} catch (InterruptedException ex) {
			mStickman.mLogger.warning("Action " + mName + " got interrupted");
		}
	}
}
