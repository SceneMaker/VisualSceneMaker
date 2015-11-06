package de.dfki.vsm.players.stickman.animation;

import de.dfki.vsm.players.stickman.Stickman;
import de.dfki.vsm.players.stickman.body.BodyPart;
import static java.lang.Thread.sleep;
import java.util.ArrayList;
import java.util.concurrent.Semaphore;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class Animator {

	public static final int sMAX_ANIM_STEPS = 20;
	public int mCurrentStep = sMAX_ANIM_STEPS;
	private final Stickman mStickman;
	private final Animation mAnimation;
	private ArrayList<BodyAnimation> mAnimationComponents = new ArrayList<>();
	private String mDescription = "";
	private int mRenderPauseDuration = 0;
	public Semaphore mRenderingPause = new Semaphore(0);

	//private long mPreparationTime = 0;
	public Animator(Stickman sm, Animation a, ArrayList<BodyAnimation> animComps) {
		mStickman = sm;
		mAnimation = a;
		mAnimationComponents = animComps;
		mDescription = mAnimation.getClass().getSimpleName() + " (" + mAnimation.mID + "), " + mAnimation.toString();
		mRenderPauseDuration = 40; // 40 milliseconds equals 25fps - resulting that by default an animation takes 500ms

		render();
	}

	public Animator(Stickman sm, Animation a, ArrayList<BodyAnimation> animComps, int duration) {
		//mPreparationTime = System.currentTimeMillis();
		mStickman = sm;
		mAnimation = a;
		mAnimationComponents = animComps;
		mDescription = mAnimation.getClass().getSimpleName() + " (" + mAnimation.mID + "), " + mAnimation.toString();

		mRenderPauseDuration = new Float(duration / sMAX_ANIM_STEPS).intValue();
		mRenderPauseDuration = (mRenderPauseDuration < 1) ? 1 : mRenderPauseDuration; // minimum delay is 1 millisecond
		
		render();
	}

	private void render() {
		mCurrentStep = sMAX_ANIM_STEPS;
		while (mCurrentStep > 0) {
			//for (mCurrentStep = sMAX_ANIM_STEPS; mCurrentStep > 0; mCurrentStep--) {
			if (mCurrentStep == sMAX_ANIM_STEPS) {
				//mStickman.mLogger.info("\t\t\tpreparing " + mDescription);
				// prepare animation components
				mAnimationComponents.stream().forEach((ba) -> {
					BodyPart bodypart = ba.mBodyPart;
					String action = ba.mAction;
					int param = ba.mParam;
					String paramString = ba.mParamString;
					if (action.equalsIgnoreCase("rotate")) {
						bodypart.setRotation(param);
					}
					if (action.equalsIgnoreCase("tilt")) {
						bodypart.setTilt(param);
					}
					if (action.equalsIgnoreCase("translate")) {
						bodypart.setTranslation(param);
					}
					if (action.equalsIgnoreCase("shape")) {
						bodypart.setShape(paramString);
					}
				});
			}

			if (mCurrentStep > 1) {
				for (BodyAnimation ba : mAnimationComponents) {
					BodyPart bodypart = ba.mBodyPart;
					String action = ba.mAction;

					if (action.equalsIgnoreCase("rotate")) {
						bodypart.calculateRotation(mCurrentStep);
					}

					if (action.equalsIgnoreCase("tilt")) {
						bodypart.calculateRotation(mCurrentStep);
					}

					if (action.equalsIgnoreCase("translate")) {
						bodypart.calculateTranslation(mCurrentStep);
					}

					if (action.equalsIgnoreCase("shape")) {
						bodypart.calculateShape(mCurrentStep);
					}
				}

				mStickman.repaint();

				new WaitThread(mRenderPauseDuration).start();
				// block this until WaitThread will unblock 
				try {
					mRenderingPause.acquire(1);
				} catch (InterruptedException ex) {
					mStickman.mLogger.severe(ex.getMessage());
				}
			}

			if (mCurrentStep == 1) {
				for (BodyAnimation ba : mAnimationComponents) {
					String action = ba.mAction;
					BodyPart bodypart = ba.mBodyPart;

					if (action.equalsIgnoreCase("rotate")) {
						bodypart.resetRotation();
					}

					if (action.equalsIgnoreCase("tilt")) {
						bodypart.resetRotation();
					}

					if (action.equalsIgnoreCase("translate")) {
						bodypart.resetTranslation();
					}
				}

				mStickman.repaint();

				new WaitThread(mRenderPauseDuration).start();
				// block this until WaitThread will unblock 
				try {
					mRenderingPause.acquire(1);
				} catch (InterruptedException ex) {
					mStickman.mLogger.severe(ex.getMessage());
				}

				mAnimation.mAnimationPartStart.release();
				return;
			}

			mCurrentStep -= 1;
		}
	}

	private class WaitThread extends Thread {

		int mSleepTime = 0;

		public WaitThread(int time) {
			mSleepTime = time;
		}

		@Override
		public void run() {
			// directly go to sleep
			try {
				
				sleep(mSleepTime, 0);
			} catch (InterruptedException ex) {
				mStickman.mLogger.severe(ex.getMessage());
			}
			// release sempahore
			mRenderingPause.release();
		}
	}
}