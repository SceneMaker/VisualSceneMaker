/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players.stickman.animationlogic;

import de.dfki.vsm.players.stickman.Stickman;
import java.util.ArrayList;
import java.util.concurrent.Semaphore;

/**
 *
 * @author Patrick Gebhard
 *
 */
public abstract class Animation extends Thread {

	public ArrayList<AnimationContent> mAnimationPart = new ArrayList<>();
	public Semaphore mAnimationPartStart = new Semaphore(0);
	public Semaphore mAnimationStart = new Semaphore(1);
	public Animator mAnimator;
	public AnimationPause mAnimationPause;
	public Stickman mStickman;
	public boolean mBlocking = false;
	public int mDuration = -1;
	public String mID;
	public Object mParameter = "";

	public Animation(Stickman sm, int duration, boolean block) {
		setName(sm.mName + "'s Animation " + getClass().getSimpleName());
		mStickman = sm;
		mID = mStickman.getID(); // default ID;
		mBlocking = block;
		mDuration = duration;
	}

	public void setParameter(Object p) {
		mParameter = p;
	}

	public void setID(String id) {
		mID = id;
	}

	public void waitForClearance() {
		//mStickman.mLogger.info("Introducing " + this.toString() + " to Animationcheduler");
		mStickman.mAnimationScheduler.introduce(this);
		//mStickman.mLogger.info("\tdone.");

		// block this animation for animation - AnimationSheduler will unblock 
		try {
			//mStickman.mLogger.info("Block - give Animation Scheduler control when to start the animation" + this.toString());
			mAnimationStart.acquire(1);
		} catch (InterruptedException ex) {
			mStickman.mLogger.severe(ex.getMessage());
		}

		// tell Stickman this animation has been scheduled and a next one can come
		//mStickman.mLogger.info("Releasing launch for next animations");
		mStickman.mAnimationLaunchControl.release();
	}

	public void play() {
		// wait until AnimationScheduler says go!
		try {
			mAnimationStart.acquire(1);
		} catch (InterruptedException ex) {
			mStickman.mLogger.severe(ex.getMessage());
		}

		playAnimation();
	}

	public void playAnimation() {
		// place animation code here
	}

	public void playAnimationPart(int duration) {
		mAnimator = new Animator(mStickman, this, mAnimationPart, duration);

		try {
			mAnimationPartStart.acquire();
		} catch (InterruptedException ex) {
			mStickman.mLogger.severe(ex.getMessage());
		}

	}

	public void pauseAnimation(int duration) {
		mAnimationPause = new AnimationPause(mStickman, this, duration);

		try {
			mAnimationPartStart.acquire();
		} catch (InterruptedException ex) {
			mStickman.mLogger.severe(ex.getMessage());
		}
	}

	public void finalizeAnimation() {
		//mStickman.mLogger.info(mStickman.mName + "'s Animation " + getClass().getSimpleName() + " with id " + mID + " has ended - notify Listeners!");
		// unblock AnimationScheduler if animation is a blocking animation
		if (mBlocking) {
			//mStickman.mLogger.info("unblocking AnimationScheduler");
			mStickman.mAnimationScheduler.proceed(this);
		} else {
			mStickman.mAnimationScheduler.removeAnimation(this);
		}
		// send event that Animation is ended
		mStickman.notifyListeners(this);
	}

	@Override
	public void run() {
		//mStickman.mLogger.info(mStickman.mName + "'s Animation " + getClass().getSimpleName() + " wait for clearance.");
		waitForClearance();

		//mStickman.mLogger.info(mStickman.mName + "'s Animation " + getClass().getSimpleName() + " play.");
		play();

		//mStickman.mLogger.info(mStickman.mName + "'s Animation " + getClass().getSimpleName() + " finalize.");
		finalizeAnimation();
	}

	public String toString() {
		return getClass().getSimpleName() + ", " + getName();
	}
}
