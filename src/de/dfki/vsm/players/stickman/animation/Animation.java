/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players.stickman.animation;

import de.dfki.vsm.players.stickman.Stickman;
import java.util.ArrayList;
import java.util.concurrent.Semaphore;

/**
 *
 * @author Patrick Gebhard
 *
 */
public abstract class Animation extends Thread {

	public ArrayList<BodyAnimation> mAnimationPart = new ArrayList<>();
	public Semaphore mAnimationPartStart = new Semaphore(0);
	public Semaphore mAnimationStart = new Semaphore(1);
	public Animator mAnimator;
	public AnimationPause mAnimationPause;
	public Stickman mStickman;
	public boolean mBlocking = false;
	public int mDuration = -1;
	public String mID;

	public Animation(Stickman sm, int duration, boolean block) {
		setName(sm.mName + "'s Animation " + getClass().getSimpleName());
		mStickman = sm;
		mID = mStickman.getID();
		mBlocking = block;
		mDuration = duration;
	}

	public void waitForClearance() {
		mStickman.mAnimationScheduler.introduce(this);

		// block this animation for animation - AnimationSheduler will unblock 
		try {
			mAnimationStart.acquire(1);
		} catch (InterruptedException ex) {
			mStickman.mLogger.severe(ex.getMessage());
		}

		// tell Stickman this animation has been scheduled and a next one can come
		mStickman.mAnimationLaunchControl.release();
	}

	private void play() {
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
		waitForClearance();

		play();

		finalizeAnimation();
	}

	public String toSting() {
		return getClass().getSimpleName() + ", " + getName();
	}
}