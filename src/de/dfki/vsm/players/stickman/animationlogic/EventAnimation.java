/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players.stickman.animationlogic;

import de.dfki.vsm.model.scenescript.SceneUttr;
import de.dfki.vsm.players.action.sequence.WordTimeMarkSequence;
import de.dfki.vsm.players.stickman.Stickman;
import java.util.List;

/**
 *
 * @author Patrick Gebhard
 *
 */
public abstract class EventAnimation extends Animation {

	public SceneUttr mUtterance;
	public List<Long> mTimepoints;
	public WordTimeMarkSequence mWTS;

	public EventAnimation(Stickman sm, int duration, WordTimeMarkSequence wts, boolean block) {
		super(sm, duration, block);
		mWTS = wts;
		setName(sm.mName + "'s Event Animation " + getClass().getSimpleName());
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

	public void playEventAnimationPart() {
		mAnimator = new Animator(mStickman, this, mAnimationPart, mWTS);

		try {
			mAnimationPartStart.acquire();
		} catch (InterruptedException ex) {
			mStickman.mLogger.severe(ex.getMessage());
		}
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

	public String toString() {
		return getClass().getSimpleName() + ", " + getName();
	}
}
