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

	public EventAnimation(Stickman sm, int duration, boolean block) {
		super(sm, duration, block);
		setName(sm.mName + "'s Event Animation " + getClass().getSimpleName());
	}

	public void playEventAnimationPart() {
		mAnimator = new Animator(mStickman, this, mAnimationPart, mWTS);

		try {
			mAnimationPartStart.acquire();
		} catch (InterruptedException ex) {
			mStickman.mLogger.severe(ex.getMessage());
		}
	}
}