/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players.stickman.animation.head;

import de.dfki.vsm.players.stickman.Stickman;
import de.dfki.vsm.players.stickman.animation.Animation;
import de.dfki.vsm.players.stickman.animation.BodyAnimation;
import java.util.ArrayList;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class TiltLeft extends Animation {

	public TiltLeft(Stickman sm, int duration, boolean block) {
		super(sm, duration, block);
	}

	@Override
	public void playAnimation() {
		int translationUnit = 8;

		// head down
		mAnimationPart = new ArrayList<>();
		// which bodyparts are involved - check dependencies
		mAnimationPart.add(new BodyAnimation(mStickman.mRightEye, "tilt", translationUnit));
		mAnimationPart.add(new BodyAnimation(mStickman.mRightEyebrow, "tilt", translationUnit));
		mAnimationPart.add(new BodyAnimation(mStickman.mLeftEye, "tilt", translationUnit));
		mAnimationPart.add(new BodyAnimation(mStickman.mLeftEyebrow, "tilt", translationUnit));
		mAnimationPart.add(new BodyAnimation(mStickman.mHead, "tilt", translationUnit));
		mAnimationPart.add(new BodyAnimation(mStickman.mMouth, "tilt", translationUnit));

		playAnimationPart(300);
	}
}