/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players.stickman.animation.gesture;

import de.dfki.vsm.players.stickman.Stickman;
import de.dfki.vsm.players.stickman.animationlogic.Animation;
import de.dfki.vsm.players.stickman.animationlogic.AnimationContent;
import java.util.ArrayList;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class WaveLeft extends Animation {

	public WaveLeft(Stickman sm, int duration, boolean block) {
		super(sm, duration, block);
	}

	@Override
	public void playAnimation() {
		int rotationUnit = 10;

		// 50 is the sum of all animation parts
		float timeFactor = (mDuration > 0) ? mDuration / 50 : 1;

		// bring upper arm and fore arm in position
		mAnimationPart = new ArrayList<>();
		mAnimationPart.add(new AnimationContent(mStickman.mLeftUpperArm, "rotate", rotationUnit));
		mAnimationPart.add(new AnimationContent(mStickman.mLeftForeArm, "rotate", rotationUnit * 16));
		mAnimationPart.add(new AnimationContent(mStickman.mLeftHand, "rotate", rotationUnit * 16));
		playAnimationPart(200);

		pauseAnimation(200);

		// wave right
		mAnimationPart = new ArrayList<>();
		mAnimationPart.add(new AnimationContent(mStickman.mLeftForeArm, "rotate", -rotationUnit * 4));
		mAnimationPart.add(new AnimationContent(mStickman.mLeftHand, "rotate", -rotationUnit * 4));
		playAnimationPart(100);

		// wave left
		mAnimationPart = new ArrayList<>();
		mAnimationPart.add(new AnimationContent(mStickman.mLeftForeArm, "rotate", rotationUnit * 4));
		mAnimationPart.add(new AnimationContent(mStickman.mLeftHand, "rotate", rotationUnit * 4));
		playAnimationPart(100);

		pauseAnimation(200);

		// go back in the default position
		mAnimationPart = new ArrayList<>();
		mAnimationPart.add(new AnimationContent(mStickman.mLeftUpperArm, "rotate", -rotationUnit));
		mAnimationPart.add(new AnimationContent(mStickman.mLeftForeArm, "rotate", -rotationUnit * 16));
		mAnimationPart.add(new AnimationContent(mStickman.mLeftHand, "rotate", -rotationUnit * 16));
		playAnimationPart(200);
	}
}
