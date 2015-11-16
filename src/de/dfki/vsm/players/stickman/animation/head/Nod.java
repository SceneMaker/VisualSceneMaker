/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players.stickman.animation.head;

import de.dfki.vsm.players.stickman.Stickman;
import de.dfki.vsm.players.stickman.animationlogic.Animation;
import de.dfki.vsm.players.stickman.animationlogic.AnimationContent;
import java.util.ArrayList;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class Nod extends Animation {

	public Nod(Stickman sm, int duration, boolean block) {
		super(sm, duration, block);
	}

	@Override
	public void playAnimation() {
		int translationUnit = 3;

		// head down
		mAnimationPart = new ArrayList<>();
		mAnimationPart.add(new AnimationContent(mStickman.mRightEyebrow, "translate", translationUnit));
		mAnimationPart.add(new AnimationContent(mStickman.mRightEye, "translate", translationUnit));
		mAnimationPart.add(new AnimationContent(mStickman.mLeftEyebrow, "translate", translationUnit));
		mAnimationPart.add(new AnimationContent(mStickman.mLeftEye, "translate", translationUnit));
		mAnimationPart.add(new AnimationContent(mStickman.mHead, "translate", translationUnit));
		mAnimationPart.add(new AnimationContent(mStickman.mMouth, "translate", translationUnit));
		playAnimationPart(200);

		// head up
		mAnimationPart = new ArrayList<>();
		mAnimationPart.add(new AnimationContent(mStickman.mRightEyebrow, "translate", -translationUnit));
		mAnimationPart.add(new AnimationContent(mStickman.mRightEye, "translate", -translationUnit));
		mAnimationPart.add(new AnimationContent(mStickman.mLeftEyebrow, "translate", -translationUnit));
		mAnimationPart.add(new AnimationContent(mStickman.mLeftEye, "translate", -translationUnit));
		mAnimationPart.add(new AnimationContent(mStickman.mHead, "translate", -translationUnit));
		mAnimationPart.add(new AnimationContent(mStickman.mMouth, "translate", -translationUnit));

		playAnimationPart(200);
	}
}
