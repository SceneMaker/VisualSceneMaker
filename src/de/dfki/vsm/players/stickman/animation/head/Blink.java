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
public class Blink extends Animation {

	public Blink(Stickman sm, int duration, boolean block) {
		super(sm, duration, block);
	}

	@Override
	public void playAnimation() {
		mAnimationPart = new ArrayList<>();
		mAnimationPart.add(new BodyAnimation(mStickman.mLeftEye, "shape", "BLINK"));
		mAnimationPart.add(new BodyAnimation(mStickman.mRightEye, "shape", "BLINK"));
		playAnimationPart(20);
		
		pauseAnimation(300);

		// blink up
		mAnimationPart = new ArrayList<>();
		mAnimationPart.add(new BodyAnimation(mStickman.mLeftEye, "shape", "DEFAULT"));
		mAnimationPart.add(new BodyAnimation(mStickman.mRightEye, "shape", "DEFAULT"));
		playAnimationPart(20);
	}
}
