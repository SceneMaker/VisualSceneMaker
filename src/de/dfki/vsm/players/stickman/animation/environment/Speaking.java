/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players.stickman.animation.environment;

import de.dfki.vsm.players.stickman.Stickman;
import de.dfki.vsm.players.stickman.animation.Animation;
import de.dfki.vsm.players.stickman.animation.BodyAnimation;
import de.dfki.vsm.players.stickman.environment.SpeechBubble;
import java.util.ArrayList;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class Speaking extends Animation {

	public Speaking(Stickman sm, int duration, boolean block) {
		super(sm, duration, block);
	}

	@Override
	public void playAnimation() {
		// smile
		mAnimationPart = new ArrayList<>();
		mAnimationPart.add(new BodyAnimation(mStickman.mSpeechBubble, "shape", SpeechBubble.SHAPE.SPEAK.name()));
		playAnimationPart(mDuration);
		
		mAnimationPart.add(new BodyAnimation(mStickman.mSpeechBubble, "shape", SpeechBubble.SHAPE.DEFAULT.name()));
		playAnimationPart(20);

	}
}
