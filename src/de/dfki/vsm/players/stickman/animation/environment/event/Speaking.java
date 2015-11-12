/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players.stickman.animation.environment.event;

import de.dfki.vsm.players.action.sequence.WordTimeMarkSequence;
import de.dfki.vsm.players.stickman.Stickman;
import de.dfki.vsm.players.stickman.animation.AnimationContent;
import de.dfki.vsm.players.stickman.animation.EventAnimation;
import de.dfki.vsm.players.stickman.environment.SpeechBubble;
import java.util.ArrayList;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class Speaking extends EventAnimation {

	public Speaking(Stickman sm, WordTimeMarkSequence wts, int duration, boolean block) {
		super(sm, duration, wts, block);
	}

	@Override
	public void playAnimation() {	
		mAnimationPart = new ArrayList<>();
		mAnimationPart.add(new AnimationContent(mStickman.mSpeechBubble, "shape", SpeechBubble.SHAPE.SPEAK.name(), mWTS));
		
		playEventAnimationPart();
		
		mAnimationPart.add(new AnimationContent(mStickman.mSpeechBubble, "shape", SpeechBubble.SHAPE.DEFAULT.name()));
		playAnimationPart(20);

	}
}
