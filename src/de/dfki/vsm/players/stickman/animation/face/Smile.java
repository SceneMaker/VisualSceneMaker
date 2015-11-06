package de.dfki.vsm.players.stickman.animation.face;

import de.dfki.vsm.players.stickman.Stickman;
import de.dfki.vsm.players.stickman.animation.Animation;
import de.dfki.vsm.players.stickman.animation.BodyAnimation;
import java.util.ArrayList;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class Smile extends Animation {

	public Smile(Stickman sm, int duration, boolean block) {
		super(sm, duration, block);
	}

	@Override
	public void playAnimation() {
		// smile
		mAnimationPart = new ArrayList<>();
		mAnimationPart.add(new BodyAnimation(mStickman.mMouth, "shape", "SMILE"));
		
		playAnimationPart(20);
		
		pauseAnimation(1200);
		
		// no smile
		mAnimationPart = new ArrayList<>();
		mAnimationPart.add(new BodyAnimation(mStickman.mMouth, "shape", "DEFAULT"));
		
		playAnimationPart(20);
	}
}
