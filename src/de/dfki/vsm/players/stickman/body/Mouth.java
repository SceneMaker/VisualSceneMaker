package de.dfki.vsm.players.stickman.body;

import de.dfki.vsm.players.stickman.Stickman;
import de.dfki.vsm.players.stickman.animationlogic.Animator;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.geom.GeneralPath;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class Mouth extends BodyPart {

	public static enum SHAPE {

		DEFAULT, SMILE, SAD, O
	};

	Head mHead;

	public Mouth.SHAPE mShape = Mouth.SHAPE.DEFAULT;

	public Mouth(Head head) {
		mHead = head;
		mLength = 20;
		mSize = new Dimension(mLength * 2, 5);
		mDefaultRotationPoint = mHead.mDefaultRotationPoint;
		mColor = new Color(mHead.mStickman.mType == Stickman.TYPE.FEMALE ? 64 : 32, 0, 0, 128);

		init();
	}

	@Override
	public void setShape(String s) {
		Mouth.SHAPE shape = Mouth.SHAPE.valueOf(s);
		mShape = (shape != null) ? shape : Mouth.SHAPE.DEFAULT;
	}

	@Override
	public void resetShape() {
		mShape = Mouth.SHAPE.DEFAULT;
	}

	@Override
	public void createShape() {
		mStart = mHead.getMouthPostion();
		mEnd = new Point(mStart.x + mLength / 2, mStart.y);

		double movement;

		clearDrawObjects();
		GeneralPath gp = new GeneralPath();

		switch (mShape) {
			case DEFAULT:
				gp.moveTo(mStart.x - mLength / 2, mStart.y);
				gp.quadTo(mStart.x, mStart.y + 1, mEnd.x, mEnd.y);
				break;

			case SMILE:
				movement = mLength / 2 + (Animator.sMAX_ANIM_STEPS - mShapeAnimationStep) / 3;

				gp.moveTo(mStart.x - mLength / 2 - movement, mStart.y - mLength / 2 - movement / 4 + 5);
				gp.quadTo(mStart.x, mStart.y + movement, mEnd.x + movement, mStart.y - mLength / 2 - movement / 4 + 5);
				break;

			case SAD:
				movement = mLength / 2 + (Animator.sMAX_ANIM_STEPS - mShapeAnimationStep) / 2;

				gp.moveTo(mStart.x - mLength / 2 - movement / 2, mStart.y - mLength / 2 + movement / 2);
				gp.quadTo(mStart.x, mStart.y - movement, mEnd.x + movement / 2, mStart.y - mLength / 2 + movement / 2);
				break;

			case O:
				gp.moveTo(mStart.x - mLength / 2, mStart.y);
				gp.quadTo(mStart.x, mStart.y - mLength / 2, mEnd.x, mStart.y);
				gp.quadTo(mStart.x, mStart.y + mLength / 2, mStart.x - mLength / 2, mStart.y);
				break;
		}

		addToDrawObjects(gp);
	}
}
