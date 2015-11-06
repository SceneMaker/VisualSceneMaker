package de.dfki.vsm.players.stickman.body;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.geom.GeneralPath;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class RightEyebrow extends BodyPart {

	public static enum SHAPE {

		DEFAULT
	};

	Head mHead;
	public RightEyebrow.SHAPE mShape = RightEyebrow.SHAPE.DEFAULT;

	public RightEyebrow(Head head) {
		mHead = head;
		mLength = 16;
		mSize = new Dimension(mLength, 5);
		mDefaultRotationPoint = mHead.mDefaultRotationPoint;
		mColor = new Color(0, 0, 0, 64);
		mStroke = new BasicStroke(2.5f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);

		init();
	}

	@Override
	public void setShape(String s) {
		RightEyebrow.SHAPE shape = RightEyebrow.SHAPE.valueOf(s);
		mShape = (shape != null) ? shape : RightEyebrow.SHAPE.DEFAULT;
	}

	@Override
	public void resetShape() {
		mShape = RightEyebrow.SHAPE.DEFAULT;
	}

	@Override
	public void createShape() {
		mStart = mHead.getRightEyebrowPostion();
		mEnd = new Point(mStart.x - mLength, mStart.y);

		clearDrawObjects();
		GeneralPath gp = new GeneralPath();

		switch (mShape) {
			case DEFAULT:
				gp = new GeneralPath();
				gp.moveTo(mStart.x, mStart.y);
				gp.quadTo((mStart.x + mEnd.x) / 2, mStart.y - 3, mEnd.x, mEnd.y);
				break;
		}

		addToDrawObjects(gp);
	}

}
