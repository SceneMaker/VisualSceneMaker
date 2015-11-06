/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
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
public class LeftEyebrow extends BodyPart {

	public static enum SHAPE {

		DEFAULT
	};

	Head mHead;
	public LeftEyebrow.SHAPE mShape = LeftEyebrow.SHAPE.DEFAULT;

	public LeftEyebrow(Head head) {
		mHead = head;
		mLength = 16;
		mSize = new Dimension(mLength, mLength);
		mDefaultRotationPoint = mHead.mDefaultRotationPoint;
		mColor = new Color(0, 0, 0, 64);
		mStroke = new BasicStroke(2.5f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);

		init();
	}

	@Override
	public void setShape(String s) {
		SHAPE shape = LeftEyebrow.SHAPE.valueOf(s);
		mShape = (shape != null) ? shape : LeftEyebrow.SHAPE.DEFAULT;
	}

	@Override
	public void resetShape() {
		mShape = LeftEyebrow.SHAPE.DEFAULT;
	}

	@Override
	public void createShape() {
		mStart = mHead.getLeftEyebrowPostion();
		mEnd = new Point(mStart.x + mLength, mStart.y);

		clearDrawObjects();
		GeneralPath gp = new GeneralPath();

		switch (mShape) {
			case DEFAULT:
				gp.moveTo(mStart.x, mStart.y);
				gp.quadTo((mStart.x + mEnd.x) / 2, mStart.y - 3, mEnd.x, mEnd.y);
				break;
		}

		addToDrawObjects(gp);
	}
}
