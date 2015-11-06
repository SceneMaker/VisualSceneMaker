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
import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class RightHand extends BodyPart {

	RightForeArm mRightForeArm;

	public RightHand(RightForeArm rfa) {
		mRightForeArm = rfa;
		mLength = 10;
		mSize = new Dimension(mLength, mLength);

		mColor = new Color(80, 80, 80);
		mStroke = new BasicStroke(2.5f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);

		setDefaulRotation(-30);

		init();
	}

	@Override
	public void createShape() {
		mStart = mRightForeArm.getHandStartPosition();
		mEnd = new Point(mStart.x, mStart.y + mLength);

		clearDrawObjects();

		GeneralPath gp = new GeneralPath();
		gp.moveTo(mStart.x, mStart.y);
		gp.lineTo(mStart.x + 5, mStart.y);
		gp.moveTo(mStart.x, mStart.y);
		gp.lineTo(mEnd.x, mEnd.y);
		gp.moveTo(mStart.x + 1, mStart.y);
		gp.lineTo(mEnd.x + 4, mEnd.y - 2f);
		gp.moveTo(mStart.x - 1, mStart.y);
		gp.lineTo(mEnd.x - 3, mEnd.y - 2f);

		addToDrawObjects(gp);
	}

	@Override
	public void calculate(int step) {
		createShape();

		AffineTransform t = new AffineTransform();
		// flip hand when rotation is more than 60 degrees
		if (mRotation  < -60) {
			t.scale(-1.0, 1.0);
			t.translate(-mStart.x * 2, 0);
		}
		
		t.rotate(Math.toRadians(mRotation), mStart.x, mStart.y);
		for (GeneralPath g : mGraphicPaths) {
			g.transform(t);
		}
	}
}
