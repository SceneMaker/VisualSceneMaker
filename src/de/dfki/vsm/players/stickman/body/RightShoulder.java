package de.dfki.vsm.players.stickman.body;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class RightShoulder extends BodyPart {

	Body mBody;

	int mShoulderLength = 15;
	Dimension mSize = new Dimension(mShoulderLength, mShoulderLength);

	Point mStart;
	Point mEnd;

	GeneralPath mShoulder;

	public RightShoulder(Body body) {
		mBody = body;

		mDefaultRotation = 70;
		mRotation = mDefaultRotation;
		mToDegree = mDefaultRotation;
		mRotationStep = 0.0f;

		init();

		calculate(0);
	}

	public Point getRightShoulderEndPosition() {
		return (mShoulder != null) ? new Point((int) mShoulder.getCurrentPoint().getX(), (int) mShoulder.getCurrentPoint().getY()) : new Point(0, 0);
	}

	@Override
	public void calculate(int step) {
		mStart = mBody.getRightArmStartPostion();
		mEnd = new Point(mStart.x, mStart.y + mShoulderLength);

		mShoulder = new GeneralPath();
		mShoulder.moveTo(mStart.x, mStart.y + 2);
		mShoulder.quadTo(mStart.x, (mStart.y + mEnd.y) / 2, mEnd.x, mEnd.y);

		AffineTransform t = new AffineTransform();
		t.rotate(Math.toRadians(mRotation), mStart.x, mStart.y);
		mShoulder.transform(t);
	}

	@Override
	protected void paintComponent(Graphics g) {
		super.paintComponent(g);

		//create();
		Graphics2D g2 = (Graphics2D) g;
		g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		// draw outlines
		g2.setColor(new Color(80, 80, 80));
		g2.setStroke(new BasicStroke(3, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));

		g2.draw(mShoulder);
	}
}
