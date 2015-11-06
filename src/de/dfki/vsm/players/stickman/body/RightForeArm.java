/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
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
public class RightForeArm extends BodyPart {

	RightUpperArm mUpperArm;
	int mArmLength = 80;
	Dimension mSize = new Dimension(mArmLength, mArmLength);

	Point mStart;
	Point mEnd;

	GeneralPath mArm;

	public RightForeArm(RightUpperArm arm) {
		mUpperArm = arm;

		mDefaultRotation = -20;
		mRotation = mDefaultRotation;
		mToDegree = mDefaultRotation;

		init();

		calculate(0);
	}

	public Point getHandStartPosition() {
		return (mArm != null) ? new Point((int) mArm.getCurrentPoint().getX(), (int) mArm.getCurrentPoint().getY()) : new Point(0, 0);
	}

	@Override
	public void calculate(int step) {
		mStart = mUpperArm.getRightUpperArmEndPosition();
		mEnd = new Point(mStart.x, mStart.y + mArmLength);

		mArm = new GeneralPath();
		mArm.moveTo(mStart.x, mStart.y + 2);
		mArm.quadTo(mStart.x - 5, (mStart.y + mEnd.y) / 2, mEnd.x, mEnd.y);

		AffineTransform t = new AffineTransform();
		t.rotate(Math.toRadians(mDefaultRotation), mStart.x, mStart.y);
		mArm.transform(t);
	}

	@Override
	protected void paintComponent(Graphics g) {
		super.paintComponent(g); //To change body of generated methods, choose Tools | Templates.

		Graphics2D g2 = (Graphics2D) g;
		g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		// draw outlines
		g2.setColor(new Color(80, 80, 80));
		g2.setStroke(new BasicStroke(3, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));

		g2.draw(mArm);
	}
}
