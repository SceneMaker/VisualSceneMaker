/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players.stickman.body;

import de.dfki.vsm.players.stickman.Stickman;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.geom.GeneralPath;
import javax.swing.JComponent;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class Body extends JComponent {

	Neck mNeck;

	Dimension mSize = new Dimension(120, 300);
	int mHalfSizeX = mSize.width / 2;
	int mHalfSizeY = mSize.height / 2;
	int mDrawOffset = 20;
	Point mStart;

	Color mFemaleColor = new Color(154, 83, 198, 240);
	Color mMaleColor = new Color(14, 134, 122, 240);
	public Color mColor  = mFemaleColor;

	GeneralPath mFemaleBodyFront, mFemaleBodyLeft, mFemaleBodyRight;
	GeneralPath mMaleBodyFront, mMaleBodyLeft, mMaleBodyRight;

	public Body(Neck neck) {
		mNeck = neck;
		mStart = mNeck.getBodyStartPosition();
		mColor = (mNeck.mHead.mStickman.mType == Stickman.TYPE.FEMALE) ? mFemaleColor : mMaleColor;
		init();
	}

	private void init() {
		setLayout(null);
		setSize(mSize);
	}

	private void calculate() {
		mStart = mNeck.getBodyStartPosition();

		mFemaleBodyFront = new GeneralPath();
		mFemaleBodyFront.moveTo(mStart.x, mStart.y);
		mFemaleBodyFront.quadTo(mStart.x, mHalfSizeY + mDrawOffset, mStart.x + mHalfSizeX, mSize.height + 10);

		mFemaleBodyFront.curveTo(mStart.x + mHalfSizeX - 40, mSize.height - 10, mStart.x - mHalfSizeX + 40, mSize.height + 20, mStart.x - mHalfSizeX, mSize.height);

		//mFemaleBodyFront.lineTo(mStart.x - mHalfSizeX, mSize.height);
		mFemaleBodyFront.quadTo(mStart.x, mHalfSizeY + mDrawOffset, mStart.x, mStart.y);

		mFemaleBodyLeft = new GeneralPath();
		mFemaleBodyLeft.moveTo(mStart.x, mStart.y);
		mFemaleBodyLeft.quadTo(mStart.x + mDrawOffset, mSize.height / 3 * 2, mStart.x, mSize.height);
		mFemaleBodyLeft.lineTo(mStart.x - mHalfSizeX, mSize.height);
		mFemaleBodyLeft.quadTo(mStart.x, mHalfSizeY + mDrawOffset, mStart.x, mStart.y);

		mFemaleBodyRight = new GeneralPath();
		mFemaleBodyRight.moveTo(mStart.x, mStart.y);
		mFemaleBodyRight.quadTo(mStart.x - mDrawOffset, mSize.height / 3 * 2, mStart.x, mSize.height);
		mFemaleBodyRight.lineTo(mStart.x + mHalfSizeX, mSize.height);
		mFemaleBodyRight.quadTo(mStart.x, mHalfSizeY + mDrawOffset, mStart.x, mStart.y);

		mMaleBodyFront = new GeneralPath();
		mMaleBodyFront.moveTo(mStart.x, mStart.y);
		mMaleBodyFront.quadTo(mStart.x, mHalfSizeY + mDrawOffset, mStart.x + mHalfSizeX - mDrawOffset, mSize.height);
		mMaleBodyFront.lineTo(mStart.x - mHalfSizeX + mDrawOffset, mSize.height);
		mMaleBodyFront.quadTo(mStart.x, mHalfSizeY + mDrawOffset, mStart.x, mStart.y);

		mMaleBodyLeft = new GeneralPath();
		mMaleBodyLeft.moveTo(mStart.x, mStart.y);
		mMaleBodyLeft.quadTo(mStart.x + mDrawOffset, mSize.height / 3 * 2, mStart.x, mSize.height);
		mMaleBodyLeft.lineTo(mStart.x - mHalfSizeX + mDrawOffset, mSize.height);
		mMaleBodyLeft.quadTo(mStart.x, mHalfSizeY + mDrawOffset, mStart.x, mStart.y);

		mMaleBodyRight = new GeneralPath();
		mMaleBodyRight.moveTo(mStart.x, mStart.y);
		mMaleBodyRight.quadTo(mStart.x - mDrawOffset, mSize.height / 3 * 2, mStart.x, mSize.height);
		mMaleBodyRight.lineTo(mStart.x + mHalfSizeX - mDrawOffset, mSize.height);
		mMaleBodyRight.quadTo(mStart.x, mHalfSizeY + mDrawOffset, mStart.x, mStart.y);
	}

	public Point getLeftArmStartPostion() {
		return new Point(mStart.x + 1, mStart.y);
	}

	public Point getRightArmStartPostion() {
		return new Point(mStart.x - 1, mStart.y);
	}

	public Point getLeftLegStartPostion() {
		if (mNeck.mHead.mStickman.mOrientation == Stickman.ORIENTATION.LEFT) {
			return new Point(mStart.x + mHalfSizeX - mDrawOffset, mSize.height);
		} else {
			return new Point(mStart.x + mHalfSizeX - mDrawOffset,
			  (mNeck.mHead.mStickman.mType == Stickman.TYPE.FEMALE) ? mSize.height + 3 : mSize.height);
		}
	}

	public Point getRightLegStartPostion() {
		if (mNeck.mHead.mStickman.mOrientation == Stickman.ORIENTATION.RIGHT) {
			return new Point(mStart.x, mSize.height);
		} else {
			return new Point(mStart.x - mHalfSizeX + mDrawOffset,
			  (mNeck.mHead.mStickman.mType == Stickman.TYPE.FEMALE) ? mSize.height + 5 : mSize.height);
		}
	}

	private void paintLeftOrientation(Graphics2D g2) {
		if (mNeck.mHead.mStickman.mType == Stickman.TYPE.FEMALE) {
			g2.fill(mFemaleBodyLeft);
		} else {
			g2.fill(mMaleBodyLeft);
		}

		// draw outlines
		g2.setColor(g2.getColor().darker());
		g2.setStroke(new BasicStroke(2));

		if (mNeck.mHead.mStickman.mType == Stickman.TYPE.FEMALE) {
			g2.draw(mFemaleBodyLeft);
		} else {
			g2.draw(mMaleBodyLeft);
		}
	}

	private void paintRightOrientation(Graphics2D g2) {
		if (mNeck.mHead.mStickman.mType == Stickman.TYPE.FEMALE) {
			g2.fill(mFemaleBodyRight);
		} else {
			g2.fill(mMaleBodyRight);
		}

		// draw outlines
		g2.setColor(g2.getColor().darker());
		g2.setStroke(new BasicStroke(2));

		if (mNeck.mHead.mStickman.mType == Stickman.TYPE.FEMALE) {
			g2.draw(mFemaleBodyRight);
		} else {
			g2.draw(mMaleBodyRight);
		}
	}

	private void paintFrontOrientation(Graphics2D g2) {
		if (mNeck.mHead.mStickman.mType == Stickman.TYPE.FEMALE) {
			g2.fill(mFemaleBodyFront);
		} else {
			g2.fill(mMaleBodyFront);
		}

		// draw outlines
		g2.setColor(g2.getColor().darker());
		g2.setStroke(new BasicStroke(3, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));

		if (mNeck.mHead.mStickman.mType == Stickman.TYPE.FEMALE) {
			g2.draw(mFemaleBodyFront);
		} else {
			g2.draw(mMaleBodyFront);
		}
	}

	@Override
	protected void paintComponent(Graphics g) {
		super.paintComponent(g); //To change body of generated methods, choose Tools | Templates.

		calculate();

		Graphics2D g2 = (Graphics2D) g;
		g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

		if (mNeck.mHead.mStickman.mType == Stickman.TYPE.FEMALE) {
			g2.setColor(mFemaleColor);
		} else {
			g2.setColor(mMaleColor);
		}

		if (mNeck.mHead.mStickman.mOrientation == Stickman.ORIENTATION.LEFT) {
			paintLeftOrientation(g2);
		} else if (mNeck.mHead.mStickman.mOrientation == Stickman.ORIENTATION.RIGHT) {
			paintRightOrientation(g2);
		} else {
			paintFrontOrientation(g2);
		}
	}

}
