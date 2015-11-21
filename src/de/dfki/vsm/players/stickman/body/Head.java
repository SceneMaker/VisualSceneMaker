package de.dfki.vsm.players.stickman.body;

import de.dfki.vsm.players.stickman.Stickman;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class Head extends BodyPart {

	public Dimension mSize = new Dimension(120, 100);

	public Stickman mStickman;

	int mHalfHeight = mSize.height / 2;
	int mHalfWidth = mSize.width / 2;
	int mEarWidth = 10;
	int mDrawOffset = 10;
	int mXCenterOffset = mEarWidth / 2;
	int mYCenterOffset = mEarWidth / 2;

	GeneralPath mHead, mLeftEar, mRightEar, mFemaleHair, mMaleHair;

	public Head(Stickman sm) {
		mStickman = sm;
		mDefaultRotationPoint = new Point(mSize.width / 2, mSize.height);
		mStroke = new BasicStroke(2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);

		init();

		calculate(0);
	}

	public Point getLeftEyebrowPostion() {
		return new Point(mHalfWidth + 23, mHalfHeight - 16);
	}

	public Point getRightEyebrowPostion() {
		return new Point(mHalfWidth - 11, mHalfHeight - 16);
	}

	public Point getLeftEyePostion() {
		return new Point(mHalfWidth + 32, mHalfHeight - 8);
	}

	public Point getRightEyePostion() {
		return new Point(mHalfWidth - 20, mHalfHeight - 8);
	}

	public Point getMouthPostion() {
		return new Point(mHalfWidth + mEarWidth / 2, mHalfHeight + mDrawOffset * 3);
	}

	public Point getSpeechBubbleStartPosition() {
		return new Point(mHalfWidth + 20, mHalfHeight + 30);
	}

	public Point getNeckStartPosition() {
		return new Point(mSize.width / 2 + mXCenterOffset, mSize.height + mYCenterOffset);
	}

	public void calculate(int step) {
		// head
		mHead = new GeneralPath();
		mHead.moveTo(mEarWidth, mHalfHeight);
		mHead.curveTo(mEarWidth, -mHalfHeight / 5, mSize.width, -mHalfHeight / 5, mSize.width, mHalfHeight);
		mHead.curveTo(mSize.width, 120, mEarWidth, 120, mEarWidth, mHalfHeight);
		AffineTransform t = new AffineTransform();
		t.rotate(Math.toRadians(mRotation), mDefaultRotationPoint.x, mDefaultRotationPoint.y);
		t.translate(0, mTranslation);
		mHead.transform(t);

		//left ear
		mLeftEar = new GeneralPath();
		mLeftEar.moveTo(10, mSize.height / 2 + 10);
		mLeftEar.quadTo(7, mSize.height / 2, 10, mSize.height / 2 - 10);
		mLeftEar.curveTo(0, mSize.height / 2 - 10, 0, mSize.height / 2 + 10, 10, mSize.height / 2 + 10);
		t = new AffineTransform();
		t.rotate(Math.toRadians(mRotation), mDefaultRotationPoint.x, mDefaultRotationPoint.y);
		t.translate(1, 3 + mTranslation);
		mLeftEar.transform(t);

		//right ear
		mRightEar = new GeneralPath();
		mRightEar.moveTo(mSize.width, mSize.height / 2 + 10);
		mRightEar.quadTo(mSize.width + 3, mSize.height / 2, mSize.width, mSize.height / 2 - 10);
		mRightEar.curveTo(mSize.width + 10, mSize.height / 2 - 10, mSize.width + 10, mSize.height / 2 + 10, mSize.width, mSize.height / 2 + 10);
		t = new AffineTransform();
		t.rotate(Math.toRadians(mRotation), mDefaultRotationPoint.x, mDefaultRotationPoint.y);
		t.translate(-1, 3 + mTranslation);
		mRightEar.transform(t);

		// female hair
		mFemaleHair = new GeneralPath();
		mFemaleHair.moveTo(mStart.x, mSize.height + 20);
		// right top locke
		mFemaleHair.quadTo(mEarWidth + 10, mSize.height, mEarWidth, mHalfHeight);
		// top hair
		mFemaleHair.curveTo(mEarWidth + 20, -mHalfHeight / 8, mSize.width - 20, -mHalfHeight / 8, mSize.width, mHalfHeight);
		// left top locke
		mFemaleHair.quadTo(mEarWidth + mSize.width - 20, mSize.height, mSize.width + mEarWidth, mSize.height + 20);
		// left down locke
		mFemaleHair.quadTo(mSize.width - 10, mSize.height + 30, mSize.width - 10, mSize.height + 20);
		// forehead hair
		mFemaleHair.curveTo(mSize.width + 30, -mHalfHeight / 4, mStart.x - 20, -mHalfHeight / 4, mEarWidth + 10, mSize.height + 20);
		// right down locke
		mFemaleHair.quadTo(20, mSize.height + 30, mStart.x, mSize.height + 20);
		// move it upwards a bit
		t = new AffineTransform();
		t.rotate(Math.toRadians(mRotation), mDefaultRotationPoint.x, mDefaultRotationPoint.y);
		t.translate(0, -15 + mTranslation);
		mFemaleHair.transform(t);

		// male hair
		mMaleHair = new GeneralPath();
		mMaleHair.moveTo(mEarWidth, mHalfHeight);
		mMaleHair.quadTo(mHalfWidth - 30, -mHalfHeight / 3, mHalfWidth + 20, mHalfHeight - 30);
		mMaleHair.quadTo((mHalfWidth + 40 + mSize.width) / 2, 0, mSize.width, mHalfHeight);
		mMaleHair.curveTo(mSize.width, -mHalfHeight / 2, mEarWidth, -mHalfHeight / 2, mEarWidth, mHalfHeight);
		// move it downwards a bit
		t = new AffineTransform();
		t.rotate(Math.toRadians(mRotation), mDefaultRotationPoint.x, mDefaultRotationPoint.y);
		t.translate(0, 2 + mTranslation);
		mMaleHair.transform(t);

		// TODO - This schould be done in all bodyparts
		setBounds(mHead.getBounds().x + new Float(mStickman.mGeneralXTranslation).intValue(),
		  mHead.getBounds().y + new Float(mStickman.mGeneralYTranslation).intValue(),
		  new Float(mHead.getBounds().width * mStickman.mScale).intValue(),
		  new Float(mHead.getBounds().height * mStickman.mScale).intValue());
	}

	@Override
	protected void paintComponent(Graphics g) {
		super.paintComponent(g);

		Graphics2D g2 = (Graphics2D) g;

		// fill
		g2.setColor(new Color(242, 227, 217, 200));
		// head
		g2.fill(mHead);

		// ears
		g2.fill(mLeftEar);
		g2.fill(mRightEar);

		// draw outlines
		g2.setColor(g2.getColor().darker());
		g2.setStroke(new BasicStroke(2));
		//head
		g2.draw(mHead);
		// ears
		g2.draw(mLeftEar);
		g2.draw(mRightEar);

		// hair
		if (mStickman.mType == Stickman.TYPE.FEMALE) {
			g2.setColor(new Color(240, 212, 0, 255));
			g2.fill(mFemaleHair);
			// draw outlines
			g2.setColor(g2.getColor().darker());
			g2.setStroke(new BasicStroke(2));
			g2.draw(mFemaleHair);
		} else {

			g2.setColor(new Color(97, 58, 0, 255));
			g2.fill(mMaleHair);
			// draw outlines
			g2.setColor(g2.getColor().darker());
			g2.setStroke(new BasicStroke(2));
			g2.draw(mMaleHair);
		}
	}
}
