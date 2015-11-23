package de.dfki.vsm.players.stickman.environment;

import de.dfki.vsm.players.stickman.animationlogic.Animator;
import de.dfki.vsm.players.stickman.body.BodyPart;
import de.dfki.vsm.players.stickman.body.Head;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.font.FontRenderContext;
import java.awt.font.LineBreakMeasurer;
import java.awt.font.TextAttribute;
import java.awt.font.TextLayout;
import java.awt.geom.GeneralPath;
import java.text.AttributedString;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class SpeechBubble extends BodyPart {

	public static enum SHAPE {

		DEFAULT, SPEAK, THINK
	};

	Head mHead;
	public SpeechBubble.SHAPE mShape = SpeechBubble.SHAPE.DEFAULT;
	public String mText = "";
	public String mCurrentlySpokenText = "";
	int mLength = 120;
	int mHeight = 30;
	Point mBubbleCenter = new Point(0, 0);
	Dimension mSize = new Dimension(mHeight, mLength);
	FontMetrics mFontMetrics;
	Font mFont;

	GeneralPath mBubble;

	public SpeechBubble(Head head) {
		mHead = head;
		mColor = new Color(255, 255, 255, 192);

		Map<TextAttribute, Object> map = new HashMap<>();
		map.put(TextAttribute.KERNING, TextAttribute.KERNING_ON);
		map.put(TextAttribute.FAMILY, Font.SANS_SERIF);
		//map.put(TextAttribute.POSTURE, TextAttribute.POSTURE_OBLIQUE);
		map.put(TextAttribute.WEIGHT, TextAttribute.WEIGHT_DEMIBOLD);
		map.put(TextAttribute.SIZE, 14);

		mFont = Font.getFont(map);
		mFontMetrics = getFontMetrics(mFont);
		setFont(mFont);

		init();
	}

	@Override
	public void setShape(String s) {
		SpeechBubble.SHAPE shape = SpeechBubble.SHAPE.valueOf(s);
		mShape = (shape != null) ? shape : SpeechBubble.SHAPE.DEFAULT;
	}

	@Override
	public void resetShape() {
		mShape = SpeechBubble.SHAPE.DEFAULT;
	}

	@Override
	public void createShape() {
		mStart = mHead.getSpeechBubbleStartPosition();

		switch (mShape) {
			case DEFAULT:
				mBubble = new GeneralPath();
				break;

			case SPEAK:
				Point upperBubbleSpikeStart = new Point(mStart.x + mHead.mSize.width / 3, mStart.y - mHead.mSize.height / 3 - 10);
				Point lowerBubbleSpikeStart = new Point(upperBubbleSpikeStart.x + 10, upperBubbleSpikeStart.y + 5);
				Point topSeam = new Point(upperBubbleSpikeStart.x + mLength / 2, upperBubbleSpikeStart.y - mHeight * 3);
				Point bottomSeam = new Point(upperBubbleSpikeStart.x + mLength / 2, upperBubbleSpikeStart.y + mHeight / 3);

				mBubbleCenter.x = topSeam.x;
				mBubbleCenter.y = (topSeam.y + bottomSeam.y) / 2;

				mBubble = new GeneralPath();
				mBubble.moveTo(mStart.x, mStart.y);
				mBubble.quadTo((mStart.x + upperBubbleSpikeStart.x) / 2 + 10, (mStart.y + upperBubbleSpikeStart.y) / 2 + 10, upperBubbleSpikeStart.x, upperBubbleSpikeStart.y);
				mBubble.curveTo(upperBubbleSpikeStart.x - mLength / 3, upperBubbleSpikeStart.y - 10, topSeam.x - mLength, topSeam.y, topSeam.x, topSeam.y);
				mBubble.curveTo(topSeam.x + mLength, topSeam.y, bottomSeam.x + mLength, bottomSeam.y, bottomSeam.x, bottomSeam.y);
				mBubble.quadTo((lowerBubbleSpikeStart.x + bottomSeam.x) / 2, (lowerBubbleSpikeStart.y + bottomSeam.y) / 2 + 3, lowerBubbleSpikeStart.x, lowerBubbleSpikeStart.y);
				mBubble.quadTo((mStart.x + lowerBubbleSpikeStart.x) / 2 + 10, (mStart.y + lowerBubbleSpikeStart.y) / 2 + 10, mStart.x, mStart.y);
				break;
		}
	}

	@Override
	protected void paintComponent(Graphics g) {
		super.paintComponent(g);

		Graphics2D g2 = (Graphics2D) g;

		g2.setColor(mColor);

		g2.fill(mBubble);

		// draw text
		if (mShape != SHAPE.DEFAULT) {
			// compute font correction offsets
			final int hOffset = mFontMetrics.getAscent() - mFontMetrics.getDescent() + 4;
			//final int wIdOffset = mFontMetrics.stringWidth(mText) / 2;

			g2.setColor(new Color(0, 0, 0, 172));

			FontRenderContext fontRenderContext = g2.getFontRenderContext();
			
			int startB = mText.indexOf(mCurrentlySpokenText);
			int endB = startB + mCurrentlySpokenText.length();

			AttributedString attributedString = new AttributedString(mText);
			attributedString.addAttribute(TextAttribute.FONT, mFont);
			if (endB > startB) {
				//mHead.mStickman.mLogger.info("total >" + mText + "< current >" + mCurrentlySpokenText + "< start " + startB + " end " + endB);
				//attributedString.addAttribute(TextAttribute.UNDERLINE, TextAttribute.UNDERLINE_ON, startB, endB);
				//attributedString.addAttribute(TextAttribute.INPUT_METHOD_UNDERLINE, TextAttribute.UNDERLINE_LOW_TWO_PIXEL, startB, endB);
				attributedString.addAttribute(TextAttribute.FOREGROUND, new Color(92, 31, 32), startB, endB);
			}
			LineBreakMeasurer textBreak = new LineBreakMeasurer(attributedString.getIterator(), fontRenderContext);

			// get lines ...
			int lines = 0;
			while (textBreak.getPosition() < mLength) {
				TextLayout layout = textBreak.nextLayout(mLength);
				if (layout == null) {
					break;
				}
				lines++;
			}

			int drawYOffset = lines * hOffset / 2;

			// layout and draw 
			textBreak = new LineBreakMeasurer(attributedString.getIterator(), fontRenderContext);

			lines = 1;
			int start = 0;
			int end = 0;
			while (textBreak.getPosition() < mLength) {
				start = end;
				end = textBreak.getPosition();
				TextLayout layout = textBreak.nextLayout(mLength);
				if (layout == null) {
					break;
				}
				float textCunkWidth = layout.getAdvance();
				layout.draw(g2, mBubbleCenter.x - textCunkWidth / 2, mBubbleCenter.y + hOffset * lines - drawYOffset);
				lines++;
			}
		}

		if (mCurrentlySpokenText.isEmpty()) {
			g2.setColor(new Color(0, 0, 0, 64 - (Animator.sMAX_ANIM_STEPS - (new Double(mShapeAnimationStep)).intValue()) * 3));
		} else {
			g2.setColor(new Color(0, 0, 0, 64));
		}
		g2.setStroke(new BasicStroke(3));

		g2.draw(mBubble);
	}
}
