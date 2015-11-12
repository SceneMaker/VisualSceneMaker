package de.dfki.vsm.players.stickman;

import de.dfki.vsm.model.scenescript.SceneUttr;
import de.dfki.vsm.players.action.sequence.WordTimeMarkSequence;
import de.dfki.vsm.players.stickman.animation.Animation;
import de.dfki.vsm.players.stickman.animation.listener.AnimationListener;
import de.dfki.vsm.players.stickman.animation.AnimationScheduler;
import de.dfki.vsm.players.stickman.animation.EventAnimation;
import de.dfki.vsm.players.stickman.body.Body;
import de.dfki.vsm.players.stickman.body.Head;
import de.dfki.vsm.players.stickman.body.LeftEye;
import de.dfki.vsm.players.stickman.body.LeftEyebrow;
import de.dfki.vsm.players.stickman.body.LeftForeArm;
import de.dfki.vsm.players.stickman.body.LeftHand;
import de.dfki.vsm.players.stickman.body.LeftLeg;
import de.dfki.vsm.players.stickman.body.LeftShoulder;
import de.dfki.vsm.players.stickman.body.LeftUpperArm;
import de.dfki.vsm.players.stickman.body.Mouth;
import de.dfki.vsm.players.stickman.body.Neck;
import de.dfki.vsm.players.stickman.body.RightEye;
import de.dfki.vsm.players.stickman.body.RightEyebrow;
import de.dfki.vsm.players.stickman.body.RightForeArm;
import de.dfki.vsm.players.stickman.body.RightHand;
import de.dfki.vsm.players.stickman.body.RightLeg;
import de.dfki.vsm.players.stickman.body.RightShoulder;
import de.dfki.vsm.players.stickman.body.RightUpperArm;
import de.dfki.vsm.players.stickman.environment.SpeechBubble;
import de.dfki.vsm.players.stickman.util.AnimationLoader;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.font.TextAttribute;
import java.awt.geom.AffineTransform;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Semaphore;
import java.util.logging.ConsoleHandler;
import java.util.logging.Formatter;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import javax.swing.JComponent;

/**
 *
 * @author Patrick Gebhard
 *
 * This work is inspired by the stickmans drawn by Sarah Johnson
 * (www.sarah-johnson.com) in the Valentine music video from Kina Grannis shot
 * by Ross Ching in 2012
 *
 */
public class Stickman extends JComponent {

	// general stuff
	public static enum ORIENTATION {

		FRONT, LEFT, RIGHT
	};

	public static enum GENDER {

		FEMALE, MALE
	};

	static public final Color sFOREGROUND = new Color(188, 188, 188, 128);
	public GENDER mGender = GENDER.FEMALE;
	public String mName = "Stickman";
	public ORIENTATION mOrientation = ORIENTATION.FRONT;
	public float mScale = 1.0f;

	public static Dimension mSize = new Dimension(400, 600);
	FontMetrics mFontMetrics;
	Font mFont;

	// amimation stuff
	public Semaphore mAnimationLaunchControl = new Semaphore(1);
	public AnimationScheduler mAnimationScheduler;
	private final ArrayList<AnimationListener> mAnimationListeners = new ArrayList<>();

	// body parts
	public Head mHead;
	public LeftEyebrow mLeftEyebrow;
	public LeftEye mLeftEye;
	public RightEyebrow mRightEyebrow;
	public RightEye mRightEye;
	public Mouth mMouth;
	public Neck mNeck;
	public Body mBody;
	public LeftShoulder mLeftShoulder;
	public LeftUpperArm mLeftUpperArm;
	public LeftForeArm mLeftForeArm;
	public LeftHand mLeftHand;
	public RightShoulder mRightShoulder;
	public RightUpperArm mRightUpperArm;
	public RightForeArm mRightForeArm;
	public RightHand mRightHand;
	public LeftLeg mLeftLeg;
	public RightLeg mRightLeg;
	// environment
	public SpeechBubble mSpeechBubble;

	// logging
	public Logger mLogger = Logger.getAnonymousLogger();

	// id
	private long mID = 0;

	public Stickman(String name, GENDER gender, float scale) {
		this(name, gender);
		mScale = scale;
	}

	public Stickman(String name, GENDER gender) {
		mName = name;
		mGender = gender;

		mHead = new Head(this);
		mLeftEyebrow = new LeftEyebrow(mHead);
		mLeftEye = new LeftEye(mHead);
		mRightEyebrow = new RightEyebrow(mHead);
		mRightEye = new RightEye(mHead);
		mMouth = new Mouth(mHead);
		mNeck = new Neck(mHead);
		mBody = new Body(mNeck);
		mLeftShoulder = new LeftShoulder(mBody);
		mLeftUpperArm = new LeftUpperArm(mLeftShoulder);
		mLeftForeArm = new LeftForeArm(mLeftUpperArm);
		mLeftHand = new LeftHand(mLeftForeArm);
		mRightShoulder = new RightShoulder(mBody);
		mRightUpperArm = new RightUpperArm(mRightShoulder);
		mRightForeArm = new RightForeArm(mRightUpperArm);
		mRightHand = new RightHand(mRightForeArm);
		mLeftLeg = new LeftLeg(mBody);
		mRightLeg = new RightLeg(mBody);

		mSpeechBubble = new SpeechBubble(mHead);

		init();
	}

	private void init() {
		setLayout(null);
		setPreferredSize(mSize);
		setMinimumSize(mSize);
		setSize(mSize);

		// font stuff
		Map<TextAttribute, Object> map = new HashMap<>();
		map.put(TextAttribute.KERNING, TextAttribute.KERNING_ON);
		map.put(TextAttribute.FAMILY, Font.SANS_SERIF);
		//map.put(TextAttribute.POSTURE, TextAttribute.POSTURE_OBLIQUE);
		map.put(TextAttribute.WEIGHT, TextAttribute.WEIGHT_DEMIBOLD);
		map.put(TextAttribute.SIZE, 14);

		mFont = Font.getFont(map);
		mFontMetrics = getFontMetrics(mFont);
		setFont(mFont);

		ConsoleHandler ch = new ConsoleHandler();
		ch.setFormatter(new StickmanLogFormatter());

		mLogger.addHandler(ch);
		mLogger.setUseParentHandlers(false);

		mAnimationScheduler = new AnimationScheduler(this);
		mAnimationScheduler.start();
	}

	public void addListener(AnimationListener al) {
		mAnimationListeners.add(al);
	}

	public void removeListener(AnimationListener al) {
		if (mAnimationListeners.contains(al)) {
			mAnimationListeners.remove(al);
		}
	}

	public void notifyListeners(Animation a) {
		synchronized (mAnimationListeners) {
			mAnimationListeners.stream().forEach((al) -> {
				al.update(a);
			});
		}
	}

	public String getID() {
		return (new StringBuffer()).append(mName).append(" Animation ").append(mID++).toString();
	}

	@Override
	public String getName() {
		return mName;
	}

	// Sets the orientation of the character, allowed values are: LEFT, RIGHT, FRONT
	public void setOrientation(String orientation) {
		if (orientation.equalsIgnoreCase(ORIENTATION.LEFT.toString())) {
			mOrientation = ORIENTATION.LEFT;
		} else if (orientation.equalsIgnoreCase(ORIENTATION.RIGHT.toString())) {
			mOrientation = ORIENTATION.RIGHT;
		} else {
			mOrientation = ORIENTATION.FRONT;
		}
	}

	public Animation doEventFeedbackAnimation(String type, String name, int duration, WordTimeMarkSequence wts, boolean block) {

//		if (type.equalsIgnoreCase("environment")) {
//			if (name.equalsIgnoreCase("Speaking")) {
//				mSpeechBubble.mText = utterance.getText();
//			}
//		}
		
		EventAnimation a = AnimationLoader.getInstance().load(this, type, name, wts, duration, block);

		if (a != null) {
			try {
				mAnimationLaunchControl.acquire();
				a.start();
			} catch (InterruptedException ex) {
				mLogger.severe(ex.getMessage());
			}
		}

		return a;
	}

	public Animation doAnimation(String type, String name, int duration, String text, boolean block) {
		if (type.equalsIgnoreCase("environment")) {
			if (name.equalsIgnoreCase("Speaking")) {
				mSpeechBubble.mText = text;
			}
		}

		return doAnimation(type, name, duration, block);
	}

	public Animation doAnimation(String type, String name, String text, boolean block) {
		if (type.equalsIgnoreCase("environment")) {
			if (name.equalsIgnoreCase("Speaking")) {
				mSpeechBubble.mText = text;
			}
		}

		return doAnimation(type, name, -1, block);
	}

	public Animation doAnimation(String type, String name, boolean block) {
		return doAnimation(type, name, -1, block);
	}

	public Animation doAnimation(String type, String name, int duration, boolean block) {
		Animation a = AnimationLoader.getInstance().load(this, type, name, duration, block);

		if (a != null) {
			try {
				mAnimationLaunchControl.acquire();
				a.start();
			} catch (InterruptedException ex) {
				mLogger.severe(ex.getMessage());
			}
		}

		return a;
	}

	@Override
	protected void paintComponent(Graphics g) {
		super.paintComponent(g);

		Graphics2D g2 = (Graphics2D) g;
		g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

		if (!mName.equalsIgnoreCase("")) {
			g2.setColor(sFOREGROUND);
			g2.fillRect(0, 0, mSize.width, mSize.height);

			// draw Stickman's name
			final int hOffset = mFontMetrics.getAscent() + mFontMetrics.getDescent();
			final int wOffset = mFontMetrics.stringWidth(mName);

			g2.setColor(sFOREGROUND.darker());
			g2.fillRect(0, mSize.height - hOffset * 4, mSize.width, mSize.height);
			g2.setColor(mBody.mColor.darker());
			g2.drawString(mName, 10, mSize.height - hOffset);
		}

		// draw everthing in the middle and scaled
		AffineTransform at = g2.getTransform();
		at.translate(mSize.width / 2 - mHead.mSize.width * mScale, getBounds().height - 470 * mScale);
		at.scale(mScale, mScale);
		g2.setTransform(at);

		// draw body parts
		mHead.update(g);
		mLeftEyebrow.update(g);
		mLeftEye.update(g);
		mRightEyebrow.update(g);
		mRightEye.update(g);
		mMouth.update(g);
		mNeck.update(g);
		mBody.update(g);
		mLeftShoulder.update(g);
		mLeftUpperArm.update(g);
		mLeftForeArm.update(g);
		mLeftHand.update(g);
		mRightShoulder.update(g);
		mRightUpperArm.update(g);
		mRightForeArm.update(g);
		mRightHand.update(g);
		mLeftLeg.update(g);
		mRightLeg.update(g);

		// draw environment
		mSpeechBubble.update(g);
	}

	private static class StickmanLogFormatter extends Formatter {

		@Override
		public String format(LogRecord record) {
			return ((new StringBuffer()).append(record.getLevel()).append(": ").append(record.getMessage()).append("\n")).toString();
		}
	}
}
