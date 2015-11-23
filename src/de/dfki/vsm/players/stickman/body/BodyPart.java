/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players.stickman.body;

import de.dfki.vsm.players.stickman.animationlogic.Animator;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.swing.JComponent;

/**
 *
 * @author Patrick Gebhard
 *
 */
public abstract class BodyPart extends JComponent {

	public enum SHAPE {

		DEFAULT
	};

	// variables for size and drawing
	public Dimension mSize = new Dimension(10, 10);
	public Point mStart = new Point(0, 0), mEnd = new Point(0, 0);
	public int mLength = 0;

	public double mAnimationStep = 0;
	public int mShapeAnimationStep = 0;

	public int mDefaultTranslation = 0;
	public double mTranslation = mDefaultTranslation;
	public double mToTranslation = mDefaultTranslation;
	public double mTranslationStep = 0.0f;

	public int mDefaultRotation = 0;
	public Point mDefaultRotationPoint = new Point(0, 0);
	public double mRotation = mDefaultRotation;
	public double mToDegree = mDefaultRotation;
	public double mRotationStep = 0.0f;

	List<GeneralPath> mGraphicPaths = Collections.synchronizedList(new ArrayList());

	public Color mColor = new Color(0, 0, 0);
	public BasicStroke mStroke = new BasicStroke(3.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);

	public void init() {
		setLayout(null);
		setSize(mSize);

		calculate(0);
	}

	public void setTranslation(int length) {
		mToTranslation = mTranslation + length;
		mTranslationStep = Math.abs(Math.abs(mTranslation) - Math.abs(mToTranslation)) / Animator.sMAX_ANIM_STEPS * (length / Math.abs(length));
	}

	public synchronized void calculateTranslation(int step) {
		mTranslation += mTranslationStep;
		mTranslation = (double) Math.round(mTranslation * 1000d) / 1000d; // the poor man's round method

		calculate(step);
	}

	public void resetTranslation() {
		mTranslationStep = 0.0d;
	}

	public void setDefaulRotation(int degree) {
		mDefaultRotation = degree;
		mRotation = mDefaultRotation;
		mToDegree = mDefaultRotation;
		mRotationStep = 0.0f;
	}

	public void setRotation(int degree) {
		mToDegree = mRotation + degree;
		mRotationStep = Math.abs(Math.abs(mRotation) - Math.abs(mToDegree)) / Animator.sMAX_ANIM_STEPS * (degree / Math.abs(degree));
	}

	public void setTilt(int degree) {
		mToDegree = mRotation + degree;
		mRotationStep = Math.abs(Math.abs(mRotation) - Math.abs(mToDegree)) / Animator.sMAX_ANIM_STEPS * (degree / Math.abs(degree));
	}

	public synchronized void calculateRotation(int step) {
		mRotation += mRotationStep;
		mRotation = (double) Math.round(mRotation * 1000d) / 1000d; // the poor man's round method

		calculate(step);
	}

	public void resetRotation() {
		mTranslationStep = 0.0d;
	}

	public void setShape(String s) {
		// place code for setting shape
	}

	public void createShape() {
		// create the shape
	}

	public synchronized void calculateShape(int step) {
		mShapeAnimationStep = step;

		calculate(step);
	}

	public void resetShape() {
		mShapeAnimationStep = 0;
	}

	public void clearDrawObjects() {
		mGraphicPaths = new ArrayList<>();
	}

	public void addToDrawObjects(GeneralPath gp) {
		mGraphicPaths.add(gp);
	}

	public synchronized void calculate(int step) {
		createShape();

		AffineTransform t = new AffineTransform();
		t.translate(0, mTranslation);
		t.rotate(Math.toRadians(mRotation), mDefaultRotationPoint.x, mDefaultRotationPoint.y);

		for (GeneralPath gp : mGraphicPaths) {
			gp.transform(t);
		}

	}

	@Override
	protected void paintComponent(Graphics g) {
		super.paintComponent(g);

		Graphics2D g2 = (Graphics2D) g;

		g2.setColor(mColor);
		g2.setStroke(mStroke);

		for (GeneralPath gp : mGraphicPaths) {
			g2.draw(gp);
		}
	}
}
