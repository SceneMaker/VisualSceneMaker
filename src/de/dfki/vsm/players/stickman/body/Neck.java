/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players.stickman.body;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.geom.GeneralPath;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class Neck extends BodyPart {

	Head mHead;

	public Neck(Head head) {
		mHead = head;
		mLength = 8;
		mSize = new Dimension(4, mLength);
		mColor = new Color(80, 80, 80);
		
		init();
	}

	public Point getBodyStartPosition() {
		return new Point(mEnd.x, mEnd.y);
	}

	@Override
	public void createShape() {
		mStart = mHead.getNeckStartPosition();
		mEnd = new Point(mStart.x, mStart.y + mLength);

		clearDrawObjects();
		
		GeneralPath gp = new GeneralPath();
		gp.moveTo(mStart.x, mStart.y);
		gp.lineTo(mEnd.x, mEnd.y);
		
		addToDrawObjects(gp);
	}
}
