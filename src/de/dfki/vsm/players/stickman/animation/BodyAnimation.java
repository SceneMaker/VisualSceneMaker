/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players.stickman.animation;

import de.dfki.vsm.players.stickman.body.BodyPart;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class BodyAnimation {

	public BodyPart mBodyPart;
	public String mAction;
	public int mParam;
	public String mParamString;

	public BodyAnimation(BodyPart bp, String a, int p) {
		mBodyPart = bp;
		mAction = a;
		mParam = p;
		mParamString = "";
	}

	public BodyAnimation(BodyPart bp, String a, String p) {
		mBodyPart = bp;
		mAction = a;
		mParam = 0;
		mParamString = p;
	}

}
