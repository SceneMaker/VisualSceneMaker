/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players.stickman.animationlogic;

import de.dfki.vsm.players.action.sequence.WordTimeMarkSequence;
import de.dfki.vsm.players.stickman.body.BodyPart;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class AnimationContent {

	public BodyPart mBodyPart;
	public String mAction;
	public int mParam;
	public String mParamString;
	public WordTimeMarkSequence mWTS;

	public AnimationContent(BodyPart bp, String a, int p) {
		mBodyPart = bp;
		mAction = a;
		mParam = p;
		mParamString = "";
	}

	public AnimationContent(BodyPart bp, String a, String p) {
		mBodyPart = bp;
		mAction = a;
		mParam = 0;
		mParamString = p;
	}

	public AnimationContent(BodyPart bp, String a, String p, WordTimeMarkSequence wts) {
		mBodyPart = bp;
		mAction = a;
		mParam = 0;
		mParamString = p;
		mWTS = wts;
	}
}
