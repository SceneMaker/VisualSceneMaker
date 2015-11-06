/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players.stickman.animation;

/**
 *
 * @author Patrick Gebhard
 *
 */
public abstract class AnimationObject {

	// The Event Source
	protected transient Animation mSource;

	// Construct An Event
	public AnimationObject(final Animation source) {
		mSource = source;
	}

	// Get The Event Source
	public final Object getSource() {
		return mSource;
	}
}
