package de.dfki.vsm.players.stickman.util;

/**
 *
 * @author Patrick Gebhard
 * 
 */
public class TimingInfo {
	
	static final int sCHARDURATION = 50;
	
	public static int spokenStringDuration(String word) {
		return word.length() * sCHARDURATION;
	}
}
