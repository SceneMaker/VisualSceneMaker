package de.dfki.vsm.players.stickman.util;

/**
 *
 * @author Patrick Gebhard
 * 
 */
public class TimingInfo {
	
	static final int sCHARDURATION = 50;
	
	public static int spokenStringDuration(String word) {
		
		String s = word.trim();
		
		int numOfSpaces = s.length() - s.replace(" ", "").length();
		
		int duration =  word.length() * sCHARDURATION;
		
		duration = duration  * ((numOfSpaces == 0) ? 2 : 1); // double the time if only one word is there
		
		duration = (duration < 500) ? 500 : duration; // minimum is 500 ms
		
		return duration; 
	}
}
