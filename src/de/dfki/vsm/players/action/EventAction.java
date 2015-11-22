package de.dfki.vsm.players.action;

import java.util.List;

/**
 *
 * @author Patrick Gebhard
 *
 */
public abstract class EventAction extends Action {

	// a list of time marks for actions that are synchronized by this action
	public List<String> mSynchronizedActionTimeMarks;

	public void addTimeMark(String timemark) {
		if (!mSynchronizedActionTimeMarks.contains(timemark)) {
			mSynchronizedActionTimeMarks.add(timemark);
		}
	}
}