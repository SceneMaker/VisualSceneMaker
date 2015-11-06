package de.dfki.vsm.players.action;

/**
 *
 * @author Patrick Gebhard
 *
 */
public interface ActionListener {

	public static enum STATE {

		ALL_ACTIONS_FINISHED, ACTION_STARTED, ACTION_FINISHED, ACTION_UNKNOWN
	}

	public void update(final STATE actionState);

	public void update(final Action action, final STATE actionState);
}
