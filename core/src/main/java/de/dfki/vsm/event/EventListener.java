package de.dfki.vsm.event;

/**
 * @author Gregor Mehlmann
 */
public interface EventListener {

    // Update an event listener with an event object
    public void update(final EventObject event);
}
