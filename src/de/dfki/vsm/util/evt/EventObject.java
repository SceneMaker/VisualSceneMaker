package de.dfki.vsm.util.evt;

/**
 * @author Not me
 */
public abstract class EventObject {

    // The Event Source
    protected transient Object mSource;

    // Construct An Event
    public EventObject(final Object source) {
        mSource = source;
    }

    // Get The Event Source
    public final Object getSource() {
        return mSource;
    }
}
