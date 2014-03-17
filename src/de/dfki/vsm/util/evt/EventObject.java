package de.dfki.vsm.util.evt;

/**
 * @author Gregor Mehlmann
 */
public abstract class EventObject {

    // The Event Data
    protected transient Object mData;

    // Construct An Event
    public EventObject(final Object data) {
        mData = data;
    }

    // Get The Event Source
    public final Object getSource() {
        return mData;
    }
}
