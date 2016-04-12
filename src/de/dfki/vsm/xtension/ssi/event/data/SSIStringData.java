package de.dfki.vsm.xtension.ssi.event.data;

/**
 * @author Gregor Mehlmann
 */
public final class SSIStringData implements SSIEventData {

    // The raw string data
    final String mData;

    // Construct string data
    public SSIStringData(final String data) {
        mData = data;
    }

    // Get string representation
    @Override
    public String toString() {
        return mData;
    }
}
