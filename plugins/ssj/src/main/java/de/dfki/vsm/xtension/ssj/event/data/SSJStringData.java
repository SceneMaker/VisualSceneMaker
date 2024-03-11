package de.dfki.vsm.xtension.ssj.event.data;

/**
 * @author Gregor Mehlmann
 */
public final class SSJStringData extends SSJEventData
{
    // The raw string data
    final String mData;

    // Construct string data
    public SSJStringData()
    {
        mData = null;
    }

    // Construct string data
    public SSJStringData(final String data)
    {
        mData = data;
    }

    // Get string representation
    @Override
    public final String toString()
    {
        return mData;
    }
}
