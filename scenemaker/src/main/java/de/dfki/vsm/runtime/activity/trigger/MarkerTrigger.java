package de.dfki.vsm.runtime.activity.trigger;

import java.util.Objects;

/**
 * @author Gregor Mehlmann
 */
public final class MarkerTrigger implements ActivityTrigger {

    private final String mMarker;

    public MarkerTrigger(final String marker) {
        mMarker = marker;
    }

    public final String getMarker() {
        return mMarker;
    }

    @Override
    public final String toString() {
        return getMarker();
    }

    @Override
    public final boolean equals(final Object object) {
        if (object instanceof MarkerTrigger) {
            final MarkerTrigger trigger = (MarkerTrigger) object;
            // Check if the markers are equal
            return trigger.getMarker().equals(mMarker);
        }
        return false;
    }

    @Override
    public int hashCode() {
        int hash = 3;
        hash = 23 * hash + Objects.hashCode(mMarker);
        return hash;
    }
}
