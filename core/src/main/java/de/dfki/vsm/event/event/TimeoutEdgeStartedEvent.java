package de.dfki.vsm.event.event;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge;

/**
 * @author Patrick Gebhard
 */
public class TimeoutEdgeStartedEvent extends EventObject {
    private final TimeoutEdge mEdge;
    private final long mTimeoutMs;
    private final long mStartedAt;

    public TimeoutEdgeStartedEvent(Object source, TimeoutEdge edge, long timeoutMs, long startedAt) {
        super(source);
        mEdge = edge;
        mTimeoutMs = timeoutMs;
        mStartedAt = startedAt;
    }

    public TimeoutEdge getEdge() {
        return mEdge;
    }

    public long getTimeoutMs() {
        return mTimeoutMs;
    }

    public long getStartedAt() {
        return mStartedAt;
    }

    public String getEventDescription() {
        return "TimeoutEdgeEvent(" + (mEdge != null ? mEdge.getSourceUnid() : "") + ")";
    }
}
