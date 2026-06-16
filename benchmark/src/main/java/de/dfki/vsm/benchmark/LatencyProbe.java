package de.dfki.vsm.benchmark;

import de.dfki.vsm.event.EventListener;
import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.event.event.NodeStartedEvent;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Registers on a project's EventDispatcher and records timing deviation
 * for each NodeStartedEvent on the benchmark loop node ("N2").
 *
 * Deviation = (actual inter-event interval) - (expected timeout ms).
 * Positive = fired late, negative = fired early (shouldn't happen).
 */
class LatencyProbe implements EventListener {

    private final int mExpectedIntervalMs;
    private final String mTargetNodeId;
    private final int mWarmupCount;

    private long mLastNano = -1;
    private int mSamplesReceived = 0;

    private final List<Long> mDeviations = Collections.synchronizedList(new ArrayList<>());

    LatencyProbe(String targetNodeId, int expectedIntervalMs, int warmupCount) {
        mTargetNodeId = targetNodeId;
        mExpectedIntervalMs = expectedIntervalMs;
        mWarmupCount = warmupCount;
    }

    @Override
    public void update(EventObject event) {
        if (!(event instanceof NodeStartedEvent)) return;
        NodeStartedEvent nse = (NodeStartedEvent) event;
        if (!mTargetNodeId.equals(nse.getNode().getId())) return;

        long now = System.nanoTime();
        if (mLastNano > 0) {
            long actualMs = (now - mLastNano) / 1_000_000L;
            long deviation = actualMs - mExpectedIntervalMs;
            if (mSamplesReceived >= mWarmupCount) {
                mDeviations.add(deviation);
            }
        }
        mLastNano = now;
        mSamplesReceived++;
    }

    List<Long> getDeviations() {
        return new ArrayList<>(mDeviations);
    }

    int sampleCount() {
        return mDeviations.size();
    }
}
