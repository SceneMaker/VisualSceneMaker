package de.dfki.vsm.util.evt;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.util.LinkedList;
import java.util.Timer;
import java.util.TimerTask;

/**
 * @author Gregor Mehlmann
 */
public class EventCaster {

    // The Singelton Instance
    private static EventCaster sInstance = null;
    // The Logger Instance
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();
    // The Listener List
    private final LinkedList<EventListener> mListenerList
            = new LinkedList<EventListener>();
    // The Timer Thread
    private final Timer mTimer = new Timer("EventCasterTimer");

    // Get The Singelton Instance
    public final static synchronized EventCaster getInstance() {
        if (sInstance == null) {
            sInstance = new EventCaster();
        }
        return sInstance;
    }

    // Construct The Instance
    private EventCaster() {
    }

    // Cancel The Timer Thread
    public final synchronized void cancel() {
        mTimer.cancel();
    }

    // Add An Event Listener
    public final synchronized void append(final EventListener listener) {
        mListenerList.add(listener);
    }

    public final synchronized void remove(final EventListener listener) {
        mListenerList.remove(listener);
    }

    public final synchronized void convey(final EventObject event) {
        for (final EventListener listener : mListenerList) {
            listener.update(event);
        }
    }

    public final synchronized void schedule(final EventObject event, final long timeout) {
        // Create The Timer Task
        final TimerTask timer = new TimerTask() {

            @Override
            public void run() {
                convey(event);
            }
        };
        // Schedule The Event
        mTimer.schedule(timer, timeout);
    }
}
