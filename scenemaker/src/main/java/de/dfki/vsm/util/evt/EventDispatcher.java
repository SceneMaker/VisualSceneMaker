package de.dfki.vsm.util.evt;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.CopyOnWriteArrayList;
import javax.swing.SwingUtilities;

/**
 * @author Gregor Mehlmann
 */
public final class EventDispatcher {

    // The Singelton Instance
    private static EventDispatcher sInstance = null;

    // The Logger Instance
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    // The Listener List
    private final CopyOnWriteArrayList<EventListener> mListenerList;

    // The Timer Thread
    private Timer mTimer;

    // Construct The Instance
    private EventDispatcher() {
        //
        mTimer = new Timer("EventCasterTimer");
        //
        mListenerList
                = new CopyOnWriteArrayList();
    }

    // Get The Singelton Instance
    public final static synchronized EventDispatcher getInstance() {
        if (sInstance == null) {
            sInstance = new EventDispatcher();
        }
        return sInstance;
    }

    // Add an event listener
    public final void register(final EventListener listener) {
        mListenerList.add(listener);
    }

    // Remove an event listener
    public final void remove(final EventListener listener) {
        mListenerList.remove(listener);
    }

    // Dispatch an event object
    private void dispatch(final EventObject event) {
        for (final EventListener listener : mListenerList) {
            listener.update(event);
        }
    }

    // Immediately schedule an event
    public final void convey(final EventObject event) {
        //schedule(event, 1);
        dispatch(event);
    }

    // Schedule dispatching of event
    private void schedule(final EventObject event, final long timeout) {

        // Create a timer task
        final TimerTask task = new TimerTask() {
            @Override
            public void run() {
                dispatch(event);
            }
        };

        try {
            // Schedule this timer task
            mTimer.schedule(task, timeout);
        } catch (final IllegalStateException exc) {
            mLogger.warning(exc.toString());
        }
    }

    // Cancel the timer thread
    public final synchronized void abort() {
        mTimer.cancel();
    }

    // Reset the timer thread
    public final synchronized void reset() {
        mTimer = new Timer();
    }
}
