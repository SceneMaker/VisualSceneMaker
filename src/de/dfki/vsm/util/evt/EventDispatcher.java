package de.dfki.vsm.util.evt;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------
import java.util.LinkedList;
import java.util.Timer;
import java.util.TimerTask;

/**
 * @author Not me
 */
public class EventDispatcher {

    // The Singelton Instance
    private static EventDispatcher sInstance = null;

    // The Logger Instance
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    // The Listener List
    private final LinkedList<EventListener> mListenerList = new LinkedList<EventListener>();

    // The Timer Thread
    private final Timer mTimer = new Timer("EventCasterTimer");

    // Construct The Instance
    private EventDispatcher() {
    }

    // Get The Singelton Instance
    public final static synchronized EventDispatcher getInstance() {
        if (sInstance == null) {
            sInstance = new EventDispatcher();
        }

        return sInstance;
    }

    // Cancel The Timer Thread
    public final synchronized void cancel() {
        mTimer.cancel();
    }

    // Add An Event Listener
    public final synchronized void register(final EventListener listener) {
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
