package de.dfki.vsm.xtension.baxter.utils.messagehandlers.handlers;

import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.baxter.utils.MessageObservable;
import de.dfki.vsm.xtension.baxter.utils.MessageObserver;

import java.util.Map;

/**
 * Created by alvaro on 22.09.16.
 */
public class AudioHandler implements MessageObserver {
    private final Map<String, ActivityWorker> mActivityWorkerMap;

    public AudioHandler(Map<String, ActivityWorker> mActivityWorkerMap, MessageObservable observable){
        this.mActivityWorkerMap = mActivityWorkerMap;
        observable.register(this);
    }

    @Override
    public void update(String message) {
        if (message.contains("#AUDIO#end#")) {
            handleAudio(message);
        }
    }

    void handleAudio(String message) {
        final int start = message.lastIndexOf("#") + 1;
        final String event_id = message.substring(start);
        synchronized (mActivityWorkerMap) {
            mActivityWorkerMap.remove(event_id);
            mActivityWorkerMap.notifyAll();
        }
    }
}
