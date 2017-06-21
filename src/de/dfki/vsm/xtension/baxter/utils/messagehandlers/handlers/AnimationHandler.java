package de.dfki.vsm.xtension.baxter.utils.messagehandlers.handlers;

import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.xtension.baxter.utils.MessageObservable;
import de.dfki.vsm.xtension.baxter.utils.MessageObserver;

import java.util.Map;

/**
 * Created by alvaro on 22.09.16.
 */
public class AnimationHandler implements MessageObserver {
    private final Map<String, ActivityWorker> mActivityWorkerMap;

    public AnimationHandler(Map<String, ActivityWorker> mActivityWorkerMap, MessageObservable observable) {
        this.mActivityWorkerMap = mActivityWorkerMap;
        observable.register(this);
    }

    @Override
    public void update(String message) {
        if (message.contains("#ANIM#end#")) {
            handleAnimation(message);
        }
    }

    private void handleAnimation(final String message) {
        int start = message.lastIndexOf("#") + 1;
        String animId = message.substring(start);
        if (mActivityWorkerMap.containsKey(animId)) {
            mActivityWorkerMap.remove(animId);
        }
        synchronized (mActivityWorkerMap) {
            mActivityWorkerMap.notifyAll();
        }
    }
}
