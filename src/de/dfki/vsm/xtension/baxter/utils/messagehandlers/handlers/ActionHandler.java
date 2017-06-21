package de.dfki.vsm.xtension.baxter.utils.messagehandlers.handlers;

import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.baxter.utils.MessageObservable;
import de.dfki.vsm.xtension.baxter.utils.MessageObserver;

import java.util.Map;

/**
 * Created by alvaro on 22.09.16.
 */
public class ActionHandler implements MessageObserver {
    private final Map<String, ActivityWorker> mActivityWorkerMap;
    private final RunTimeProject project;

    public ActionHandler(Map<String, ActivityWorker> mActivityWorkerMap, RunTimeProject project, MessageObservable observable) {
        this.mActivityWorkerMap = mActivityWorkerMap;
        this.project = project;
        observable.register(this);
    }

    @Override
    public void update(String message) {
        if (message.contains("$")) {
            handleAction(message);
        }
    }

    void handleAction(final String message) {
        synchronized (mActivityWorkerMap) {
            mActivityWorkerMap.notifyAll();
            project.getRunTimePlayer().getActivityScheduler().handle(message);
        }
    }
}
