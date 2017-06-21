package de.dfki.vsm.xtension.baxter.utils.messagehandlers;

import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.baxter.utils.MessageObservable;
import de.dfki.vsm.xtension.baxter.utils.MessageObserver;
import de.dfki.vsm.xtension.baxter.utils.messagehandlers.handlers.ActionHandler;
import de.dfki.vsm.xtension.baxter.utils.messagehandlers.handlers.AnimationHandler;
import de.dfki.vsm.xtension.baxter.utils.messagehandlers.handlers.AudioHandler;

import java.util.LinkedList;
import java.util.Map;

public class BaxterMessageHandler implements MessageObservable{

    private final LinkedList<MessageObserver> observers = new LinkedList<>();

    public BaxterMessageHandler(Map<String, ActivityWorker> workerMap, RunTimeProject p) {
        createObservers(workerMap, p);
    }

    public BaxterMessageHandler(RunTimeProject p) {

    }

    protected void createObservers(Map<String, ActivityWorker> workerMap, RunTimeProject p) {
        //Loading basic Handlers
        new ActionHandler(workerMap, p, this);
        new AudioHandler(workerMap, this);
        new AnimationHandler(workerMap, this);
    }

    public void handle(final String message){
        notifyAll(message);
    }

    @Override
    public void register(MessageObserver o) {
        observers.add(o);
    }

    @Override
    public void unregister(MessageObserver o) {
        if(observers.contains(o)){
            observers.remove(o);
        }
    }

    @Override
    public void notifyAll(final String message) {
        for (final MessageObserver o:observers) {
            o.update(message);
        }
    }
}