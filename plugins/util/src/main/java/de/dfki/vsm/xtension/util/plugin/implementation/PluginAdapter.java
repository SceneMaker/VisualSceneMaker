package de.dfki.vsm.xtension.util.plugin.implementation;

import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.xtension.util.plugin.AgentPlugin;
import de.dfki.vsm.xtension.util.runtime.DrivenRuntime;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public abstract class PluginAdapter extends ActivityExecutor implements DrivenRuntime {
    // The singleton logger instance
    protected final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    // The map of activity worker
    private final Map<String, Thread> mActivityWorkerMap = new HashMap<>();

    public PluginAdapter(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    protected AgentPlugin mPlugin;

    @Override
    public synchronized String marker(long id) {
        return mPlugin.marker(id);
    }

    @Override
    public void execute(AbstractActivity activity) {

        if (activity instanceof SpeechActivity) {
            SpeechActivity sa = (SpeechActivity) activity;
            executeSpeechActivity(sa);
        } else if (activity instanceof ActionActivity) {
            ActionActivity actionActivity = (ActionActivity) activity;
            mPlugin.execute(actionActivity);
        }
    }

    @Override
    public void execute(SpeechActivity activity) {
        mPlugin.execute(activity);
    }


    @Override
    public void execute(ActionActivity activity) {
        mPlugin.execute(activity);
    }

    private void executeSpeechActivity(SpeechActivity sa) {
        String text = sa.getTextOnly("${'").trim();
        List<String> timemarks = sa.getTimeMarks("${'");


        // If text is empty - assume activity has empty text but has marker activities registered
        if (text.isEmpty()) {
            executeTimeMarks(timemarks);
        } else {
            // Make text activity blocking
            sa.setType(AbstractActivity.Type.blocking);
            mPlugin.execute(sa);
        }
    }

    public void wait(String vmuid) {
        synchronized (mActivityWorkerMap) {
            //only enable blocking method if at least one connection exists.
            // TODO: make sure it is a valid connection with a valid VuppetMaster client

            // organize wait for feedback if (activity instanceof SpeechActivity) {
            Thread cAW = Thread.currentThread();
            mActivityWorkerMap.put(vmuid, cAW);


            mLogger.message("ActivityWorker waiting for feedback on action with id " + vmuid + "...");
            while (mActivityWorkerMap.containsValue(cAW)) {
                try {
                    mActivityWorkerMap.wait();
                } catch (InterruptedException exc) {
                    mLogger.failure(exc.toString());
                }
            }
            mLogger.message("ActivityWorker proceed - got feedback on blocking action with id " + vmuid + "...");
        }
    }

    private void executeTimeMarks(List<String> timemarks) {
        for (String tm : timemarks) {
            mLogger.warning("Directly executing activity at timemark " + tm);
            handleMarker(tm);
        }
    }

    public void purgeActivities() {
        mLogger.message("Remove active (but not needed anymore) activity actions");
        synchronized (mActivityWorkerMap) {
            mActivityWorkerMap.clear();
            // wake me up ..
            mActivityWorkerMap.notifyAll();
        }
    }

    public void stopWaiting(String header) {
        mLogger.message("Processing stop message ...");

        synchronized (mActivityWorkerMap) {
            if (mActivityWorkerMap.containsKey(header)) {
                mLogger.message("Removing id from active activities ids ...");
                mActivityWorkerMap.remove(header);
                // wake me up ...
                mLogger.message("Unlocking activity manager ...");
                mActivityWorkerMap.notifyAll();
                mLogger.message("done.");
            } else {
                mLogger.failure("Activityworker for action with id " + header + " has been stopped before ...");
            }
        }
    }

    public void handleMarker(String marker) {
        if (mProject.getRunTimePlayer().getActivityScheduler().hasMarker(marker)) {
            mProject.getRunTimePlayer().getActivityScheduler().handle(marker);
        } else {
            mLogger.failure("Marker has already been processed!");
        }
    }

    public void setVar(String name, AbstractValue value) {
        mProject.setVariable(name, value);
    }

    public boolean hasVar(String varName) {
        return mProject.hasVariable(varName);
    }

    public AbstractValue getVar(String varName) {
        return mProject.getValueOf(varName);
    }

    public AgentConfig getAgentConfig(String agentName) {
        return mProject.getAgentConfig(agentName);
    }
}
