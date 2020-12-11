package de.dfki.vsm.xtension.timer;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author Gregor Mehlmann, Patrick Gebhard
 */
public final class TimerExecutor extends ActivityExecutor {

    private final ConcurrentHashMap<String, Long> mTimerMap
            = new ConcurrentHashMap<>();

    // Construct executor
    public TimerExecutor(
            final PluginConfig config,
            final RunTimeProject project) {
        super(config, project);
    }

    // Get marker syntax
    @Override
    public synchronized String marker(final long id) {
        return "$(" + id + ")";
    }

    @Override
    public final void launch() {
    }

    @Override
    public final void unload() {
    }

    @Override
    public void execute(final AbstractActivity activity) {

        //activity.setType(AbstractActivity.Type.blocking);

        // Get log message features

        mLogger.message("TimerExecutor, processing action " + activity.getName());
        final String name = activity.getName();
        if (name.equalsIgnoreCase("clear")) {
            clear();
        } else if (name.equalsIgnoreCase("init")) {
            init(activity.get("id"));
        } else if (name.equalsIgnoreCase("time")) {
            time(activity.get("id"), activity.get("var"));
        } else if (name.equalsIgnoreCase("systime")) {
            mProject.setVariable(activity.get("var"), Long.toString(System.currentTimeMillis()));
        } else if (name.equalsIgnoreCase("timediff")) {
            Long lasttime = Long.parseLong(activity.get("lasttime"));
            Long currenttime = System.currentTimeMillis();
            Long diff = currenttime-lasttime;

            mProject.setVariable(activity.get("var"), diff.intValue());
        }
    }

    private void clear() {
        mTimerMap.clear();
    }

    private void init(final String id) {
        mTimerMap.put(id, System.currentTimeMillis());
    }

    private void time(final String id, final String var) {
        if (mTimerMap.containsKey(id) && mProject.hasVariable(var)) {
            mProject.setVariable(var, Math.toIntExact(
                    System.currentTimeMillis() - mTimerMap.get(id)));
        }
    }
}
