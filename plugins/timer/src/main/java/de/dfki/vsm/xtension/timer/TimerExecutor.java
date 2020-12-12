package de.dfki.vsm.xtension.timer;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;

import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
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
        mLogger.message("Launching Timer Executor ...");
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
            Long diff = currenttime - lasttime;
            mProject.setVariable(activity.get("var"), diff.intValue());
        } else if (name.equalsIgnoreCase("day")) {
            Calendar calendar = Calendar.getInstance();
            Date date = calendar.getTime();
            String day = new SimpleDateFormat("EEEE", Locale.GERMANY).format(date.getTime());
            mProject.setVariable(activity.get("var"), day);
        } else if (name.equalsIgnoreCase("partofday")) {
            Instant tinst = Instant.now();
            LocalDateTime ldt = LocalDateTime.ofInstant(tinst, ZoneId.systemDefault());
            int hour = ldt.getHour();
            String partOfDayDescription =
                    ((hour > 20) || (hour < 24)) ? "late" :
                            ((hour >= 0) || (hour < 4)) ? "very late" :
                                    ((hour >= 4) || (hour < 7)) ? "early" :
                                            ((hour >= 7) || (hour < 12)) ? "late early" : "mid day";
            mProject.setVariable(activity.get("var"), partOfDayDescription);
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
