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
            init(feature(activity, "id"));
        } else if (name.equalsIgnoreCase("time")) {
            time(feature(activity, "id"), feature(activity, "var"));
        } else if (name.equalsIgnoreCase("systime")) {
            mProject.setVariable(feature(activity, "var"), Long.toString(System.currentTimeMillis()));
        } else if (name.equalsIgnoreCase("timediff")) {
            Long lasttime = Long.parseLong(feature(activity, "lasttime"));
            Long currenttime = System.currentTimeMillis();
            Long diff = currenttime - lasttime;
            mProject.setVariable(feature(activity, "var"), diff.intValue());
        } else if (name.equalsIgnoreCase("day")) {
            Calendar calendar = Calendar.getInstance();
            Date date = calendar.getTime();
            String day = new SimpleDateFormat("EE", Locale.GERMANY).format(date.getTime());
            mProject.setVariable(feature(activity, "var"), day.replace(".", ""));
        } else if (name.equalsIgnoreCase("dayverbose")) {
            Calendar calendar = Calendar.getInstance();
            Date date = calendar.getTime();
            String day = new SimpleDateFormat("EEEE", Locale.GERMANY).format(date.getTime());
            mProject.setVariable(feature(activity, "var"), day.replace(".", ""));
        } else if (name.equalsIgnoreCase("partofday")) {
            Instant tinst = Instant.now();
            LocalDateTime ldt = LocalDateTime.ofInstant(tinst, ZoneId.systemDefault());
            int hour = ldt.getHour();
            String partOfDayDescription =
                    ((hour > 20) && (hour < 24)) ? "late" :
                            ((hour >= 0) && (hour < 4)) ? "very late" :
                                    ((hour >= 4) && (hour < 7)) ? "early" :
                                            ((hour >= 7) && (hour < 12)) ? "late early" :
                                                    ((hour >= 12) && (hour < 14)) ? "mid day" :
                                                            ((hour >= 14) && (hour < 16)) ? "late mid day" :
                                                                    ((hour >= 16) && (hour < 19)) ? "afternoon" : "evening";
            mProject.setVariable(feature(activity, "var"), partOfDayDescription);
        }
    }

    // AbstractActivity.get() returns a feature's value exactly as authored, single-quotes and all
    // (ActionFeature.getVal(HashMap) never strips them - only the unused getValNoQuotes() does).
    // Every feature this plugin reads is either a variable/timer NAME or a number, and a literal
    // "'name'" matches no declared variable and "'123'" fails Long.parseLong, so every command here
    // needs the quotes gone before use.
    private static String feature(final AbstractActivity activity, final String key) {
        final String raw = activity.get(key);
        if (raw != null && raw.length() >= 2 && raw.startsWith("'") && raw.endsWith("'")) {
            return raw.substring(1, raw.length() - 1);
        }
        return raw;
    }

    private void clear() {
        mTimerMap.clear();
    }

    private void init(final String id) {
        mTimerMap.put(id, System.currentTimeMillis());
    }

    private void time(final String id, final String var) {
        if (!mTimerMap.containsKey(id)) {
            mLogger.warning("Timer: no timer initialized for id '" + id + "' - '" + var + "' not updated. "
                    + "Did the matching [time: init id='" + id + "'] command actually fire before this one?");
            return;
        }
        if (!mProject.hasVariable(var)) {
            mLogger.warning("Timer: variable '" + var + "' is not declared in this project - not updated.");
            return;
        }
        mProject.setVariable(var, Math.toIntExact(
                System.currentTimeMillis() - mTimerMap.get(id)));
    }
}
