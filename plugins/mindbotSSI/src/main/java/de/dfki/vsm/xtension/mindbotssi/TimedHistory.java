package de.dfki.vsm.xtension.mindbotssi;

import java.util.Iterator;
import java.util.LinkedList;

public class TimedHistory {

    protected final LinkedList<TimedFloats> dataHistory = new LinkedList<>() ;

    private long timeWindowSizeMillis ;

    public int getNumVariables() {
        return nVariables;
    }

    /** Caches the number of variables in each history entry.
     * Will be initialized at the first entry inserted, and future entries will be checked for consistent size.
      */
    private int nVariables = 0 ;

    public TimedHistory(float window_size_secs) {
        this.timeWindowSizeMillis = (long)window_size_secs * 1000L ;
    }

    long getTimeWindowSizeMillis() {
        return timeWindowSizeMillis;
    }

    void setTimeWindowSizeMillis(long s) {
        timeWindowSizeMillis = s;
    }

    /** Add a new entry into the times history, and also delete old values, getting too old with respect to the time window size.
     *
     */
    public void appendData(long t, float[] new_data) {

        removeOldSamples(t);

        TimedFloats new_entry = new TimedFloats(t, new_data) ;

        // Runtime consistency check
        if(dataHistory.size()==0) {
            nVariables = new_entry.vs.length ;
        } else if(new_entry.vs.length != nVariables) {
            throw new RuntimeException("Number of variables in the new sample (" + new_entry.vs.length + ")" +
                    "doesn't match the one of previous samples (" + nVariables + ")") ;
        }

        dataHistory.add(new_entry) ;

    }

    public void removeOldSamples() {
        removeOldSamples(System.currentTimeMillis());
    }

    public void removeOldSamples(long now) {
        // Remove all "old" elements
        while(dataHistory.size() > 0) {
            TimedFloats oldest_entry = dataHistory.getFirst();
            // If the first element is too old, remove it
            if(now - oldest_entry.t > timeWindowSizeMillis) {
                dataHistory.removeFirst() ;
            } else {
                // if it is young enough, we can stop here.
                break ;
            }
        }

    }

    public void appendData(float[] new_data) {
        appendData(System.currentTimeMillis(), new_data) ;
    }

    public void clearDataHistory() {
        dataHistory.clear();
    }

    public int historySize() {
        return dataHistory.size() ;
    }


    public TimedHistory extractMostRecent(float range_secs) {
        long range_millis = (long)(range_secs * 1000.f) ;

        TimedHistory out = new TimedHistory(range_secs) ;

        if(dataHistory.size() > 0) {
            // copy the most recent data into the output
            TimedFloats youngest_sample = dataHistory.getLast();
            long youngest_time = youngest_sample.t;

            // From the most recent sample, iterate towards the oldest
            // copy each sample until the time difference exceeds the given input
            Iterator<TimedFloats> it = dataHistory.descendingIterator();
            while(it.hasNext()) {
                TimedFloats sample = it.next();
                long delta = youngest_time - sample.t ;
                assert delta >= 0 ;
                if(delta > range_millis ) {
                    break ;             // <-- JUMPS OUT !!!
                }
                out.appendData(sample.t, sample.vs);

            }
        }

        return out ;

    }


}
