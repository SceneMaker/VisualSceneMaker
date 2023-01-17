package de.dfki.vsm.xtension.mindbotssi;

/** Small support class to store several float values together with a timestamp.
 */
public class TimedFloats {

    /* Timestamp in milliseconds */
    long t;

    /* Values */
    float[] vs;

    TimedFloats(long t, float[] values) {
        this.t = t;
        this.vs = values;
    }

    TimedFloats(float[] v) {
        this(System.currentTimeMillis(), v);
    }
}
