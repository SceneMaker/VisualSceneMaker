package de.dfki.vsm.xtension.ssi.logger;

/**
 * @author Gregor Mehlmann
 */
public final class SSILoggerTimer {

    private static long mInit = System.currentTimeMillis();

    public static void init() {
        mInit = System.currentTimeMillis();
    }

    public static int time() {
        return java.lang.Math.toIntExact(System.currentTimeMillis() - mInit);
    }

}
