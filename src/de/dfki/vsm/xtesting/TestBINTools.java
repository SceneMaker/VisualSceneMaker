package de.dfki.vsm.xtesting;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.bin.BINUtilities;
import de.dfki.vsm.util.log.LOGDefaultLogger;

/**
 * @author Gregor Mehlmann
 */
public class TestBINTools {

    // Get The System logger
    private static final LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public static void main(String args[]) {
        try {

            //
            long   x = System.currentTimeMillis();
            long   y, z;
            byte[] a, b;

            //
            a = BINUtilities.LongToBytesBE(x);
            b = BINUtilities.LongToBytesLE(x);

            //
            y = BINUtilities.BytesBEToLong(a);
            z = BINUtilities.BytesLEToLong(b);

            //
            sLogger.message("x=" + x + "(" + BINUtilities.LongToHexString(x) + "," + BINUtilities.LongToOctString(x)
                            + ")");
            sLogger.message("x=" + y + "(" + BINUtilities.LongToHexString(y) + "," + BINUtilities.LongToOctString(x)
                            + ")");
            sLogger.message("x=" + z + "(" + BINUtilities.LongToHexString(z) + "," + BINUtilities.LongToOctString(x)
                            + ")");

            //
        } catch (Exception exc) {
            exc.printStackTrace();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public static void test(String args[]) {

        // The System Logger
        final LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();

        //
        short s = 3645;
        int   i = 47484949;
        long  l = 478909020L;
        float f = 9267.9556F;

        //
        sLogger.message("s:" + BINUtilities.ShortToHexString(s));

        byte[] sbe = BINUtilities.ShortToBytesBE(s);
        byte[] sle = BINUtilities.ShortToBytesLE(s);

        sLogger.message("sbe:" + BINUtilities.BytesToHexString(sbe));
        sLogger.message("sle:" + BINUtilities.BytesToHexString(sle));

        short s1 = BINUtilities.BytesBEToShort(sbe);
        short s2 = BINUtilities.BytesLEToShort(sle);

        sLogger.message("s1:" + s1 + " " + BINUtilities.ShortToHexString(s1));
        sLogger.message("s2:" + s2 + " " + BINUtilities.ShortToHexString(s2));
        sLogger.message("l:" + BINUtilities.IntToHexString(i));

        byte[] ibe = BINUtilities.IntToBytesBE(i);
        byte[] ile = BINUtilities.IntToBytesLE(i);

        sLogger.message("ibe:" + BINUtilities.BytesToHexString(ibe));
        sLogger.message("ile:" + BINUtilities.BytesToHexString(ile));

        int i1 = BINUtilities.BytesBEToInt(ibe);
        int i2 = BINUtilities.BytesLEToInt(ile);

        sLogger.message("i1:" + i1 + " " + BINUtilities.IntToHexString(i1));
        sLogger.message("i2:" + i2 + " " + BINUtilities.IntToHexString(i2));
        sLogger.message("l:" + BINUtilities.LongToHexString(l));

        byte[] lbe = BINUtilities.LongToBytesBE(l);
        byte[] lle = BINUtilities.LongToBytesLE(l);

        sLogger.message("lbe:" + BINUtilities.BytesToHexString(lbe));
        sLogger.message("lle:" + BINUtilities.BytesToHexString(lle));

        long l1 = BINUtilities.BytesBEToLong(lbe);
        long l2 = BINUtilities.BytesLEToLong(lle);

        sLogger.message("l1:" + l1 + " " + BINUtilities.LongToHexString(l1));
        sLogger.message("l2:" + l2 + " " + BINUtilities.LongToHexString(l2));
        sLogger.message("f:" + f);

        byte[] fbe = BINUtilities.FloatToBytesBE(f);
        byte[] fle = BINUtilities.FloatToBytesLE(f);

        sLogger.message("fbe:" + BINUtilities.BytesToHexString(fbe));
        sLogger.message("fle:" + BINUtilities.BytesToHexString(fle));

        float f1 = BINUtilities.BytesBEToFloat(fbe);
        float f2 = BINUtilities.BytesLEToFloat(fle);

        sLogger.message("f1:" + f1);
        sLogger.message("f2:" + f2);
        sLogger.message("x:" + BINUtilities.BytesLEToFloat(BINUtilities.FloatToBytesLE(12.57F)));
        sLogger.message("y:" + BINUtilities.BytesBEToFloat(BINUtilities.FloatToBytesBE(52.557F)));
    }
}
