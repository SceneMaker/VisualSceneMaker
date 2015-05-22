package de.dfki.vsm.util.bin;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------

import javax.xml.bind.DatatypeConverter;

/**
 * @author Gregor Mehlmann
 */
public final class BINUtilities {

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final static byte[] ShortToBytesLE(final long val) {
        byte[] buf = new byte[2];

        for (int i = 0; i < 2; i++) {
            buf[i] = (byte) (val >>> (8 * i));
        }

        return buf;
    }

    public final static byte[] IntToBytesLE(final long val) {
        byte[] buf = new byte[4];

        for (int i = 0; i < 4; i++) {
            buf[i] = (byte) (val >>> (8 * i));
        }

        return buf;
    }

    public final static byte[] LongToBytesLE(final long val) {
        byte[] buf = new byte[8];

        for (int i = 0; i < 8; i++) {
            buf[i] = (byte) (val >>> (8 * i));
        }

        return buf;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final static byte[] ShortToBytesBE(final long val) {
        byte[] buf = new byte[2];

        for (int i = 0; i < 2; i++) {
            buf[i] = (byte) (val >>> ((1 - i) * 8));
        }

        return buf;
    }

    public final static byte[] IntToBytesBE(final long val) {
        byte[] buf = new byte[4];

        for (int i = 0; i < 4; i++) {
            buf[i] = (byte) (val >>> ((3 - i) * 8));
        }

        return buf;
    }

    public final static byte[] LongToBytesBE(final long val) {
        byte[] buf = new byte[8];

        for (int i = 0; i < 8; i++) {
            buf[i] = (byte) (val >>> ((7 - i) * 8));
        }

        return buf;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final static short BytesLEToShort(final byte[] buf) {
        short val = 0x0;

        for (int i = 0; i < buf.length; i++) {
            val += ((short) buf[i] & 0xFF) << (8 * i);
        }

        return val;
    }

    public final static int BytesLEToInt(final byte[] buf) {
        int val = 0x0;

        for (int i = 0; i < buf.length; i++) {
            val += ((int) buf[i] & 0xFF) << (8 * i);
        }

        return val;
    }

    public final static long BytesLEToLong(final byte[] buf) {
        long val = 0x0L;

        for (int i = 0; i < buf.length; i++) {
            val += ((long) buf[i] & 0xFFL) << (8 * i);
        }

        return val;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final static short BytesBEToShort(final byte[] buf) {
        short val = 0x0;

        for (int i = 0; i < buf.length; i++) {
            val += ((short) buf[i] & 0xFF) << ((1 - i) * 8);
        }

        return val;
    }

    public final static int BytesBEToInt(final byte[] buf) {
        int val = 0x0;

        for (int i = 0; i < buf.length; i++) {
            val += ((int) buf[i] & 0xFF) << ((3 - i) * 8);
        }

        return val;
    }

    public final static long BytesBEToLong(final byte[] buf) {
        long val = 0x0L;

        for (int i = 0; i < buf.length; i++) {
            val += ((long) buf[i] & 0xFFL) << ((7 - i) * 8);
        }

        return val;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final static String ShortToHexString(final short value) {
        final String string = Integer.toString(value, 16).toUpperCase();

        if (string.charAt(0) == '-') {
            return "-0X" + string.substring(1, string.length()) + "S";
        } else {
            return "0X" + string + "S";
        }
    }

    public final static String IntToHexString(final int value) {
        final String string = Integer.toString(value, 16).toUpperCase();

        if (string.charAt(0) == '-') {
            return "-0X" + string.substring(1, string.length());
        } else {
            return "0X" + string;
        }
    }

    public final static String LongToHexString(final long value) {
        final String string = Long.toString(value, 16).toUpperCase();

        if (string.charAt(0) == '-') {
            return "-0X" + string.substring(1, string.length()) + "L";
        } else {
            return "0X" + string + "L";
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final static String ShortToOctString(final short value) {
        final String string = Integer.toString(value, 8).toUpperCase();

        if (string.charAt(0) == '-') {
            return "-0" + string.substring(1, string.length()) + "S";
        } else {
            return "0" + string + "S";
        }
    }

    public final static String IntToOctString(final int value) {
        final String string = Integer.toString(value, 8).toUpperCase();

        if (string.charAt(0) == '-') {
            return "-0" + string.substring(1, string.length());
        } else {
            return "0" + string;
        }
    }

    public final static String LongToOctString(final long value) {
        final String string = Long.toString(value, 8).toUpperCase();

        if (string.charAt(0) == '-') {
            return "-0" + string.substring(1, string.length()) + "L";
        } else {
            return "0" + string + "L";
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final static String BytesToHexString(final byte[] value) {
        return DatatypeConverter.printHexBinary(value);
    }

    public final static String BytesToBase64String(final byte[] value) {
        return DatatypeConverter.printBase64Binary(value);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final static byte[] FloatToBytesBE(final float value) {
        return IntToBytesBE(Float.floatToIntBits(value));
    }

    public final static byte[] FloatToBytesLE(final float value) {
        return IntToBytesLE(Float.floatToIntBits(value));
    }

    public final static float BytesLEToFloat(final byte[] value) {
        return Float.intBitsToFloat(BytesLEToInt(value));
    }

    public final static float BytesBEToFloat(final byte[] value) {
        return Float.intBitsToFloat(BytesBEToInt(value));
    }
}
