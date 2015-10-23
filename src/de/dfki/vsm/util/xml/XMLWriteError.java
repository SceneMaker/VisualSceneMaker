package de.dfki.vsm.util.xml;

/**
 * @author Not me
 */
public class XMLWriteError extends Exception {

    // The writeable source object
    private final XMLWriteable mObj;

    // An information message string
    private final String mMsg;

    // Construct an XML write error
    public XMLWriteError(final XMLWriteable obj, final String msg) {
        mObj = obj;
        mMsg = msg;
    }

    // Get the message of the error
    public final String getMsg() {
        return mMsg;
    }

    // Get the source of the error
    public final Object getObj() {
        return mObj;
    }
}
