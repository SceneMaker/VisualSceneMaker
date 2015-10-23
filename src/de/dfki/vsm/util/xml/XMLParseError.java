package de.dfki.vsm.util.xml;

/**
 * @author Not me
 */
public final class XMLParseError extends Exception {

    // The parseable source object
    private final XMLParseable mObj;
    // An information message string
    private final String mMsg;

    // Construct an XML parse error
    public XMLParseError(final XMLParseable obj, final String msg) {
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
