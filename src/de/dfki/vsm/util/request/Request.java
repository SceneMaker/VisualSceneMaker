package de.dfki.vsm.util.request;

//~--- JDK imports ------------------------------------------------------------

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * @author Gregor Mehlmann
 */
public interface Request {
    enum Protocol { UDP, TCP }

    public Protocol getProtocol();

    public void request(InputStream in, OutputStream out) throws IOException;

    public void request(byte[] in, int length) throws IOException;
}
