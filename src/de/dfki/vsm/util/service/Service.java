package de.dfki.vsm.util.service;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * @author Gregor Mehlmann
 */
public interface Service {

    enum Protocol {

        UDP, TCP
    }

    public Protocol getProtocol();

    public void serve(InputStream in, OutputStream out) throws IOException;

    public void serve(byte[] in, int length) throws IOException;
}
