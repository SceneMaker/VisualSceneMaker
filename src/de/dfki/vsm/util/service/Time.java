package de.dfki.vsm.util.service;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.project.ProjectData;

//~--- JDK imports ------------------------------------------------------------

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;

import java.util.Date;

/**
 * @author Gregor Mehlmann
 */
public class Time implements Service {
    public Time(ProjectData project) {}

    public Protocol getProtocol() {
        return Protocol.TCP;
    }

    public void serve(byte[] in, int length) throws IOException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void serve(InputStream i, OutputStream o) throws IOException {
        PrintWriter out = new PrintWriter(o);

        out.print(new Date() + "\n");
        out.close();
        i.close();
    }
}
