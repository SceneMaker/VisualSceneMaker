package de.dfki.vsm.util.service;

import de.dfki.vsm.model.project.ProjectData;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;

/**
 * @author Gregor Mehlmann
 */
public class UniqueID implements Service {

    public volatile int id = 0;

    public UniqueID(ProjectData project) {
    }

    public synchronized int nextId() {
        return id++;
    }

    public Protocol getProtocol() {
        return Protocol.TCP;
    }

    public void serve(byte[] in, int length) throws IOException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void serve(InputStream i, OutputStream o) throws IOException {
        PrintWriter out = new PrintWriter(o);
        out.print("You are client #: " + nextId() + "\n");
        out.close();
        i.close();
    }
}
