package de.dfki.vsm.util.service;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.project.ProjectData;

//~--- JDK imports ------------------------------------------------------------

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;

/**
 * @author Gregor Mehlmann
 */
public class Echo implements Service {
    public Echo(ProjectData project) {}

    public Protocol getProtocol() {
        return Protocol.TCP;
    }

    public void serve(byte[] in, int length) throws IOException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void serve(InputStream i, OutputStream o) throws IOException {
        BufferedReader in  = new BufferedReader(new InputStreamReader(i));
        PrintWriter    out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(o)));

        out.println(in.readLine());
        out.flush();
        out.close();
        in.close();
    }
}
