package de.dfki.vsm.util.service;

import de.dfki.vsm.model.project.ProjectData;
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
public class Print implements Service {

    public Print(ProjectData project) {
    }

    public Protocol getProtocol() {
        return Protocol.TCP;
    }

    public void serve(byte[] in, int length) throws IOException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void serve(InputStream i, OutputStream o) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(i));
        PrintWriter out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(o)));
        String line;
        while ((line = in.readLine()) != null) {
            if (line.length() == 0) {
                break;
            }
            System.out.println(line);
        }
        out.close();
        in.close();
    }
}
