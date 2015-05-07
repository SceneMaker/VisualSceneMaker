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
public class Reverse implements Service {
    public Reverse(ProjectData project) {}

    public Protocol getProtocol() {
        return Protocol.TCP;
    }

    public void serve(byte[] in, int length) throws IOException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void serve(InputStream i, OutputStream o) throws IOException {
        BufferedReader in  = new BufferedReader(new InputStreamReader(i));
        PrintWriter    out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(o)));

        out.print("Welcome to the line reversal server.\n");
        out.print("Enter lines.  End with a '.' on a line by itself.\n");

        for (;;) {
            out.print("> ");
            out.flush();

            String line = in.readLine();

            if ((line == null) || line.equals(".")) {
                break;
            }

            for (int j = line.length() - 1; j >= 0; j--) {
                out.print(line.charAt(j));
            }

            out.print("\n");
        }

        out.close();
        in.close();
    }
}
