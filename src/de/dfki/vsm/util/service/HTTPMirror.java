package de.dfki.vsm.util.service;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;

/**
 * @author Gregor Mehlmann
 */
public class HTTPMirror implements Service {

    public Protocol getProtocol() {
        return Protocol.TCP;
    }

    public void serve(byte[] in, int length) throws IOException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void serve(InputStream i, OutputStream o) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(i));
        PrintWriter out = new PrintWriter(o);
        out.print("HTTP/1.0 200 \n");
        out.print("Content-Type: text/plain\n\n");
        String line;
        while ((line = in.readLine()) != null) {
            if (line.length() == 0) {
                break;
            }
            out.print(line + "\n");
        }
        out.close();
        in.close();
    }
}
