package de.dfki.vsm.util.ios;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;

/**
 * @author Gregor Mehlmann
 */
public class IndentWriter {

    private final PrintWriter mStream;
    private String mIndent = "  ";
    private String mStack = "";
    private int mLine = 0;
    private boolean mNewline = true;

    public IndentWriter(File file) throws IOException {
        mStream = new PrintWriter(file, "UTF-8");

    }

    public IndentWriter(File file, String indent) throws IOException {
        mStream = new PrintWriter(file, "UTF-8");
        mIndent = indent;
    }

    public IndentWriter(OutputStream stream) {
        mStream = new PrintWriter(stream);
    }

    public IndentWriter(OutputStream stream, String indent) {
        mStream = new PrintWriter(stream);
        mIndent = indent;
    }

    public IndentWriter print(String s) {
        // split the string into lines
        int start = 0, i = 0, length = s.length();
        for (i = 0; i < length; ++i) {
            switch (s.charAt(i)) {
                // line break
                case '\n':
                    ++mLine;
                    mNewline = true;
                    mStream.print(s.substring(start, i));
                    mStream.print('\n');
                    start = i + 1;
                    break;
                // ignore carriage-returns
                case '\r':
                    mStream.print(s.substring(start, i));
                    start = i + 1;
                    break;
                // indent
                default:
                    indent();
                    break;
            }
            // print the rest of the string
        }
        if (start < i) {
            mStream.print(s.substring(start, i));
        }
        return this;
    }

    public IndentWriter println(String s) {
        return print(s).endl();
    }

    public IndentWriter print(Object o) {
        return print(o.toString());
    }

    public IndentWriter println(Object o) {
        return print(o.toString()).endl();
    }

    public final IndentWriter endl() {
        ++mLine;
        mNewline = true;
        mStream.print('\n');
        mStream.flush();
        return this;
    }

    public final IndentWriter flush() {
        mStream.flush();
        return this;
    }

    public final IndentWriter push() {
        mStack += mIndent;
        return this;
    }

    public final IndentWriter pop() {
        if (mStack.length() >= mIndent.length()) {
            mStack = mStack.substring(0, mStack.length() - mIndent.length());
        }
        return this;
    }

    public final void close() {
        mStream.close();
    }

    private void indent() {
        if (mNewline) {
            mStream.print(mStack);
            mNewline = false;
        }
    }
}
