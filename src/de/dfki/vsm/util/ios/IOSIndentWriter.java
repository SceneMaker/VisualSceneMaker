package de.dfki.vsm.util.ios;

//~--- JDK imports ------------------------------------------------------------

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;

/**
 * @author Not me
 */
public class IOSIndentWriter {
    private String            mIndent  = "  ";
    private String            mStack   = "";
    private int               mLine    = 0;
    private boolean           mNewline = true;
    private final PrintWriter mStream;

    public IOSIndentWriter(File file) throws IOException {
        mStream = new PrintWriter(file, "UTF-8");
    }

    public IOSIndentWriter(OutputStream stream) {
        mStream = new PrintWriter(stream);
    }

    public IOSIndentWriter(File file, String indent) throws IOException {
        mStream = new PrintWriter(file, "UTF-8");
        mIndent = indent;
    }

    public IOSIndentWriter(OutputStream stream, String indent) {
        mStream = new PrintWriter(stream);
        mIndent = indent;
    }

    public IOSIndentWriter print(String s) {

        // split the string into lines
        int start  = 0,
            i      = 0,
            length = s.length();

        for (i = 0; i < length; ++i) {
            switch (s.charAt(i)) {

            // line break
            case '\n' :
                ++mLine;
                mNewline = true;
                mStream.print(s.substring(start, i));
                mStream.print('\n');
                start = i + 1;

                break;

            // ignore carriage-returns
            case '\r' :
                mStream.print(s.substring(start, i));
                start = i + 1;

                break;

            // indent
            default :
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

    public IOSIndentWriter println(String s) {
        return print(s).endl();
    }

    public IOSIndentWriter print(Object o) {
        return print(o.toString());
    }

    public IOSIndentWriter println(Object o) {
        return print(o.toString()).endl();
    }

    public final IOSIndentWriter endl() {
        ++mLine;
        mNewline = true;
        mStream.print('\n');
        mStream.flush();

        return this;
    }

    public final IOSIndentWriter flush() {
        mStream.flush();

        return this;
    }

    public final IOSIndentWriter push() {
        mStack += mIndent;

        return this;
    }

    public final IOSIndentWriter pop() {
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
