package de.dfki.vsm.util.syn;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.log.LOGDefaultLogger;

import java_cup.runtime.Scanner;

//~--- JDK imports ------------------------------------------------------------

import java.io.CharArrayReader;
import java.io.Reader;

import java.util.LinkedList;

import javax.swing.text.Segment;

/**
 * @author Gregor Mehlmann
 */
public abstract class SyntaxDocLexxer implements Scanner {

    // Get The System Logger
    protected final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    // Comment Generation Flag
    protected boolean mComment;

    // Whitespace Generation Flag
    protected boolean mWhiteSpace;

    // Newline Generation Flag
    protected boolean mNewline;

    // The Token Object Index
    protected int mTokenIndex;

    // The Last Type Of Token
    protected int mLastToken;

    // The Last Lexxer State
    protected int mLastState;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public SyntaxDocLexxer() {

        // Do Nothing Here
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public abstract SyntaxDocSymbol next_token() throws Exception;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public abstract void init(final Reader reader, final boolean comment, final boolean newline,
                              final boolean whitespace);

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public LinkedList<SyntaxDocSymbol> scan_token_list(final Segment segment, final int offset) {

        // Create A New List Of Symbols
        final LinkedList<SyntaxDocSymbol> list = new LinkedList<>();

        // Create A New Character Reader
        final CharArrayReader reader = new CharArrayReader(segment.array, segment.offset, segment.count);

        // Reset Lexxer With The Reader
        init(reader, true, true, true);

        // Scan All The Syntax Symbols
        try {
            SyntaxDocSymbol symbol;

            do {

                // Get A New Symbol
                symbol = next_token();

                // Add The Symbol
                if (symbol != null) {
                    list.add(symbol);
                }
            } while (symbol != null);
        } catch (Exception exc) {
            mLogger.message(exc.toString());
        }

        // Return The Symbol List
        return list;
    }
}
