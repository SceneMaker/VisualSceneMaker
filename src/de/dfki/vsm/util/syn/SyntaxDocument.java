package de.dfki.vsm.util.syn;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------

import java.awt.event.ActionEvent;

import java.util.LinkedList;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.event.DocumentEvent;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.PlainDocument;
import javax.swing.text.Segment;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.UndoManager;

/**
 * @author Not me
 */
public class SyntaxDocument extends PlainDocument implements UndoableEditListener {

    // The Singelton Logger
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    // The List Of Tokens
    public LinkedList<SyntaxDocSymbol> mSymbolList = new LinkedList<>();

    // Redo / Undo Helpers
    private final UndoManager mUndoManager = new UndoManager();
    private final UndoAction  mUndoAction  = new UndoAction(mUndoManager);
    private final RedoAction  mRedoAction  = new RedoAction(mUndoManager);

    // The Document Lexxer
    private final SyntaxDocLexxer mLexxer;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public SyntaxDocument(final SyntaxDocLexxer lexxer) {

        // Initialize The Syntax Lexxer
        mLexxer = lexxer;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final LinkedList<SyntaxDocSymbol> getSymbolList() {
        return mSymbolList;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final LinkedList<SyntaxDocSymbol> getSymbolList(final int lower, final int upper) {

        // Get A New List Of Symbols
        final LinkedList<SyntaxDocSymbol> list = new LinkedList<>();

        // Copy All Relevant Symbols
        for (final SyntaxDocSymbol symbol : mSymbolList) {
            final SyntaxDocToken token = symbol.getValue();

            //
            if ((token.getUpper() > lower) && (token.getLower() < upper)) {
                list.add(symbol);
            }
        }

        return list;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    protected void fireChangedUpdate(final DocumentEvent e) {

        // Scan The Document Content
        loadSymbolList();
        super.fireChangedUpdate(e);
        mLogger.message("fireChangedUpdate");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    protected void fireInsertUpdate(final DocumentEvent e) {

        // Scan The Document Content
        loadSymbolList();
        super.fireInsertUpdate(e);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    protected void fireRemoveUpdate(final DocumentEvent e) {

        // Scan The Document Content
        loadSymbolList();
        super.fireRemoveUpdate(e);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private void loadSymbolList() {

        // mLogger.message("Scanning Scenecript");
        try {

            // Create A New Text Segment
            final Segment segment = new Segment();

            // Fill The New Text Segment
            getText(0, getLength(), segment);

            // Scan The Segment Of Text
            mSymbolList = mLexxer.scan_token_list(segment, 0);
        } catch (Exception exc) {

            // Catch Error Or Exception
            mLogger.failure(exc.toString());

            // Return False At Failure
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final int[] getPosition(final int offset) {

        // Get The Line and Column For An offset
        int data[] = { 1, 1 };

        try {
            for (int i = 0; i < offset; i++) {
                final String text = getText(i, 1);

                if (text.equals("\n")) {
                    data[0]++;
                    data[1] = 0;
                } else {
                    data[1]++;
                }
            }
        } finally {
            return data;
        }
    }

    // Get The Line and Column For Position
    public final int[] getLineOf(final int position) {
        int data[] = { 1, 1 };

        try {
            for (int i = 0; i < position; i++) {
                if (getText(i, 1).equals(System.getProperty("line.seperator"))) {
                    data[0]++;
                    data[1] = 0;
                }

                data[1] = 0;
            }
        } finally {
            return data;
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final int getLowerNewLine(final int offset) {
        try {

            // Start Searching From The Offset
            int i = offset;

            // As Long As Start Is Not Reached
            while (i > 0) {

                // Read The Left Character
                final String text = getText(i - 1, 1);

                if (text.equals("\n")) {

                    // Return Position Before Newline
                    return i - 1;
                } else {

                    // Decrement The Read Mark
                    i--;
                }
            }
        } catch (Exception exc) {
            exc.printStackTrace();
        }

        // Return The Start Of File
        return 0;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final int getUpperNewLine(final int offset) {
        try {

            // Start Searching From The Offset
            int i = offset;

            // As Long As End Is Not Reached
            while (i < getLength()) {

                // Read The Right Character
                final String text = getText(i, 1);

                if (text.equals("\n")) {

                    // Return Position After Newline
                    return i + 1;
                } else {

                    // Increment The Read Mark
                    i++;
                }
            }
        } catch (Exception exc) {
            exc.printStackTrace();
        }

        // Return The End Of File
        return getLength();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final AbstractAction getUndoAction() {
        return mUndoAction;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final AbstractAction getRedoAction() {
        return mRedoAction;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void undoableEditHappened(final UndoableEditEvent e) {
        mUndoManager.addEdit(e.getEdit());
        mUndoAction.updateUndoState();
        mRedoAction.updateRedoState();
    }

    ////////////////////////////////////////////////////////////////////////////
//  //////////////////////////////////////////////////////////////////////////
//  //////////////////////////////////////////////////////////////////////////
    private class RedoAction extends AbstractAction {

        // The Undo Manager
        private final UndoManager mManager;

        ////////////////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////////////////
        private RedoAction(final UndoManager manager) {
            super("Redo");

            // Initialize Manager
            mManager = manager;

            // Set Action Disabled
            setEnabled(false);
        }

        ////////////////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////////////////
        @Override
        public void actionPerformed(ActionEvent evt) {
            try {
                mManager.redo();

                // TODO: PARSE
            } catch (CannotRedoException e) {
                e.printStackTrace();
            }

            updateRedoState();
            mUndoAction.updateUndoState();
        }

        ////////////////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////////////////
        private void updateRedoState() {
            if (mManager.canRedo()) {
                setEnabled(true);
                putValue(Action.NAME, mManager.getRedoPresentationName());
            } else {
                setEnabled(false);
                putValue(Action.NAME, "Redo");
            }
        }
    }


    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private class UndoAction extends AbstractAction {

        // The Undo Manager
        private final UndoManager mManager;

        ////////////////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////////////////
        private UndoAction(final UndoManager manager) {
            super("Undo");

            // Initialize Manager
            mManager = manager;

            // Set Action Disabled
            setEnabled(false);
        }

        ////////////////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////////////////
        @Override
        public void actionPerformed(ActionEvent evt) {
            try {
                mManager.undo();

                // TODO: PARSE
            } catch (CannotUndoException e) {
                e.printStackTrace();
            }

            updateUndoState();
            mRedoAction.updateRedoState();
        }

        ////////////////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////////////////
        private void updateUndoState() {
            if (mManager.canUndo()) {
                setEnabled(true);
                putValue(Action.NAME, mManager.getUndoPresentationName());
            } else {
                setEnabled(false);
                putValue(Action.NAME, "Undo");
            }
        }
    }
}
