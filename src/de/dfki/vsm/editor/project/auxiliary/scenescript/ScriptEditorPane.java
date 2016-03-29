package de.dfki.vsm.editor.project.auxiliary.scenescript;

import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.editor.util.VisualisationTask;
import de.dfki.vsm.model.acticon.ActiconAction;
import de.dfki.vsm.model.project.EditorConfig;
import de.dfki.vsm.model.gesticon.GesticonGesture;
import de.dfki.vsm.model.visicon.VisiconViseme;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.awt.Color;
import java.awt.Font;
import java.awt.Rectangle;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.dnd.InvalidDnDOperationException;

import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.InputVerifier;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;

/**
 * @author Gregor Mehlmann
 */
public class ScriptEditorPane extends JEditorPane implements EventListener {

    // The System Logger
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    // The Event Caster
    private final EventDispatcher mEventCaster = EventDispatcher.getInstance();

    // Activity monitor
    private Font mFont = new Font("Courier New", Font.PLAIN, 12);

    // Init Drag & Drop Support
    private DropTargetListener mDropListener;
    private DropTarget mDropTarget;
    private int mValidActions;

    //
    private final EditorProject mEditorProject;
    private final EditorConfig mEditorConfig;

    // Construct scenescript editor pane
    public ScriptEditorPane(final EditorProject project) {
        mEditorProject = project;
        mEditorConfig = mEditorProject.getEditorConfig();

        mFont = new Font(mEditorConfig.sSCRIPT_FONT_TYPE, Font.PLAIN, mEditorConfig.sSCRIPT_FONT_SIZE);
        setFont(mFont);

        // Set Lexxer And Editor
        setEditorKit(new ScriptEditorKit());

        // Set An Empty Border
        setBorder(BorderFactory.createEmptyBorder());

        // Init Drag & Drop
        initDnDSupport();

        // Allow The Focus To Leave The Pane
        // Only If The Script Syntax Is Valid
        setInputVerifier(new InputVerifier() {
            @Override
            public boolean verify(final JComponent anInput) {

                // Check If The Script Is Valid
                return true;
            }
        });

        // Register Document Actions
        // registerKeyboardAction(getDocument().getUndoAction(), KeyStroke.getKeyStroke(
        // KeyEvent.VK_Z, InputEvent.CTRL_MASK), JComponent.WHEN_FOCUSED);
        // registerKeyboardAction(getDocument().getRedoAction(), KeyStroke.getKeyStroke(
        // KeyEvent.VK_Y, InputEvent.CTRL_MASK), JComponent.WHEN_FOCUSED);
        // Register As Event Listener
        mEventCaster.register(this);
    }

    // Init Drag & Drop Support
    private void initDnDSupport() {
        mValidActions = DnDConstants.ACTION_COPY;
        mDropListener = new DropTargetListener() {
            @Override
            public void dragEnter(final DropTargetDragEvent dtde) {

                // Do Nothing Here
            }

            @Override
            public void dragExit(final DropTargetEvent dte) {

                // Do Nothing Here
            }

            @Override
            public void dropActionChanged(final DropTargetDragEvent dtde) {

                // Do Nothing Here
            }

            @Override
            public void dragOver(final DropTargetDragEvent dtde) {
                Object data = null;
                DataFlavor flavor = null;

                try {
                    try {
                        flavor = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType);
                    } catch (ClassNotFoundException exc) {
                        exc.printStackTrace();
                    }

                    data = dtde.getTransferable().getTransferData(flavor);
                } catch (UnsupportedFlavorException | IOException exc) {
                    exc.printStackTrace();
                    dtde.rejectDrag();
                }

                if (data instanceof ActiconAction) {
                    dtde.acceptDrag(dtde.getDropAction());
                    setCaretPosition(viewToModel(dtde.getLocation()));
                } else if (data instanceof GesticonGesture) {
                    dtde.acceptDrag(dtde.getDropAction());
                    setCaretPosition(viewToModel(dtde.getLocation()));
                } else if (data instanceof VisiconViseme) {
                    dtde.acceptDrag(dtde.getDropAction());
                    setCaretPosition(viewToModel(dtde.getLocation()));
                } else {
                    dtde.rejectDrag();
                }
            }

            ////////////////////////////////////////////////////////////////////
            ////////////////////////////////////////////////////////////////////
            ////////////////////////////////////////////////////////////////////
            @Override
            public final void drop(final DropTargetDropEvent dtde) {
                Object data = null;
                DataFlavor flavor = null;

                try {
                    try {

                        // Get The Data Flavour
                        flavor = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType);
                    } catch (ClassNotFoundException exc) {
                        exc.printStackTrace();
                    }

                    // Transfer The Data
                    data = dtde.getTransferable().getTransferData(flavor);
                } catch (UnsupportedFlavorException | IOException exc) {
                    dtde.rejectDrop();
                }

                // Check The Data Type
                if (data instanceof ActiconAction) {

                    // Compute the Drop Position
                    int dropPosition = viewToModel(dtde.getLocation());

                    // Insert The Acticon Action
                    try {

                        // Cast The Data To An Action
                        ActiconAction action = (ActiconAction) data;

                        // Insert The Text In Document
                        getDocument().insertString(dropPosition, action.toScript(), null);

                        // Set The Caret Position
                        setCaretPosition(dropPosition);

                        // Accept the drop
                        dtde.acceptDrop(mValidActions);
                        dtde.getDropTargetContext().dropComplete(true);
                    } catch (BadLocationException | InvalidDnDOperationException exc) {
                        exc.printStackTrace();

                        // Reject The Drop
                        dtde.rejectDrop();
                    }
                } else if (data instanceof GesticonGesture) {

                    // Compute the Drop Position
                    int dropPosition = viewToModel(dtde.getLocation());

                    // Insert The Gesticon Gesture
                    try {

                        // Cast The Data To A Gesture
                        GesticonGesture gesture = (GesticonGesture) data;

                        // Insert The Text In Document
                        getDocument().insertString(dropPosition, gesture.toScript(), null);

                        // Set The Caret Position
                        setCaretPosition(dropPosition);

                        // Accept the drop
                        dtde.acceptDrop(mValidActions);
                        dtde.getDropTargetContext().dropComplete(true);
                    } catch (BadLocationException | InvalidDnDOperationException exc) {
                        exc.printStackTrace();

                        // Reject The Drop
                        dtde.rejectDrop();
                    }
                } else if (data instanceof VisiconViseme) {

                    // Compute the Drop Position
                    int modelDropPosition = viewToModel(dtde.getLocation());

                    // TODO: Do Something Here
                } else {

                    // Reject The Drop
                    dtde.rejectDrop();
                }
            }
        };

        // Set The Drop Target
        mDropTarget = new DropTarget(this, mDropListener);
    }

    // Refresh the visual appearance
    public final void refresh() {
        // Do Nothing
        mFont = new Font(mEditorConfig.sSCRIPT_FONT_TYPE, Font.PLAIN, mEditorConfig.sSCRIPT_FONT_SIZE);
        setFont(mFont);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void update(final EventObject event) {

        // Do Nothing
        mFont = new Font(mEditorConfig.sSCRIPT_FONT_TYPE, Font.PLAIN, mEditorConfig.sSCRIPT_FONT_SIZE);
        setFont(mFont);
    }

    public void append(String s) {
        try {
            Document doc = this.getDocument();
            doc.insertString(doc.getLength(), s, null);
        } catch (BadLocationException exc) {
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private final class HighlightTask extends VisualisationTask {

        private Color mColor = null;
        private Rectangle mRect = null;

        public HighlightTask(final int steps, final JComponent c, final Color col, final Rectangle rect) {
            super(steps, c);
            mColor = col;
            mRect = rect;
        }

        public int getXPos() {
            return mRect.x;
        }

        public int getYPos() {
            return mRect.y;
        }

        public int getWidth() {
            return mRect.width;
        }

        public int getHeight() {
            return mRect.height;
        }
    }
}
