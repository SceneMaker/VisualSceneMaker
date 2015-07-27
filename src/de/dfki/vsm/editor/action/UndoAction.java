package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.EditorInstance;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.KeyStroke;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.UndoManager;

/**
 *
 * @author Not me
 */
public class UndoAction extends AbstractAction {
    private static UndoAction sSingeltonInstance = null;

    private UndoAction() {
        super("Undo");
        putValue(ACCELERATOR_KEY,
                 KeyStroke.getKeyStroke(KeyEvent.VK_Z, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        setEnabled(false);
    }

    public static UndoAction getInstance() {
        if (sSingeltonInstance == null) {
            sSingeltonInstance = new UndoAction();
        }

        return sSingeltonInstance;
    }

    public void actionPerformed(ActionEvent evt) {
        UndoManager manager = EditorInstance.getInstance().getSelectedProjectEditor().getSceneFlowEditor().getUndoManager();

        try {
            manager.undo();
        } catch (CannotUndoException e) {
            e.printStackTrace();
        }

        refreshUndoState();
        RedoAction.getInstance().refreshRedoState();

        /*
         * try {
         * UndoRedoManager.getInstance().undo();
         * } catch (CannotUndoException exc) {
         * exc.printStackTrace();
         * }
         * refreshUndoState();
         * RedoAction.getInstance().refreshRedoState();
         */
    }

    public void refreshUndoState() {
        UndoManager manager = EditorInstance.getInstance().getSelectedProjectEditor().getSceneFlowEditor().getUndoManager();

        if (manager.canUndo()) {
            setEnabled(true);
            putValue(Action.NAME, manager.getUndoPresentationName());
        } else {
            setEnabled(false);
            putValue(Action.NAME, "Undo");
        }

        /*
         * if (UndoRedoManager.getInstance().canUndo()) {
         * setEnabled(true);
         * putValue(Action.NAME, UndoRedoManager.getInstance().getUndoPresentationName());
         * } else {
         * setEnabled(false);
         * putValue(Action.NAME, "Undo");
         * }
         */
    }
    
}
