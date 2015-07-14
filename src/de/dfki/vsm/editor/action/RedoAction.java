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
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.UndoManager;

/**
 *
 * @author Not me
 */
public class RedoAction extends AbstractAction {
    private static RedoAction sSingeltonInstance = null;

    private RedoAction() {
        super("Redo");
        putValue(ACCELERATOR_KEY,
                 KeyStroke.getKeyStroke(KeyEvent.VK_Z,
                                        (java.awt.event.InputEvent.SHIFT_MASK
                                         | Toolkit.getDefaultToolkit().getMenuShortcutKeyMask())));
        setEnabled(false);
    }

    public static RedoAction getInstance() {
        if (sSingeltonInstance == null) {
            sSingeltonInstance = new RedoAction();
        }

        return sSingeltonInstance;
    }

    public void actionPerformed(ActionEvent evt) {
        UndoManager manager = EditorInstance.getInstance().getSelectedProjectEditor().getSceneFlowEditor().getUndoManager();

        try {
            manager.redo();
        } catch (CannotRedoException e) {
            e.printStackTrace();
        }

        refreshRedoState();
        UndoAction.getInstance().refreshUndoState();

        /*
         * try {
         * UndoRedoManager.getInstance().redo();
         * } catch (CannotRedoException exc) {
         * exc.printStackTrace();
         * }
         * refreshRedoState();
         * UndoAction.getInstance().refreshUndoState();
         */
    }

    public void refreshRedoState() {
        UndoManager manager = EditorInstance.getInstance().getSelectedProjectEditor().getSceneFlowEditor().getUndoManager();

        if (manager.canRedo()) {
            setEnabled(true);
            putValue(Action.NAME, manager.getRedoPresentationName());
        } else {
            setEnabled(false);
            putValue(Action.NAME, "Redo");
        }

        /*
         * if (UndoRedoManager.getInstance().canRedo()) {
         * setEnabled(true);
         * putValue(Action.NAME, UndoRedoManager.getInstance().getRedoPresentationName());
         * } else {
         * setEnabled(false);
         * putValue(Action.NAME, "Redo");
         * }
         */
    }
}
