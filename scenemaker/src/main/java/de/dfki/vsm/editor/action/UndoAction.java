package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.event.ProjectChangedEvent;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.log.LOGDefaultLogger;

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
 * @author Gregor Mehlmann
 */
public class UndoAction extends AbstractAction {
    // The singelton logger instance   
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    
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
            mLogger.failure(e.getMessage());
        }

        refreshUndoState();
        RedoAction.getInstance().refreshRedoState();
        EventDispatcher.getInstance().convey(new ProjectChangedEvent(this));
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
