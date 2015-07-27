package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Edge;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.editor.dialog.ModifyTEdgeDialog;
import de.dfki.vsm.model.sceneflow.TEdge;

//~--- JDK imports ------------------------------------------------------------

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;

/**
 * @author Not me
 */
public class ModifyTEdgeAction extends ModifyEdgeAction {
    private long mOldTimeout;
    private long mNewTimeout;

    public ModifyTEdgeAction(Edge edge, WorkSpacePanel workSpace) {
        super(edge, workSpace);
    }

    @Override
    public void run() {

        // Remember the old condition
        mOldTimeout = ((TEdge) mDataEdge).getTimeout();

        // Show a dialog to modify the condition
        ModifyTEdgeDialog dialog = new ModifyTEdgeDialog(((TEdge) mDataEdge));
        TEdge             tedge  = dialog.run();

        // If the condition was successfully modified then
        // remember the new condition and update the undomanager
        if (tedge != null) {
            mNewTimeout = tedge.getTimeout();
            mUndoManager.addEdit(new Edit());
            UndoAction.getInstance().refreshUndoState();
            RedoAction.getInstance().refreshRedoState();
        }
    }

    private class Edit extends AbstractUndoableEdit {
        @Override
        public void undo() throws CannotUndoException {
            ((TEdge) mDataEdge).setTimeout(mOldTimeout);

            // mGUIEdge.update();
            mGUIEdge.repaint();
        }

        @Override
        public void redo() throws CannotRedoException {
            ((TEdge) mDataEdge).setTimeout(mNewTimeout);

            // mGUIEdge.update();
            mGUIEdge.repaint();
        }

        @Override
        public boolean canUndo() {
            return true;
        }

        @Override
        public boolean canRedo() {
            return true;
        }

        @Override
        public String getUndoPresentationName() {
            return "Undo modification of TEdge";
        }

        @Override
        public String getRedoPresentationName() {
            return "Redo modification of TEdge";
        }
    }
}
