package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Edge;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.editor.dialog.ModifyTEdgeDialog;
import de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge;

//~--- JDK imports ------------------------------------------------------------

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;

/**
 * @author Gregor Mehlmann
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
        mOldTimeout = ((TimeoutEdge) mDataEdge).getTimeout();

        // Show a dialog to modify the condition
        ModifyTEdgeDialog dialog = new ModifyTEdgeDialog(((TimeoutEdge) mDataEdge));
        TimeoutEdge             tedge  = dialog.run();

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
            ((TimeoutEdge) mDataEdge).setTimeout(mOldTimeout);

            // mGUIEdge.update();
            mGUIEdge.repaint(100);
        }

        @Override
        public void redo() throws CannotRedoException {
            ((TimeoutEdge) mDataEdge).setTimeout(mNewTimeout);

            // mGUIEdge.update();
            mGUIEdge.repaint(100);
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
