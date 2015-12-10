package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Edge;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.editor.dialog.ModifyIEdgeDialog;
import de.dfki.vsm.model.sceneflow.diagram.edges.InterruptEdge;
import de.dfki.vsm.model.sceneflow.language.command.Expression;

//~--- JDK imports ------------------------------------------------------------

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;

/**
 *
 * @author Not me
 */
public class ModifyIEdgeAction extends ModifyEdgeAction {
    private Expression mOldCondition;
    private Expression mNewCondition;

    public ModifyIEdgeAction(Edge edge, WorkSpacePanel workSpace) {
        super(edge, workSpace);
    }

    @Override
    public void run() {

        // Remember the old condition
        mOldCondition = ((InterruptEdge) mDataEdge).getGuard();

        // Show a dialog to modify the condition
        ModifyIEdgeDialog dialog = new ModifyIEdgeDialog(((InterruptEdge) mDataEdge));
        InterruptEdge             iedge  = dialog.run();

        // If the condition was successfully modified then
        // remember the new condition and update the undomanager
        if (iedge != null) {
            mNewCondition = iedge.getGuard();
            mUndoManager.addEdit(new Edit());
            UndoAction.getInstance().refreshUndoState();
            RedoAction.getInstance().refreshRedoState();
        }
    }

    private class Edit extends AbstractUndoableEdit {
        @Override
        public void undo() throws CannotUndoException {
            ((InterruptEdge) mDataEdge).setGuard(mOldCondition);

            // mGUIEdge.update();
            mGUIEdge.repaint();
        }

        @Override
        public void redo() throws CannotRedoException {
            ((InterruptEdge) mDataEdge).setGuard(mNewCondition);

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
            return "Undo modification of IEdge";
        }

        @Override
        public String getRedoPresentationName() {
            return "Redo modification of IEdge";
        }
    }
}
