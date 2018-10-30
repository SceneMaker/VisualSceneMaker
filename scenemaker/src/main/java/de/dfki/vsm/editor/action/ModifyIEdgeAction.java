package de.dfki.vsm.editor.action;

import de.dfki.vsm.editor.Edge;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.editor.dialog.ModifyIEdgeDialog;
import de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;

/**
 *
 * @author Gregor Mehlmann
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
        mOldCondition = ((InterruptEdge) mDataEdge).getCondition();

        // Show a dialog to modify the condition
        ModifyIEdgeDialog dialog = new ModifyIEdgeDialog(((InterruptEdge) mDataEdge));
        InterruptEdge             iedge  = dialog.run();

        // If the condition was successfully modified then
        // remember the new condition and update the undomanager
        if (iedge != null) {
            mNewCondition = iedge.getCondition();
            mUndoManager.addEdit(new Edit());
            UndoAction.getInstance().refreshUndoState();
            RedoAction.getInstance().refreshRedoState();
        }
    }

    private class Edit extends AbstractUndoableEdit {
        @Override
        public void undo() throws CannotUndoException {
            ((InterruptEdge) mDataEdge).setCondition(mOldCondition);

            // mGUIEdge.update();
            mGUIEdge.repaint(100);
        }

        @Override
        public void redo() throws CannotRedoException {
            ((InterruptEdge) mDataEdge).setCondition(mNewCondition);

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
            return "Undo modification of IEdge";
        }

        @Override
        public String getRedoPresentationName() {
            return "Redo modification of IEdge";
        }
    }
}
