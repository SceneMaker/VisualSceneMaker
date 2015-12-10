package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Edge;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.editor.dialog.ModifyPEdgeDialog;
import de.dfki.vsm.model.sceneflow.PEdge;

//~--- JDK imports ------------------------------------------------------------

import java.util.Vector;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;

/**
 * @author Not me
 */
public class ModifyPEdgeAction extends ModifyEdgeAction {
    private Vector<Integer> mOldProbList = new Vector<Integer>();
    private Vector<Integer> mNewProbList = new Vector<Integer>();

    public ModifyPEdgeAction(Edge edge, WorkSpacePanel workSpace) {
        super(edge, workSpace);
    }

    @Override
    public void run() {
        for (PEdge pedge : mSourceGUINode.getDataNode().getPEdgeList()) {
            mOldProbList.add(pedge.getProbability());

            // System.out.println("Old Probability " + pedge.getProbability());
        }

        ModifyPEdgeDialog dialog = new ModifyPEdgeDialog((PEdge) mDataEdge);
        PEdge             pedg   = dialog.run();

        for (PEdge pedge : mSourceGUINode.getDataNode().getPEdgeList()) {
            mNewProbList.add(pedge.getProbability());

            // System.out.println("New Probability " + pedge.getProbability());
        }

        // Update Redo/Undo state
        mUndoManager.addEdit(new Edit());
        UndoAction.getInstance().refreshUndoState();
        RedoAction.getInstance().refreshRedoState();
    }

    private class Edit extends AbstractUndoableEdit {
        @Override
        public void undo() throws CannotUndoException {
            for (int i = 0; i < mSourceGUINode.getDataNode().getPEdgeList().size(); i++) {
                mSourceGUINode.getDataNode().getPEdgeList().get(i).setProbability(mOldProbList.get(i));
            }

            // mGUIEdge.update();
            mWorkSpace.revalidate();
            mWorkSpace.repaint();
        }

        @Override
        public void redo() throws CannotRedoException {
            for (int i = 0; i < mSourceGUINode.getDataNode().getPEdgeList().size(); i++) {
                mSourceGUINode.getDataNode().getPEdgeList().get(i).setProbability(mNewProbList.get(i));
            }

            // mGUIEdge.update();
            mWorkSpace.revalidate();
            mWorkSpace.repaint();
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
            return "Undo Modification Of PEdge";
        }

        @Override
        public String getRedoPresentationName() {
            return "Redo Modification Of PEdge";
        }
    }
}
