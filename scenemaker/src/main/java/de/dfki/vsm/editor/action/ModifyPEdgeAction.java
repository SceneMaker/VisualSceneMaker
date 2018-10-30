package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Edge;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.editor.dialog.ModifyPEdgeDialog;
import de.dfki.vsm.model.sceneflow.chart.edge.RandomEdge;

//~--- JDK imports ------------------------------------------------------------

import java.util.ArrayList;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;

/**
 * @author Gregor Mehlmann
 */
public class ModifyPEdgeAction extends ModifyEdgeAction {
    private ArrayList<Integer> mOldProbList = new ArrayList<Integer>();
    private ArrayList<Integer> mNewProbList = new ArrayList<Integer>();

    public ModifyPEdgeAction(Edge edge, WorkSpacePanel workSpace) {
        super(edge, workSpace);
    }

    @Override
    public void run() {
        for (RandomEdge pedge : mSourceGUINode.getDataNode().getPEdgeList()) {
            mOldProbList.add(pedge.getProbability());

            // System.out.println("Old Probability " + pedge.getProbability());
        }

        ModifyPEdgeDialog dialog = new ModifyPEdgeDialog((RandomEdge) mDataEdge);
        RandomEdge             pedg   = dialog.run();

        for (RandomEdge pedge : mSourceGUINode.getDataNode().getPEdgeList()) {
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
            mWorkSpace.repaint(100);
        }

        @Override
        public void redo() throws CannotRedoException {
            for (int i = 0; i < mSourceGUINode.getDataNode().getPEdgeList().size(); i++) {
                mSourceGUINode.getDataNode().getPEdgeList().get(i).setProbability(mNewProbList.get(i));
            }

            // mGUIEdge.update();
            mWorkSpace.revalidate();
            mWorkSpace.repaint(100);
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
