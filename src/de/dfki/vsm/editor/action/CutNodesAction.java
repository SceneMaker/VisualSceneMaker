package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Node;
import de.dfki.vsm.editor.project.sceneflow.SceneFlowEditor;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;

//~--- JDK imports ------------------------------------------------------------

import java.util.HashSet;
import java.util.Set;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;

/**
 * @author Patrick Gebhard
 */
public class CutNodesAction extends EditorAction {
    Set<Node>             mNodes             = new HashSet<Node>();
    Set<RemoveNodeAction> mRemoveNodeActions = new HashSet<RemoveNodeAction>();
    CopyNodesAction       mCopyNodesAction   = null;
    WorkSpacePanel             mWorkSpace         = null;
    SceneFlowEditor       mSceneFlowEditor;

    public CutNodesAction(WorkSpacePanel workSpace, Node node) {
        mWorkSpace = workSpace;
        mNodes.add(node);
    }

    public CutNodesAction(WorkSpacePanel workSpace, Set<Node> nodes) {
        mWorkSpace       = workSpace;
        mNodes           = nodes;
        mSceneFlowEditor = mWorkSpace.getSceneFlowEditor();
    }

    protected void deleteNodes() {

        // first copy nodes
        mCopyNodesAction = new CopyNodesAction(mWorkSpace, mNodes);
        mCopyNodesAction.run();

        // second remove nodes
        for (Node node : mNodes) {
            if (!node.getDataNode().isHistoryNode()) {
                RemoveNodeAction rma = new RemoveNodeAction(mWorkSpace, node);

                mRemoveNodeActions.add(rma);
                rma.run();
            } else {
                mSceneFlowEditor.setMessageLabelText("History node will not be removed!");
            }
        }
    }

    protected void createNodes() {

        // first recreate nodes
        for (RemoveNodeAction action : mRemoveNodeActions) {
            action.create();
        }

        // second uncopy nodes
        mCopyNodesAction.uncopyNodes();
    }

    public void run() {
        mWorkSpace.deselectAllNodes();
        deleteNodes();
        UndoAction.getInstance().refreshUndoState();
        RedoAction.getInstance().refreshRedoState();
    }

    private class Edit extends AbstractUndoableEdit {
        @Override
        public void undo() throws CannotUndoException {
            createNodes();
        }

        @Override
        public void redo() throws CannotRedoException {
            deleteNodes();
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
            return "Undo Deletion Of Nodes ";
        }

        @Override
        public String getRedoPresentationName() {
            return "Redo Deletion Of Nodes ";
        }
    }
}
