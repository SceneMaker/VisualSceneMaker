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
public class CopyNodesAction extends EditorAction {
    Set<Node>             mNodes             = new HashSet<Node>();
    Set<RemoveNodeAction> mRemoveNodeActions = new HashSet<RemoveNodeAction>();
    WorkSpacePanel             mWorkSpace         = null;
    SceneFlowEditor       mSceneFlowEditor;

    public CopyNodesAction(WorkSpacePanel workSpace, Node node) {
        mWorkSpace = workSpace;
        mNodes.add(node);
    }

    public CopyNodesAction(WorkSpacePanel workSpace, Set<Node> nodes) {
        mWorkSpace       = workSpace;
        mNodes           = nodes;
        mSceneFlowEditor = mWorkSpace.getSceneFlowEditor();
    }

    protected void copyNodes() {
        mWorkSpace.clearClipBoard();

        for (Node node : mNodes) {

            // store a copy of each selected node
            de.dfki.vsm.model.sceneflow.Node nodeCopy = node.getDataNode().getCopy();

            mWorkSpace.getClipBoard().add(nodeCopy);
        }
    }

    protected void uncopyNodes() {
        mWorkSpace.clearClipBoard();
    }

    public void run() {
        copyNodes();
        UndoAction.getInstance().refreshUndoState();
        RedoAction.getInstance().refreshRedoState();
    }

    private class Edit extends AbstractUndoableEdit {
        @Override
        public void undo() throws CannotUndoException {
            uncopyNodes();
        }

        @Override
        public void redo() throws CannotRedoException {
            copyNodes();
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
            return "Undo Copying Of Nodes ";
        }

        @Override
        public String getRedoPresentationName() {
            return "Redo Copying Of Nodes ";
        }
    }
}
