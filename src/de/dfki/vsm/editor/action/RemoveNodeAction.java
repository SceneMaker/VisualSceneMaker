package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Edge;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.Node;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;

//~--- JDK imports ------------------------------------------------------------

import java.util.Vector;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;

/**
 * @author Not me
 */
public class RemoveNodeAction extends NodeAction {
    Vector<RemoveEdgeAction> mRemoveEdgeActionList = new Vector<RemoveEdgeAction>();

    public RemoveNodeAction(WorkSpacePanel workSpace, Node node) {
        mWorkSpace        = workSpace;
        mSceneFlowPane    = mWorkSpace.getSceneFlowEditor();
        mSceneFlowManager = mWorkSpace.getSceneFlowManager();
        mUndoManager      = mSceneFlowPane.getUndoManager();
        mIDManager        = mSceneFlowManager.getIDManager();
        mGUINode          = node;
        mCmdBadge         = mWorkSpace.getCmdBadge(mGUINode);
        mCoordinate       = mGUINode.getLocation();
        mGUINodeType      = mGUINode.getType();
        mDataNode         = node.getDataNode();
        mParentDataNode   = mDataNode.getParentNode();
        mDataNodeId       = mDataNode.getId();

        ////
        for (Edge edge : mGUINode.getConnectedEdges()) {
            mRemoveEdgeActionList.add(new RemoveEdgeAction(workSpace, edge));
        }

        ////
    }

    protected void deleteConnectedEdges() {
        for (RemoveEdgeAction action : mRemoveEdgeActionList) {
            action.delete();

            // action.run();
        }

        /*
         * for(Edge edge : mGUINode.getConnectedEdges()) {
         * edge.getSourceNode().disconnectEdge(edge);
         * edge.getTargetNode().disconnectEdge(edge);
         *
         * //mGUINode.disconnectEdge(edge);
         *
         * }
         */
    }

    protected void createConnectedEdges() {
        for (RemoveEdgeAction action : mRemoveEdgeActionList) {
            action.create();

            // action.run();
        }
    }

    public void run() {
        delete();

        // /
        deleteConnectedEdges();

        //
        mUndoManager.addEdit(new Edit());
        UndoAction.getInstance().refreshUndoState();
        RedoAction.getInstance().refreshRedoState();
        EditorInstance.getInstance().refresh();
    }

    private class Edit extends AbstractUndoableEdit {
        @Override
        public void undo() throws CannotUndoException {
            mIDManager.setID(mGUINode);
            create();

            // /
            createConnectedEdges();
        }

        @Override
        public void redo() throws CannotRedoException {
            delete();

            // /
            deleteConnectedEdges();
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
            return "Undo Deletion Of Node " + mDataNode.getName();
        }

        @Override
        public String getRedoPresentationName() {
            return "Redo Deletion Of Node " + mDataNode.getName();
        }
    }
}
