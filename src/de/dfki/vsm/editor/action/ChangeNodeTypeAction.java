package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.CmdBadge;
import de.dfki.vsm.editor.Edge;
import de.dfki.vsm.editor.Node;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.model.sceneflow.graphics.node.Graphics;

//~--- JDK imports ------------------------------------------------------------

import java.util.Set;
import java.util.Vector;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;

/**
 * @author Patrick Gebhard
 */
public class ChangeNodeTypeAction extends NodeAction {
    private Node                     mOldGUINode           = null;
    private Set<Edge>                mConnectedEdges       = null;
    private Vector<Edge>             mConnectedEdgesVector = new Vector<>();
    private Vector<RemoveEdgeAction> mRemoveEdgeActionList = new Vector<>();
    private Vector<CreateEdgeAction> mCreateEdgeActionList = new Vector<>();

    public ChangeNodeTypeAction(WorkSpacePanel workSpace, Node node) {
        mWorkSpace        = workSpace;
        mSceneFlowPane    = mWorkSpace.getSceneFlowEditor();
        mSceneFlowManager = mWorkSpace.getSceneFlowManager();
        mUndoManager      = mSceneFlowPane.getUndoManager();
        mIDManager        = mSceneFlowManager.getIDManager();
        mGUINode          = node;
        mOldGUINode       = node;
        mCoordinate       = mGUINode.getLocation();
        mGUINodeType      = mGUINode.getType();
        mDataNode         = node.getDataNode();
        mParentDataNode   = mDataNode.getParentNode();
        mDataNodeId       = mDataNode.getId();
        
        // store all connected edge
        mConnectedEdges = mGUINode.getConnectedEdges();
 
        // prepare all edges to be removed
        for (Edge edge : mConnectedEdges) {
            mConnectedEdgesVector.add(edge); // This is needed because mConnectedEdges is a shallow copy
            mRemoveEdgeActionList.add(new RemoveEdgeAction(workSpace, edge));
        }
    }

    protected void changeNodeType() {
        mSceneFlowPane.setMessageLabelText("Convert Node to SuperNode");

        // create new data node
        SuperNode newDataNode = new SuperNode(mDataNode);

        // delete all edges and delete old node
        for (RemoveEdgeAction action : mRemoveEdgeActionList) {
            action.delete();
        }

        delete();
        
        // overwrite stored data node with new value
        mDataNode   = newDataNode.getCopy();
        mDataNodeId = mIDManager.getNextFreeSuperNodeID();
        mDataNode.setNameAndId(mDataNodeId);
        mDataNode.setExhaustive(false);
        mDataNode.setPreserving(false);
       
       
        if(newDataNode.getDedge()!=null){
            if(newDataNode.getDedge().getTarget().equals(newDataNode.getId())){
                mDataNode.getDedge().setTarget(mDataNode.getId());
            }
        }
       
        de.dfki.vsm.model.sceneflow.Node mHistoryDataNode = new de.dfki.vsm.model.sceneflow.Node();

        mHistoryDataNode.setHistoryNodeFlag(true);
        mHistoryDataNode.setName("History");
        mHistoryDataNode.setId(mWorkSpace.getSceneFlowManager().getIDManager().getNextFreeNodeID());
        mHistoryDataNode.setExhaustive(false);
        mHistoryDataNode.setPreserving(false);
        mHistoryDataNode.setGraphics(new Graphics(0, 0));
        mHistoryDataNode.setParentNode((SuperNode) mDataNode);
        ((SuperNode) mDataNode).addNode(mHistoryDataNode);
        ((SuperNode) mDataNode).setHistoryNode(mHistoryDataNode);

        // create new gui node with new data node
        mGUINode = new de.dfki.vsm.editor.Node(mWorkSpace, mDataNode);
        mCmdBadge = new CmdBadge(mGUINode);
        
        create();

        // overview old values with new
        mCoordinate     = mGUINode.getLocation();
        mGUINodeType    = mGUINode.getType();
        mParentDataNode = mDataNode.getParentNode();
        mDataNodeId     = mDataNode.getId();

        // recreate all edges
        
        for (Edge edge : mConnectedEdgesVector) {
            
            de.dfki.vsm.model.sceneflow.Edge newDataEdge      = edge.getDataEdge().getCopy();
            Edge.TYPE newEdgeType      = edge.getType();
            Node      newSourceGUINode = (edge.getSourceNode().equals(mOldGUINode))? mGUINode : edge.getSourceNode();
            Node      newTargetGUINode = (edge.getTargetNode().equals(mOldGUINode))? mGUINode : edge.getTargetNode();

            newDataEdge.setSource(newSourceGUINode.getDataNode().getId());
            newDataEdge.setSourceNode(newSourceGUINode.getDataNode());
            newDataEdge.setTarget(newTargetGUINode.getDataNode().getId());
            newDataEdge.setTargetNode(newTargetGUINode.getDataNode());
            mCreateEdgeActionList.add(new CreateEdgeAction(mWorkSpace, newSourceGUINode, newTargetGUINode, newDataEdge,
                    newEdgeType));
        }

        for (CreateEdgeAction action : mCreateEdgeActionList) {
            action.create();
        }
     
    }

    protected void unchangeNodeType() {
        mSceneFlowPane.setMessageLabelText("N.A. unchange node type");
    }

    public void run() {
        changeNodeType();
        UndoAction.getInstance().refreshUndoState();
        RedoAction.getInstance().refreshRedoState();
               
        // Navigate into new supernode
        mWorkSpace.increaseWorkSpaceLevel(mGUINode);
    }

    private class Edit extends AbstractUndoableEdit {
        @Override
        public void undo() throws CannotUndoException {
            unchangeNodeType();
        }

        @Override
        public void redo() throws CannotRedoException {
            changeNodeType();
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
