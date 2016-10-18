package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.CmdBadge;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.Node.Type;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.model.sceneflow.BasicNode;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.model.sceneflow.graphics.node.NodeGraphics;

import static de.dfki.vsm.editor.Node.Type.BasicNode;
import static de.dfki.vsm.editor.Node.Type.SuperNode;
import de.dfki.vsm.editor.event.NodeSelectedEvent;
import de.dfki.vsm.util.evt.EventDispatcher;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Point;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public class CreateNodeAction extends NodeAction {
    public CreateNodeAction(WorkSpacePanel workSpace, de.dfki.vsm.model.sceneflow.BasicNode node) {
        mWorkSpace        = workSpace;
        mCoordinate       = new Point(node.getGraphics().getPosition().getXPos(),
                                      node.getGraphics().getPosition().getYPos());
        mGUINodeType      = (SuperNode.class.isInstance(node))
                            ? de.dfki.vsm.editor.Node.Type.SuperNode
                            : de.dfki.vsm.editor.Node.Type.BasicNode;
        mSceneFlowPane    = mWorkSpace.getSceneFlowEditor();
        mSceneFlowManager = mWorkSpace.getSceneFlowManager();
        mUndoManager      = mSceneFlowPane.getUndoManager();
        mIDManager        = mSceneFlowManager.getIDManager();
        mDataNodeId       = node.getId();
        mDataNode         = node;

        // DEBUG mDataNode.writeXML(new IndentOutputStream(System.out));
        mParentDataNode = mSceneFlowManager.getCurrentActiveSuperNode();

        // Create the GUI-BasicNode
        mGUINode = new de.dfki.vsm.editor.Node(mWorkSpace, mDataNode);

        // Create the command badge of the GUI-BasicNode
        mCmdBadge = new CmdBadge(mGUINode);
        mGUINode.resetLocation(mWorkSpace.mGridManager.getNodeLocation(mCoordinate));
    }

    public CreateNodeAction(WorkSpacePanel workSpace, Point coordinate, Type type) {
        mWorkSpace        = workSpace;
        mCoordinate       = coordinate;
        mGUINodeType      = type;
        mSceneFlowPane    = mWorkSpace.getSceneFlowEditor();
        mSceneFlowManager = mWorkSpace.getSceneFlowManager();
        mUndoManager      = mSceneFlowPane.getUndoManager();
        mIDManager        = mSceneFlowManager.getIDManager();

        if (mGUINodeType == BasicNode) {
            mDataNodeId = mIDManager.getNextFreeNodeID();
            mDataNode   = new BasicNode();
            mDataNode.setNameAndId(mDataNodeId);
            mDataNode.setGraphics(new NodeGraphics(mCoordinate.x, mCoordinate.y));
            mParentDataNode = mSceneFlowManager.getCurrentActiveSuperNode();
        } else if (mGUINodeType == SuperNode) {
            mDataNodeId = mIDManager.getNextFreeSuperNodeID();
            mDataNode   = new SuperNode();
            mDataNode.setNameAndId(mDataNodeId);
            mDataNode.setGraphics(new NodeGraphics(mCoordinate.x, mCoordinate.y));
            mParentDataNode = mSceneFlowManager.getCurrentActiveSuperNode();

            //////////////////
            BasicNode mHistoryDataNode = new BasicNode();

            mHistoryDataNode.setHistoryNodeFlag(true);
            mHistoryDataNode.setName("History");
            mHistoryDataNode.setId(mIDManager.getNextFreeNodeID());
            mHistoryDataNode.setGraphics(new NodeGraphics(0, 0));
            mHistoryDataNode.setParentNode((SuperNode) mDataNode);
            ((SuperNode) mDataNode).addNode(mHistoryDataNode);
            ((SuperNode) mDataNode).setHistoryNode(mHistoryDataNode);
        }

        // Create the GUI-BasicNode
        mGUINode = new de.dfki.vsm.editor.Node(mWorkSpace, mDataNode);

        // Create the command badge of the GUI-BasicNode
        mCmdBadge = new CmdBadge(mGUINode);
        
        // Make newly created node selected
        EventDispatcher.getInstance().convey(new NodeSelectedEvent(this, mDataNode));
                         
    }

    public void run() {
        create();
        mUndoManager.addEdit(new Edit());
        UndoAction.getInstance().refreshUndoState();
        RedoAction.getInstance().refreshRedoState();
    }

    private class Edit extends AbstractUndoableEdit {
        @Override
        public void undo() throws CannotUndoException {
            delete();
        }

        @Override
        public void redo() throws CannotRedoException {

            // say IDMAnger thta id is used
            mIDManager.setID(mGUINode);
            create();
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
            return "Undo Creation Of Node " + mDataNode.getName();
        }

        @Override
        public String getRedoPresentationName() {
            return "Redo Creation Of Node " + mDataNode.getName();
        }
    }
}
