package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.model.sceneflow.Node;

//~--- JDK imports ------------------------------------------------------------

import java.util.HashMap;

/**
 * @author Patrick Gebhard
 */
public class ToggleStartNodeAction extends NodeAction {
    private boolean               mIsStartNode = false;
    private HashMap<String, Node> mStartNodes  = null;

    public ToggleStartNodeAction(de.dfki.vsm.editor.Node node, WorkSpacePanel workSpace) {
        super();
        mWorkSpace        = workSpace;
        mSceneFlowPane    = mWorkSpace.getSceneFlowEditor();
        mSceneFlowManager = mWorkSpace.getSceneFlowManager();
        mUndoManager      = mSceneFlowPane.getUndoManager();
        mIDManager        = mSceneFlowManager.getIDManager();
        mGUINode          = node;
        mGUINodeType      = mGUINode.getType();
        mDataNode         = node.getDataNode();
        mParentDataNode   = mDataNode.getParentNode();
        mDataNodeId       = mDataNode.getId();

        // check start node state
        mStartNodes  = mParentDataNode.getStartNodeMap();
        mIsStartNode = (mStartNodes.containsKey(mDataNodeId))
                       ? true
                       : false;
    }

    @Override
    protected void run() {
        if (mIsStartNode) {
            mGUINode.removeStartSign();
            mStartNodes.remove(mDataNode.getId());
            mIsStartNode = false;
        } else {
            mStartNodes.put(mDataNode.getId(), mDataNode);
            mIsStartNode = true;
            mGUINode.addStartSign();
        }

        EditorInstance.getInstance().refresh();
        UndoAction.getInstance().refreshUndoState();
        RedoAction.getInstance().refreshRedoState();
    }

//  private class Edit extends AbstractUndoableEdit {
//
//      @Override
//      public void undo() throws CannotUndoException {
//          if (mIsStartNode) {
//              mGUINode.removeStartSign();
//              mStartNodes.remove(mDataNode.getId());
//              mIsStartNode = false;
//          } else {
//              mStartNodes.put(mDataNode.getId(), mDataNode);
//              mIsStartNode = true;
//              mGUINode.addStartSign();
//          }
//          Editor.getInstance().update();
//      }
//
//      @Override
//      public void redo() throws CannotRedoException {
//          if (mIsStartNode) {
//              mGUINode.removeStartSign();
//              mStartNodes.remove(mDataNode.getId());
//              mIsStartNode = false;
//          } else {
//              mStartNodes.put(mDataNode.getId(), mDataNode);
//              mIsStartNode = true;
//              mGUINode.addStartSign();
//
//          }
//          Editor.getInstance().update();
//      }
//
//      @Override
//      public boolean canUndo() {
//          return true;
//      }
//
//      @Override
//      public boolean canRedo() {
//          return true;
//      }
//
//      @Override
//      public String getUndoPresentationName() {
//          return "Undo toggle start flag of node " + mDataNode.getName();
//      }
//
//      @Override
//      public String getRedoPresentationName() {
//          return "Redo toggle start flag of node " + mDataNode.getName();
//      }
//  }
}
