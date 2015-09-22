package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Edge.TYPE;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;

/**
 * @author Not me
 */
public class ModifyEdgeAction extends EdgeAction {
    public ModifyEdgeAction(de.dfki.vsm.editor.Edge edge, WorkSpacePanel workSpace) {
        mGUIEdge       = edge;
        mWorkSpace     = workSpace;
        mDataEdge      = edge.getDataEdge();
        mSourceGUINode = edge.getSourceNode();
        mTargetGUINode = edge.getTargetNode();
        mGUIEdgeType   = edge.getType();
        mSceneFlowPane = mWorkSpace.getSceneFlowEditor();
        mUndoManager   = mSceneFlowPane.getUndoManager();
    }

    public void run() {
        if (mGUIEdgeType == TYPE.TEDGE) {
            ModifyTEdgeAction action = new ModifyTEdgeAction(mGUIEdge, mWorkSpace);
            action.run();
        } else if (mGUIEdgeType == TYPE.PEDGE) {
            ModifyPEdgeAction action = new ModifyPEdgeAction(mGUIEdge, mWorkSpace);
            action.run();
        } else if (mGUIEdgeType == TYPE.CEDGE) {
            ModifyCEdgeAction action = new ModifyCEdgeAction(mGUIEdge, mWorkSpace);
            action.run();
        } else if (mGUIEdgeType == TYPE.IEDGE) {
            ModifyIEdgeAction action = new ModifyIEdgeAction(mGUIEdge, mWorkSpace);
            action.run();
        }

        // Repaint the GUI to show the changes
        mGUIEdge.update();
        mWorkSpace.revalidate();
        mWorkSpace.repaint();
    }
}
