package de.dfki.vsm.editor.action;

import de.dfki.vsm.editor.Edge.TYPE;
import static de.dfki.vsm.editor.Edge.TYPE.CEDGE;
import static de.dfki.vsm.editor.Edge.TYPE.EEDGE;
import static de.dfki.vsm.editor.Edge.TYPE.IEDGE;
import static de.dfki.vsm.editor.Edge.TYPE.PEDGE;
import static de.dfki.vsm.editor.Edge.TYPE.TEDGE;
import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.editor.Node.Flavour;
import de.dfki.vsm.editor.SceneFlowEditor;
import de.dfki.vsm.editor.WorkSpace;
import de.dfki.vsm.model.sceneflow.CEdge;
import de.dfki.vsm.model.sceneflow.EEdge;
import de.dfki.vsm.model.sceneflow.Edge;
import de.dfki.vsm.model.sceneflow.FEdge;
import de.dfki.vsm.model.sceneflow.IEdge;
import de.dfki.vsm.model.sceneflow.PEdge;
import de.dfki.vsm.model.sceneflow.TEdge;
import java.awt.Point;
import javax.swing.undo.UndoManager;

/**
 * @author Gregor Mehlmann
 */
public abstract class EdgeAction extends EditorAction {

    protected UndoManager mUndoManager = null;
    protected SceneFlowEditor mSceneFlowPane = null;
    protected WorkSpace mWorkSpace = null;
    protected de.dfki.vsm.editor.Node mSourceGUINode = null;
    protected de.dfki.vsm.editor.Node mTargetGUINode = null;
    protected de.dfki.vsm.editor.Node mLastTargetGUINode = null;
    protected de.dfki.vsm.editor.Edge mGUIEdge = null;
    protected Edge mDataEdge = null;
    protected TYPE mGUIEdgeType = null;
    protected Point mSourceGUINodeDockPoint = null;
    protected Point mTargetGUINodeDockPoint = null;
    protected Point mLastTargetGUINodeDockPoint = null;

    public void create() {
        mDataEdge.setTargetNode(mTargetGUINode.getDataNode());
        mDataEdge.setSourceNode(mSourceGUINode.getDataNode());
        mDataEdge.setTarget(mDataEdge.getTargetNode().getId());

        switch (mGUIEdgeType) {
            case EEDGE:
                mSourceGUINode.getDataNode().setDedge(mDataEdge);
                break;
            case FEDGE:
                mSourceGUINode.getDataNode().addFEdge((FEdge) mDataEdge);
                break;
            case TEDGE:
                mSourceGUINode.getDataNode().setDedge(mDataEdge);
                break;
            case CEDGE:
                mSourceGUINode.getDataNode().addCEdge((CEdge) mDataEdge);
                break;
            case PEDGE:
                mSourceGUINode.getDataNode().addPEdge((PEdge) mDataEdge);
                break;
            case IEDGE:
                mSourceGUINode.getDataNode().addIEdge((IEdge) mDataEdge);
                break;
        }

        // Revalidate data node and graphical node types
        switch (mSourceGUINode.getDataNode().getFlavour()) {
            case NONE:
                Edge dedge = mSourceGUINode.getDataNode().getDedge();
                if (dedge instanceof EEdge) {
                    mSourceGUINode.setFlavour(Flavour.ENode);
                } else if (dedge instanceof TEdge) {
                    mSourceGUINode.setFlavour(Flavour.TNode);
                } else {
                    mSourceGUINode.setFlavour(Flavour.None);
                }
                break;
            case PNODE:
                mSourceGUINode.setFlavour(Flavour.PNode);
                break;
            case FNODE:
                mSourceGUINode.setFlavour(Flavour.FNode);
                break;
            case CNODE:
                mSourceGUINode.setFlavour(Flavour.CNode);
                break;
            case INODE:
                mSourceGUINode.setFlavour(Flavour.INode);
                break;
        }

    // Connect GUI Edge to Source GUI node
        // Connect GUI Edge to Target GUI node
        // TODO: Recompute the appearance of the source GUI node
        if (mGUIEdge == null) {
            mGUIEdge = new de.dfki.vsm.editor.Edge(mWorkSpace, mDataEdge, mGUIEdgeType, mSourceGUINode, mTargetGUINode);
        } else {
            if (mSourceGUINode.equals(mTargetGUINode)) {
                // same nodes
                mSourceGUINodeDockPoint = mSourceGUINode.connectEdgeAtSourceNode(mGUIEdge, mSourceGUINodeDockPoint);
                mTargetGUINodeDockPoint = mTargetGUINode.connectSelfPointingEdge(mGUIEdge, mTargetGUINodeDockPoint);
            } else {
                // different nodes
                mSourceGUINodeDockPoint = mSourceGUINode.connectEdgeAtSourceNode(mGUIEdge, mSourceGUINodeDockPoint);
                mTargetGUINodeDockPoint = mTargetGUINode.connectEdgetAtTargetNode(mGUIEdge, new Point(50, 50));
            }
        }
        //mSourceGUINode.update();
        Editor.getInstance().update();
        mWorkSpace.add(mGUIEdge);
        mWorkSpace.revalidate();
        mWorkSpace.repaint();
    }

    public void deleteDeflected() {
        if (mSourceGUINode.equals(mTargetGUINode)) {
            mSourceGUINodeDockPoint = mSourceGUINode.disconnectEdge(mGUIEdge);
            //
            mLastTargetGUINodeDockPoint = mGUIEdge.mLastTargetNodeDockPoint;
        } else {
            mSourceGUINodeDockPoint = mSourceGUINode.disconnectEdge(mGUIEdge);
            mLastTargetGUINodeDockPoint = mGUIEdge.mLastTargetNodeDockPoint;
        }
        cleanUpData();
    }

    public void delete() {
        // Disconnect the GUI edge from the GUI nodes
        if (mSourceGUINode.equals(mTargetGUINode)) {
            mSourceGUINodeDockPoint = mSourceGUINode.disconnectEdge(mGUIEdge);
            mTargetGUINodeDockPoint = mTargetGUINode.disconnectSelfPointingEdge(mGUIEdge);
        } else {
            mSourceGUINodeDockPoint = mSourceGUINode.disconnectEdge(mGUIEdge);
            mTargetGUINodeDockPoint = mTargetGUINode.disconnectEdge(mGUIEdge);
        }
        cleanUpData();
    }

    private void cleanUpData() {
        // Disconnect the data edge from the source data node
        switch (mGUIEdgeType) {
            case EEDGE:
                mSourceGUINode.getDataNode().removeDEdge();
                break;
            case TEDGE:
                mSourceGUINode.getDataNode().removeDEdge();
                break;
            case CEDGE:
                mSourceGUINode.getDataNode().removeCEdge((CEdge) mDataEdge);
                break;
            case PEDGE:
                mSourceGUINode.getDataNode().removePEdge((PEdge) mDataEdge);
                break;
            case FEDGE:
                mSourceGUINode.getDataNode().removeFEdge((FEdge) mDataEdge);
                break;
            case IEDGE:
                mSourceGUINode.getDataNode().removeIEdge((IEdge) mDataEdge);
                break;
        }

        // Revalidate data node and graphical node types
        switch (mSourceGUINode.getDataNode().getFlavour()) {
            case NONE:
                Edge dedge = mSourceGUINode.getDataNode().getDedge();
                if (dedge instanceof EEdge) {
                    mSourceGUINode.setFlavour(Flavour.ENode);
                } else if (dedge instanceof TEdge) {
                    mSourceGUINode.setFlavour(Flavour.TNode);
                } else {
                    mSourceGUINode.setFlavour(Flavour.None);
                }
                break;
            case PNODE:
                mSourceGUINode.setFlavour(Flavour.PNode);
                break;
            case FNODE:
                mSourceGUINode.setFlavour(Flavour.FNode);
                break;
            case CNODE:
                mSourceGUINode.setFlavour(Flavour.CNode);
                break;
            case INODE:
                mSourceGUINode.setFlavour(Flavour.INode);
                break;
        }

    // Remove the GUI-Edge from the workspace and
        // update the source node appearance
        Editor.getInstance().update();
        mWorkSpace.remove(mGUIEdge);
        mWorkSpace.revalidate();
        mWorkSpace.repaint();
    }
}
