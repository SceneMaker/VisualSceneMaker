package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Edge;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.Node;
import de.dfki.vsm.editor.Node.Flavour;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.model.sceneflow.CEdge;
import de.dfki.vsm.model.sceneflow.EEdge;
import de.dfki.vsm.model.sceneflow.FEdge;
import de.dfki.vsm.model.sceneflow.IEdge;
import de.dfki.vsm.model.sceneflow.PEdge;
import de.dfki.vsm.model.sceneflow.TEdge;
import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Point;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;

/**
 * @author Patrick Gebhard
 */
public class DeflectEdgeAction extends EdgeAction {
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    public DeflectEdgeAction(WorkSpacePanel workSpace, Edge edge, Node newTargetNode, Point newDropPoint) {
        mWorkSpace         = workSpace;
        mGUIEdge           = edge;
        mDataEdge          = edge.getDataEdge();
        mSourceGUINode     = edge.getSourceNode();
        mLastTargetGUINode = edge.getTargetNode();
        mTargetGUINode     = newTargetNode;

        // store the dockpoints
        mSourceGUINodeDockPoint     = mSourceGUINode.getEdgeDockPoint(edge);
        mLastTargetGUINodeDockPoint = edge.mLastTargetNodeDockPoint;    // last target node dockpoint
        mTargetGUINodeDockPoint     = newDropPoint;
        mLogger.message("new target dockpoint (was drop point) " + mTargetGUINodeDockPoint);
        mGUIEdgeType   = edge.getType();
        mSceneFlowPane = mWorkSpace.getSceneFlowEditor();
        mUndoManager   = mSceneFlowPane.getUndoManager();
    }

    public void createDeflectEdge() {
        mLogger.message("create new egde " + mSourceGUINode.getDataNode().getName() + " to "
                        + mTargetGUINode.getDataNode().getName());
        mDataEdge.setTargetNode(mTargetGUINode.getDataNode());
        mDataEdge.setSourceNode(mSourceGUINode.getDataNode());    // this is the new node
        mDataEdge.setTarget(mDataEdge.getTargetNode().getId());

        switch (mGUIEdgeType) {
        case EEDGE :
            mSourceGUINode.getDataNode().setDedge(mDataEdge);

            break;

        case FEDGE :
            mSourceGUINode.getDataNode().addFEdge((FEdge) mDataEdge);

            break;

        case TEDGE :
            mSourceGUINode.getDataNode().setDedge(mDataEdge);

            break;

        case CEDGE :
            mSourceGUINode.getDataNode().addCEdge((CEdge) mDataEdge);

            break;

        case PEDGE :
            mSourceGUINode.getDataNode().addPEdge((PEdge) mDataEdge);

            break;

        case IEDGE :
            mSourceGUINode.getDataNode().addIEdge((IEdge) mDataEdge);

            break;
        }

        // Revalidate data node and graphical node types
        switch (mSourceGUINode.getDataNode().getFlavour()) {
        case NONE :
            de.dfki.vsm.model.sceneflow.Edge dedge = mSourceGUINode.getDataNode().getDedge();

            if (dedge instanceof EEdge) {
                mSourceGUINode.setFlavour(Flavour.ENode);
            } else if (dedge instanceof TEdge) {
                mSourceGUINode.setFlavour(Flavour.TNode);
            } else {
                mSourceGUINode.setFlavour(Flavour.None);
            }

            break;

        case PNODE :
            mSourceGUINode.setFlavour(Flavour.PNode);

            break;

        case FNODE :
            mSourceGUINode.setFlavour(Flavour.FNode);

            break;

        case CNODE :
            mSourceGUINode.setFlavour(Flavour.CNode);

            break;

        case INODE :
            mSourceGUINode.setFlavour(Flavour.INode);

            break;
        }

        mLogger.message("edge creation");

        // create a new gui edge
        mGUIEdge = new Edge(mWorkSpace, mDataEdge, mGUIEdgeType, mSourceGUINode, mTargetGUINode,
                            mSourceGUINodeDockPoint, mTargetGUINodeDockPoint);
        mLogger.message("edge connection from source point " + mSourceGUINodeDockPoint + " to "
                        + mTargetGUINodeDockPoint);

//      // connect edge
//      if (mSourceGUINode.equals(mTargetGUINode)) {
//        // same nodes
//        //mSourceGUINodeDockPoint = mSourceGUINode.connectEdgeAtSourceNode(mGUIEdge, mSourceGUINodeDockPoint);
//        mTargetGUINodeDockPoint = mTargetGUINode.connectSelfPointingEdge(mGUIEdge, mTargetGUINodeDockPoint);
//      } else {
//        // different nodes
//        //mSourceGUINodeDockPoint = mSourceGUINode.connectEdgeAtSourceNode(mGUIEdge, mSourceGUINodeDockPoint);
//        mTargetGUINodeDockPoint = mTargetGUINode.connectEdgetAtTargetNode(mGUIEdge, mTargetGUINodeDockPoint);
//      }
        // mSourceGUINode.update();
        EditorInstance.getInstance().refresh();
        mWorkSpace.add(mGUIEdge);

        // straighten the edge ...
        mGUIEdge.straightenEdge();

        // repaint
        mWorkSpace.revalidate();
        mWorkSpace.repaint();
    }

    public void run() {

        // delete old edge
        deleteDeflected();

        // create new edge, with data from old edge
        createDeflectEdge();

        // Update Redo/Undo state
        mUndoManager.addEdit(new Edit());
        UndoAction.getInstance().refreshUndoState();
        RedoAction.getInstance().refreshRedoState();
    }

    private class Edit extends AbstractUndoableEdit {
        @Override
        public void undo() throws CannotUndoException {

//          TODO
        }

        @Override
        public void redo() throws CannotRedoException {

//          TODO
        }

        @Override
        public boolean canUndo() {
            return false;    // TOdO check
        }

        @Override
        public boolean canRedo() {
            return false;    // TOdO check
        }

        @Override
        public String getUndoPresentationName() {
            return "Undo Deletion Of Edge";
        }

        @Override
        public String getRedoPresentationName() {
            return "Redo Deletion Of Edge";
        }
    }
}
