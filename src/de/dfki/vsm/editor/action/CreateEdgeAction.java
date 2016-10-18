package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Edge;
import de.dfki.vsm.editor.Node;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.editor.dialog.ModifyIEdgeDialog;
import de.dfki.vsm.editor.dialog.ModifyCEdgeDialog;
import de.dfki.vsm.editor.dialog.ModifyPEdgeDialog;
import de.dfki.vsm.editor.dialog.ModifyTEdgeDialog;
import de.dfki.vsm.model.sceneflow.EEdge;
import de.dfki.vsm.model.sceneflow.FEdge;
import de.dfki.vsm.model.sceneflow.PEdge;
import de.dfki.vsm.model.sceneflow.command.Command;
import java.util.Vector;

//~--- JDK imports ------------------------------------------------------------

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;

/**
 * @author Gregor Mehlmann
 */
public class CreateEdgeAction extends EdgeAction {
    private boolean mShowDialog = true;

    public CreateEdgeAction(WorkSpacePanel workSpace, Node sourceNode, Node targetNode, Edge.TYPE type) {
        mWorkSpace     = workSpace;
        mTargetGUINode = targetNode;
        mSourceGUINode = sourceNode;
        mGUIEdgeType   = type;
        mSceneFlowPane = mWorkSpace.getSceneFlowEditor();
        mUndoManager   = mSceneFlowPane.getUndoManager();
        mShowDialog    = true;
    }

    public CreateEdgeAction(WorkSpacePanel workSpace, Node sourceNode, Node targetNode,
                            de.dfki.vsm.model.sceneflow.AbstractEdge dataEdge, Edge.TYPE type) {
        mWorkSpace     = workSpace;
        mTargetGUINode = targetNode;
        mSourceGUINode = sourceNode;
        mDataEdge      = dataEdge;

        // TODO check data edge integrity (i.e. pedges!)      
        mGUIEdgeType   = type;
        mSceneFlowPane = mWorkSpace.getSceneFlowEditor();
        mUndoManager   = mSceneFlowPane.getUndoManager();
        mShowDialog    = false;
    }

    protected void showCreationDialog() {
        if (mShowDialog) {
            switch (mGUIEdgeType) {
            case EEDGE :
                mDataEdge = new EEdge();

                break;

            case FEDGE :
                mDataEdge = new FEdge();

                break;

            case TEDGE :
                ModifyTEdgeDialog tedgeDialog = new ModifyTEdgeDialog(mSourceGUINode.getDataNode(),
                                                    mTargetGUINode.getDataNode());

                mDataEdge = tedgeDialog.run();

                break;

            case CEDGE :
                ModifyCEdgeDialog cedgeDialog = new ModifyCEdgeDialog(mSourceGUINode.getDataNode(),
                                                    mTargetGUINode.getDataNode());

                mDataEdge = cedgeDialog.run();

                break;

            case PEDGE :
                if(mSourceGUINode.getDataNode().getPEdgeList().isEmpty()){
                    mDataEdge = new PEdge();
                    ((PEdge)mDataEdge).setProbability(100);
                }
                else
                {
                    ModifyPEdgeDialog pedgeDialog = new ModifyPEdgeDialog(mSourceGUINode.getDataNode(),
                                                        mTargetGUINode.getDataNode());

                    mDataEdge = pedgeDialog.run();
                }

                break;

            case IEDGE :
                ModifyIEdgeDialog iedgeDialog = new ModifyIEdgeDialog(mSourceGUINode.getDataNode(),
                                                    mTargetGUINode.getDataNode());

                mDataEdge = iedgeDialog.run();

                break;
            }
        }
    }

    public void run() {
        showCreationDialog();

        if (mDataEdge != null) {
            create();

            //
            mWorkSpace.revalidate();
            mWorkSpace.repaint();

            //
            mUndoManager.addEdit(new Edit());
            UndoAction.getInstance().refreshUndoState();
            RedoAction.getInstance().refreshRedoState();
        }
    }

    private class Edit extends AbstractUndoableEdit {
        @Override
        public void undo() throws CannotUndoException {
            delete();
            mGUIEdge.repaint();

//          mWorkSpace.revalidate();
//          mWorkSpace.repaint();
        }

        @Override
        public void redo() throws CannotRedoException {
            create();
            mGUIEdge.repaint();

//          mWorkSpace.revalidate();
//          mWorkSpace.repaint();
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
            return "Undo creation of edge";
        }

        @Override
        public String getRedoPresentationName() {
            return "Redo creation of edge";
        }
    }
}
