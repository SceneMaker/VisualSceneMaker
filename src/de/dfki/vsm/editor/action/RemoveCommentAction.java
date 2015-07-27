package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Comment;
import de.dfki.vsm.editor.project.sceneflow.SceneFlowEditor;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.editor.util.SceneFlowManager;
import de.dfki.vsm.model.sceneflow.SuperNode;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Point;

import java.util.Vector;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.UndoManager;

/**
 * @author Patrick Gebhard
 */
public class RemoveCommentAction extends EditorAction {
    Vector<RemoveEdgeAction>                    mRemoveEdgeActionList = new Vector<RemoveEdgeAction>();
    private WorkSpacePanel                           mWorkSpace;
    private SceneFlowEditor                     mSceneFlowPane;
    private SceneFlowManager                    mSceneFlowManager;
    private UndoManager                         mUndoManager;
    private SuperNode                           mSuperNode;
    private Comment                             mGUIComment;
    private Point                               mLocation;
    private de.dfki.vsm.model.sceneflow.Comment mDataComment;

    public RemoveCommentAction(WorkSpacePanel workSpace, Comment c) {
        mWorkSpace        = workSpace;
        mSceneFlowPane    = mWorkSpace.getSceneFlowEditor();
        mSceneFlowManager = mWorkSpace.getSceneFlowManager();
        mSuperNode        = mSceneFlowManager.getCurrentActiveSuperNode();
        mUndoManager      = mSceneFlowPane.getUndoManager();
        mGUIComment       = c;
        mLocation         = mGUIComment.getLocation();
        mDataComment      = mGUIComment.getData();
    }

    public void delete() {
        mSuperNode.removeComment(mDataComment);
        mWorkSpace.remove(mGUIComment);
    }

    public void create() {
        mSuperNode.addComment(mDataComment);
        mWorkSpace.add(new Comment(mWorkSpace, mDataComment));
    }

    public void run() {
        delete();
        mWorkSpace.revalidate();
        mWorkSpace.repaint();
        mUndoManager.addEdit(new Edit());
        UndoAction.getInstance().refreshUndoState();
        RedoAction.getInstance().refreshRedoState();
    }

    private class Edit extends AbstractUndoableEdit {
        @Override
        public void undo() throws CannotUndoException {
            create();
            mWorkSpace.revalidate();
            mWorkSpace.repaint();
        }

        @Override
        public void redo() throws CannotRedoException {
            delete();
            mWorkSpace.revalidate();
            mWorkSpace.repaint();
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
            return "Undo Deletion of Comment";
        }

        @Override
        public String getRedoPresentationName() {
            return "Redo Deletion of Comment";
        }
    }
}
