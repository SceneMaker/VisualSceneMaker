package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Comment;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.model.sceneflow.graphics.comment.Graphics;
import de.dfki.vsm.model.sceneflow.graphics.comment.Rect;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Point;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.UndoManager;

/**
 * @author Not me
 * @author Patrick Gebhard
 */
public class CreateCommentAction extends EditorAction {
    private UndoManager                           mUndoManager = null;
    private WorkSpacePanel                             mWorkSpace   = null;
    private Point                                 mCoordinate  = null;
    private Comment                               mGUIComment;
    private de.dfki.vsm.model.sceneflow.Comment   mComment;
    private de.dfki.vsm.model.sceneflow.SuperNode mParentDataNode;

    public CreateCommentAction(WorkSpacePanel workSpace, Point coordinate) {
        mWorkSpace   = workSpace;
        mCoordinate  = coordinate;
        mUndoManager = mWorkSpace.getSceneFlowEditor().getUndoManager();
        mComment     = new de.dfki.vsm.model.sceneflow.Comment();
        mComment.setGraphics(new Graphics(new Rect(coordinate.x, coordinate.y, 100, 100)));
        mParentDataNode = mWorkSpace.getSceneFlowManager().getCurrentActiveSuperNode();

        //
        mGUIComment = new de.dfki.vsm.editor.Comment(mWorkSpace, mComment);
    }

    public void create() {
        mComment.setParentNode(mParentDataNode);
        mParentDataNode.addComment(mComment);
        mWorkSpace.add(mGUIComment);
    }

    public void run() {
        create();
        mWorkSpace.revalidate();
        mWorkSpace.repaint();
    }

    private class Edit extends AbstractUndoableEdit {
        @Override
        public void undo() throws CannotUndoException {}

        @Override
        public void redo() throws CannotRedoException {}

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
            return "Undo Creation Of Comment";
        }

        @Override
        public String getRedoPresentationName() {
            return "Redo Creation Of Comment ";
        }
    }
}
