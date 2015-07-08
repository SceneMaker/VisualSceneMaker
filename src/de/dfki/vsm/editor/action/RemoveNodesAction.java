package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.Node;
import de.dfki.vsm.editor.WorkSpace;

//~--- JDK imports ------------------------------------------------------------

import java.util.HashSet;
import java.util.Set;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;

/**
 * @author Patrick Gebhard
 */
public class RemoveNodesAction extends EditorAction {
    Set<Node>             mNodes             = new HashSet<Node>();
    Set<RemoveNodeAction> mRemoveNodeActions = new HashSet<RemoveNodeAction>();
    WorkSpace             mWorkSpace         = null;

    public RemoveNodesAction(WorkSpace workSpace, Set<Node> nodes) {
        mWorkSpace = workSpace;
        mNodes     = nodes;
    }

    protected void deleteNodes() {
        for (Node node : mNodes) {
            RemoveNodeAction rma = new RemoveNodeAction(mWorkSpace, node);

            mRemoveNodeActions.add(rma);
            rma.run();
        }
    }

    protected void createNodes() {
        for (RemoveNodeAction action : mRemoveNodeActions) {
            action.create();
        }
    }

    public void run() {
        deleteNodes();
        UndoAction.getInstance().refreshUndoState();
        RedoAction.getInstance().refreshRedoState();
    }

    private class Edit extends AbstractUndoableEdit {
        @Override
        public void undo() throws CannotUndoException {
            createNodes();
        }

        @Override
        public void redo() throws CannotRedoException {
            deleteNodes();
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
            return "Undo Deletion Of Nodes ";
        }

        @Override
        public String getRedoPresentationName() {
            return "Redo Deletion Of Nodes ";
        }
    }
}
