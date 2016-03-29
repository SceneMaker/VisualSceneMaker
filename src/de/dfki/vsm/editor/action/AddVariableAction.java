package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.dialog.VarDefDialog;
import de.dfki.vsm.editor.event.NodeSelectedEvent;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.model.sceneflow.BasicNode;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.model.sceneflow.definition.VarDef;
import de.dfki.vsm.util.evt.EventDispatcher;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.UndoManager;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Queue;

/**
 * Sergio Soto
 */
public class AddVariableAction extends EditorAction {
    private LinkedList<VarDef> varDefsList;
    private UndoManager mUndoManager = null;
    private final BasicNode mCurrentSuperNode;
    private WorkSpacePanel                             mWorkSpace   = null;
    public AddVariableAction(WorkSpacePanel  workSpace) {
        mWorkSpace = workSpace;
        mCurrentSuperNode = workSpace.getSceneFlowManager().getCurrentActiveSuperNode();;
        mUndoManager = mWorkSpace.getSceneFlowEditor().getUndoManager();
        varDefsList = new LinkedList<>();

    }

    @Override
    public void run() {
        VarDef varDef = new VarDefDialog(mCurrentSuperNode, null).run();
        // Add the new variable definition if creation was successful
        if (varDef != null) {
            mUndoManager.addEdit(new Edit());
            UndoAction.getInstance().refreshUndoState();
            RedoAction.getInstance().refreshRedoState();
            mCurrentSuperNode.addVarDef(varDef);
            //varDefsList.add(varDef);
            // launch event to update element editor
            EventDispatcher.getInstance().convey(new NodeSelectedEvent(this,mCurrentSuperNode));
            // update workspace
            EditorInstance.getInstance().refresh();
        }
    }

    private class Edit extends AbstractUndoableEdit {
        @Override
        public void undo() throws CannotUndoException {
            if(mCurrentSuperNode.getVarDefList().size()-1 >= 0) {
                VarDef varDef = mCurrentSuperNode.getVarDefAt(mCurrentSuperNode.getVarDefList().size() - 1);
                varDefsList.add(varDef);
                mCurrentSuperNode.removeVarDefAt(mCurrentSuperNode.getVarDefList().size() - 1);
                mWorkSpace.revalidate();
                mWorkSpace.repaint();
                mWorkSpace.refresh();
                EventDispatcher.getInstance().convey(new NodeSelectedEvent(this, mCurrentSuperNode));
            }
        }

        @Override
        public void redo() throws CannotRedoException {
            VarDef varDef  = varDefsList.remove();
            mCurrentSuperNode.addVarDef(varDef);
            varDefsList.add(varDef);
            mWorkSpace.revalidate();
            mWorkSpace.repaint();
            mWorkSpace.refresh();
            EventDispatcher.getInstance().convey(new NodeSelectedEvent(this,mCurrentSuperNode));
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
            return "Undo Creation Of Comment";
        }

        @Override
        public String getRedoPresentationName() {
            return "Redo Creation Of Comment ";
        }
    }
}
