package de.dfki.vsm.editor.action;

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.dialog.VarDefDialog;
import de.dfki.vsm.editor.event.NodeSelectedEvent;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.util.evt.EventDispatcher;
import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.UndoManager;
import java.util.LinkedList;

/**
 * Sergio Soto
 */
public class AddVariableAction extends EditorAction {
     //
    private final EventDispatcher mDispatcher
            = EventDispatcher.getInstance();
    
    private LinkedList<VariableDefinition> varDefsList;
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
        VariableDefinition varDef = new VarDefDialog(mCurrentSuperNode, null).run();
        // Add the new variable definition if creation was successful
        if (varDef != null) {
            mUndoManager.addEdit(new Edit());
            UndoAction.getInstance().refreshUndoState();
            RedoAction.getInstance().refreshRedoState();
            mCurrentSuperNode.addVarDef(varDef);
            //varDefsList.add(varDef);
            // launch event to update element editor
            mDispatcher.convey(new NodeSelectedEvent(this,mCurrentSuperNode));
            // update workspace
            EditorInstance.getInstance().refresh();
        }
    }

    private class Edit extends AbstractUndoableEdit {
        @Override
        public void undo() throws CannotUndoException {
            if(mCurrentSuperNode.getVarDefList().size()-1 >= 0) {
                VariableDefinition varDef = mCurrentSuperNode.getVarDefAt(mCurrentSuperNode.getVarDefList().size() - 1);
                varDefsList.add(varDef);
                mCurrentSuperNode.removeVarDefAt(mCurrentSuperNode.getVarDefList().size() - 1);
                mWorkSpace.revalidate();
                mWorkSpace.repaint(100);
                mWorkSpace.refresh();
                mDispatcher.convey(new NodeSelectedEvent(this, mCurrentSuperNode));
            }
        }

        @Override
        public void redo() throws CannotRedoException {
            VariableDefinition varDef  = varDefsList.remove();
            mCurrentSuperNode.addVarDef(varDef);
            varDefsList.add(varDef);
            mWorkSpace.revalidate();
            mWorkSpace.repaint(100);
            mWorkSpace.refresh();
            mDispatcher.convey(new NodeSelectedEvent(this,mCurrentSuperNode));
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
