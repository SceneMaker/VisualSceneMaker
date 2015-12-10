package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.dialog.definition.VarDefDialog;
import de.dfki.vsm.editor.event.NodeSelectedEvent;
import de.dfki.vsm.model.sceneflow.diagram.BasicNode;
import de.dfki.vsm.model.sceneflow.definition.VariableDefinition;
import de.dfki.vsm.util.evt.EventDispatcher;

/**
 * Sergio Soto
 */
public class AddVariableAction extends EditorAction {
    private final BasicNode mCurrentSuperNode;

    public AddVariableAction(BasicNode currentSuperNode) {
        mCurrentSuperNode = currentSuperNode;
    }

    @Override
    public void run() {
        VariableDefinition varDef = new VarDefDialog(mCurrentSuperNode, null).run();

        // Add the new variable definition if creation was successful
        if (varDef != null) {
            mCurrentSuperNode.addVarDef(varDef);
            // launch event to update element editor
            EventDispatcher.getInstance().convey(new NodeSelectedEvent(this,mCurrentSuperNode));
            // update workspace
            EditorInstance.getInstance().refresh();
        }
    }
}
