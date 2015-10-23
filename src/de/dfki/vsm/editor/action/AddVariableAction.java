package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.dialog.VarDefDialog;
import de.dfki.vsm.editor.event.NodeSelectedEvent;
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.model.sceneflow.definition.VarDef;
import de.dfki.vsm.util.evt.EventDispatcher;

/**
 * Sergio Soto
 */
public class AddVariableAction extends EditorAction {
    private final Node mCurrentSuperNode;

    public AddVariableAction(Node currentSuperNode) {
        mCurrentSuperNode = currentSuperNode;
    }

    @Override
    public void run() {
        VarDef varDef = new VarDefDialog(mCurrentSuperNode, null).run();

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
