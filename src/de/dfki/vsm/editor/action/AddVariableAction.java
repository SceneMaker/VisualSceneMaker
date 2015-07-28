package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.editor.dialog.VarDefDialog;
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.model.sceneflow.definition.VarDef;

/**
 * Sergio Soto
 */
public class AddVariableAction extends EditorAction {
    private final Node mDataNode;

    public AddVariableAction(Node dataNode) {
        mDataNode = dataNode;
    }

    @Override
    public void run() {
        VarDef varDef = new VarDefDialog(mDataNode, null).run();

        // Add the new variable definition if the creation was successful
        if (varDef != null) {
            mDataNode.addVarDef(varDef);
            // mListModel.addElement(varDef);
            EditorInstance.getInstance().refresh();
        }
    }
}
