package de.dfki.vsm.editor.action;

import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.editor.WorkSpace;
import de.dfki.vsm.editor.dialog.VarDefDialog;
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.model.sceneflow.definition.VarDef;


/**
 * Sergio Soto
 */
public class AddVariableAction extends EditorAction {

    private final WorkSpace   mWorkSpace;

    public AddVariableAction(WorkSpace workSpace) {
        mWorkSpace = workSpace;
    }

    @Override
    public void run() {
        
        Node dataNode = mWorkSpace.getProject().getSceneFlow();
         VarDef varDef = new VarDefDialog(dataNode, null).run();
        // Add the new variable definition if the creation was successful
        if (varDef != null) {
            dataNode.addVarDef(varDef);
          //  mListModel.addElement(varDef);
            Editor.getInstance().update();
        }
    
    }

}
