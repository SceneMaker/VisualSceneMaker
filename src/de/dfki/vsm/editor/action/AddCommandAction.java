package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.instance.EditorInstance;
import de.dfki.vsm.editor.Node;
import de.dfki.vsm.editor.WorkSpace;
import de.dfki.vsm.editor.dialog.CmdDialog;
import de.dfki.vsm.model.sceneflow.command.Command;

/**
 * Sergio Soto
 */
public class AddCommandAction extends EditorAction {
    private final WorkSpace mWorkSpace;
    private final Node      mNode;

    public AddCommandAction(WorkSpace workSpace, Node node) {
        mWorkSpace = workSpace;
        mNode      = node;
    }

    @Override
    public void run() {
        Command cmd = new CmdDialog(null).run();

        if (cmd != null) {
            mNode.getDataNode().addCmd(cmd);

            // mListModel.addElement(cmd);
            EditorInstance.getInstance().update();
        }
    }
}
