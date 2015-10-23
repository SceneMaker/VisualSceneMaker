package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.Node;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.editor.dialog.CmdDialog;
import de.dfki.vsm.editor.event.NodeSelectedEvent;
import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.util.evt.EventDispatcher;

/**
 * Sergio Soto
 */
public class AddCommandAction extends EditorAction {
    private final WorkSpacePanel mWorkSpace;
    private final Node      mNode;

    public AddCommandAction(WorkSpacePanel workSpace, Node node) {
        mWorkSpace = workSpace;
        mNode      = node;
    }

    @Override
    public void run() {
        Command cmd = new CmdDialog(null).run();

        if (cmd != null) {
            EventDispatcher.getInstance().convey(new NodeSelectedEvent(this, mNode.getDataNode()));
            mNode.getDataNode().addCmd(cmd);
            // mListModel.addElement(cmd);
            EditorInstance.getInstance().refresh();
        }
    }
}
