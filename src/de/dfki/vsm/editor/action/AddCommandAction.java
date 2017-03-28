package de.dfki.vsm.editor.action;

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.Node;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.editor.dialog.CmdDialog;
import de.dfki.vsm.editor.event.NodeSelectedEvent;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.util.evt.EventDispatcher;

/**
 * Sergio Soto
 */
public class AddCommandAction extends EditorAction {

    //
    private final EventDispatcher mDispatcher
            = EventDispatcher.getInstance();

    private final WorkSpacePanel mWorkSpace;
    private final Node mNode;

    public AddCommandAction(WorkSpacePanel workSpace, Node node) {
        mWorkSpace = workSpace;
        mNode = node;
    }

    @Override
    public void run() {
        Command cmd = new CmdDialog(null).run();

        if (cmd != null) {
            mDispatcher.convey(new NodeSelectedEvent(this, mNode.getDataNode()));
            mNode.getDataNode().addCmd(cmd);
            // mListModel.addElement(cmd);
            EditorInstance.getInstance().refresh();
        }
    }
}
