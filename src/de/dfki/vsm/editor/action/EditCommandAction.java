package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.CmdBadge;
import de.dfki.vsm.editor.event.NodeSelectedEvent;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.util.evt.EventDispatcher;

/**
 * Sergio Soto
 */
public class EditCommandAction extends EditorAction {
    private final WorkSpacePanel mWorkSpace;
    private final CmdBadge  mCmdBadge;

    public EditCommandAction(WorkSpacePanel workSpace, CmdBadge c) {
        mWorkSpace = workSpace;
        mCmdBadge  = c;
    }

    @Override
    public void run() {
        EventDispatcher.getInstance().convey(new NodeSelectedEvent(this, mCmdBadge.getNode().getDataNode()));
        mCmdBadge.setEditMode();
        mCmdBadge.revalidate();
        mCmdBadge.repaint();
    }
}
