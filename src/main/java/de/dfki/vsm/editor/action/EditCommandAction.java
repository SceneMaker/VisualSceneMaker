package de.dfki.vsm.editor.action;

import de.dfki.vsm.editor.CmdBadge;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.event.EventDispatcher;
import de.dfki.vsm.event.event.NodeSelectedEvent;

/**
 * Sergio Soto
 */
public class EditCommandAction extends EditorAction {
     //
    private final EventDispatcher mDispatcher
            = EventDispatcher.getInstance();
    
    private final WorkSpacePanel mWorkSpace;
    private final CmdBadge  mCmdBadge;

    public EditCommandAction(WorkSpacePanel workSpace, CmdBadge c) {
        mWorkSpace = workSpace;
        mCmdBadge  = c;
    }

    @Override
    public void run() {
        mDispatcher.convey(new NodeSelectedEvent(this, mCmdBadge.getNode().getDataNode()));
        mCmdBadge.setEditMode();
        mCmdBadge.revalidate();
        mCmdBadge.repaint(100);
    }
}
