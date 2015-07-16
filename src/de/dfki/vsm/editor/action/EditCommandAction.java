package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.CmdBadge;
import de.dfki.vsm.editor.WorkSpacePanel;

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
        mCmdBadge.setSelected();
        mCmdBadge.revalidate();
        mCmdBadge.repaint();
    }
}
