package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;

//~--- JDK imports ------------------------------------------------------------

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 *
 * @author Patrick Gebhard
 */
public class StraightenEdgeAction {
    private de.dfki.vsm.editor.Edge mGUIEdge = null;
    private WorkSpacePanel               mWorkSpace;

    public StraightenEdgeAction(WorkSpacePanel workSpace, de.dfki.vsm.editor.Edge edge) {
        mWorkSpace = workSpace;
        mGUIEdge   = edge;
    }

    public ActionListener getActionListener() {
        return new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                mGUIEdge.straightenEdge();

                // renew graphical representation on work space
                mWorkSpace.revalidate();
                mWorkSpace.repaint();
            }
        };
    }
}
