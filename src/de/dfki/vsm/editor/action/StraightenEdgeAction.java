package de.dfki.vsm.editor.action;

import de.dfki.vsm.editor.WorkSpace;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 *
 * @author Patrick Gebhard
 */
public class StraightenEdgeAction {

    private WorkSpace mWorkSpace;
    private de.dfki.vsm.editor.Edge mGUIEdge = null;

    public StraightenEdgeAction(WorkSpace workSpace, de.dfki.vsm.editor.Edge edge) {
        mWorkSpace = workSpace;
        mGUIEdge = edge;
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
