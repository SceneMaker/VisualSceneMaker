
/*
* To change this license header, choose License Headers in Project Properties.
* To change this template file, choose Tools | Templates
* and open the template in the editor.
 */
package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;

//~--- JDK imports ------------------------------------------------------------

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 *
 * @author Souza Putra
 */
public class ShortestEdgeAction {
    private de.dfki.vsm.editor.Edge mGUIEdge = null;
    private WorkSpacePanel               mWorkSpace;

    public ShortestEdgeAction(WorkSpacePanel workSpace, de.dfki.vsm.editor.Edge edge) {
        mWorkSpace = workSpace;
        mGUIEdge   = edge;
    }

    public ActionListener getActionListener() {
        return new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                mGUIEdge.rebuildEdgeNicely();

                // renew graphical representation on work space
                mWorkSpace.revalidate();
                mWorkSpace.repaint();
            }
        };
    }
}
