package de.dfki.vsm.model.sceneflow.graphics.workspace;

import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.model.sceneflow.BasicNode;
import de.dfki.vsm.model.sceneflow.SuperNode;

import java.awt.*;

/**
 * Created by alvaro on 1/24/17.
 */
public class WorkSpaceSuperNode extends WorkAreaSize {

    private SuperNode superNode;

    public WorkSpaceSuperNode(WorkSpacePanel workSpacePanel, int nodeWidth, int nodeHeight) {
        super(workSpacePanel, nodeWidth, nodeHeight);
    }

    public WorkSpaceSuperNode(WorkSpacePanel workSpacePanel, int nodeWidth, int nodeHeight, SuperNode superNode) {
        super(workSpacePanel, nodeWidth, nodeHeight);
        this.superNode = superNode;
    }

    public Dimension calculate() {
        calculateDimensionForSuperNode();
        super.calculate();
        return new Dimension(width, height);
    }

    private void calculateDimensionForSuperNode() {
        for (BasicNode childNode: superNode.getNodeList()){
            updateWidth(childNode);
            updateHeight(childNode);
        }
    }

    private void updateHeight(BasicNode childNode) {
        if (childNode.getGraphics().getPosition().getYPos() > height) {
            height = childNode.getGraphics().getPosition().getYPos() + nodeHeight;
        }
    }

    private void updateWidth(BasicNode childNode) {
        if (childNode.getGraphics().getPosition().getXPos() > width) {
            width = childNode.getGraphics().getPosition().getXPos() + nodeWidth;
        }
    }


}
