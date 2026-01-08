package de.dfki.vsm.model.sceneflow.chart.graphics.workspace;

import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;

import java.awt.*;

/**
 * Created by Alvaro on 1/24/17.
 * Calculate the current area workspace
 */
public abstract class WorkAreaSize {
    protected int width;
    protected int height = 0;
    int nodeWidth;
    int nodeHeight;
    private WorkSpacePanel mWorkSpacePanel;
    WorkAreaSize(WorkSpacePanel workSpacePanel, int nodeWidth, int nodeHeight){
            this.mWorkSpacePanel = workSpacePanel;
            this.nodeHeight = nodeHeight;
            this.nodeWidth = nodeWidth;
        width = 0;
    }

    public Dimension calculate() {
        getSizeFromSceneFlowNodeList();
        getSizeFromSuperNodeList();
        adjustSizeToWorkSpace();
        return new Dimension(width, height);
    }

    private void adjustSizeToWorkSpace() {
        if(mWorkSpacePanel.getSize().height > height){
            height = mWorkSpacePanel.getSize().height;
        }
        if(mWorkSpacePanel.getSize().width> width){
            width = mWorkSpacePanel.getSize().width;
        }
    }

    private void getSizeFromSuperNodeList() {
        for (SuperNode n : mWorkSpacePanel.getSceneFlowEditor().getSceneFlow().getSuperNodeList()) {
            updateWidth(n);
            updateHeight(n);
        }
    }

    private void getSizeFromSceneFlowNodeList() {
        for (BasicNode n : mWorkSpacePanel.getSceneFlowEditor().getSceneFlow().getNodeList()) {
            updateWidth(n);
            updateHeight(n);
        }
    }

    private void updateHeight(BasicNode n) {
        if (n.getGraphics().getPosition().getYPos() > height) {
            height = n.getGraphics().getPosition().getYPos() + nodeHeight;
        }
    }

    private void updateWidth(BasicNode n) {
        if (n.getGraphics().getPosition().getXPos() > width) {
            width = n.getGraphics().getPosition().getXPos() + nodeWidth;
        }
    }
}
