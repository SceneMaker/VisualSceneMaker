package de.dfki.vsm.editor.action;

//package de.dfki.vsm.editor.action;
//
//import de.dfki.vsm.editor.WorkSpace;
//import java.awt.event.ActionEvent;
//import java.awt.event.ActionListener;
//import de.dfki.vsm.editor.SceneFlowEditor;
//import de.dfki.vsm.editor.dialog.ModifyNodeDialog;
//import de.dfki.vsm.editor.util.SceneFlowManager;
//import de.dfki.vsm.mod.sceneflow.Node;
//import de.dfki.vsm.mod.sceneflow.SceneFlow;
//
///**
// * A Modify-Node-Action.
// * @author Not me
// * @author Patrick Gebhard
// */
//public class ModifyNodeAction {
//
//  // The GUI-Node that we want to modify
//  private de.dfki.vsm.editor.Node mGuiNode = null;
//  // The workspace where the GUI-Node is displayed
//  private WorkSpace mWorkSpace = null;
//  // The sceneflow pane where the workspace is dispayed
//  private SceneFlowEditor mSceneFlowPane = null;
//  // The sceneflow manager of the sceneflow pane
//  private SceneFlowManager mSceneFlowManager = null;
//  // The sceneflow that is maintained by the sceneflow manager
//  private SceneFlow mSceneFlow = null;
//  // The DATA-Node connected with the GUI-Node before modification
//  private de.dfki.vsm.mod.sceneflow.Node mOldDataNode = null;
//  // The DATA-Node connected with the GUI-Node after modification
//  private de.dfki.vsm.mod.sceneflow.Node mNewDataNode = null;
//
//  public ModifyNodeAction(de.dfki.vsm.editor.Node node, WorkSpace workSpace) {
//    mGuiNode = node;
//    mOldDataNode = mGuiNode.getDataNode();
//    mWorkSpace = workSpace;
//    mSceneFlowPane = mWorkSpace.getSceneFlowPane();
//    mSceneFlowManager = mSceneFlowPane.getSceneFlowManager();
//    mSceneFlow = mSceneFlowManager.getSceneFlow();
//  }
//
//  // Construction with a data node only
//  public ModifyNodeAction(Node node, WorkSpace workSpace) {
//    //mGuiNode = node;
//    //mOldDataNode = mGuiNode.getDataNode();
//    mOldDataNode = node;
//    mWorkSpace = workSpace;
//    mSceneFlowPane = mWorkSpace.getSceneFlowPane();
//    mSceneFlowManager = mSceneFlowPane.getSceneFlowManager();
//    mSceneFlow = mSceneFlowManager.getSceneFlow();
//  }
//
//  public ActionListener getActionListener() {
//    return new ActionListener() {
//
//      public void actionPerformed(ActionEvent e) {
//        ModifyNodeDialog dialog = new ModifyNodeDialog(mSceneFlow, mOldDataNode);
//        mNewDataNode = dialog.run();
//        // Repaint the GUI-Node to show the changes
//        if (mGuiNode != null) {
//          mGuiNode.update();
//        }
//      }
//    };
//  }
//}




