package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.ForkingEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.TimeoutEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.RandomEdge;
import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.editor.project.sceneflow.SceneFlowEditor;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.editor.util.IDManager;
import de.dfki.vsm.model.sceneflow.*;

import java.util.*;

//~--- JDK imports ------------------------------------------------------------

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;

/**
 * @author Patrick Gebhard
 */
public class PasteNodesAction extends EditorAction {
    Set<CreateNodeAction>          mCreateNodeActions = new HashSet<CreateNodeAction>();
    WorkSpacePanel                      mWorkSpace         = null;
    Hashtable<BasicNode, ArrayList<GuargedEdge>> mNodesCEdges       = new Hashtable<BasicNode, ArrayList<GuargedEdge>>();
    Hashtable<BasicNode, ArrayList<RandomEdge>> mNodesPEdges       = new Hashtable<BasicNode, ArrayList<RandomEdge>>();
    Hashtable<BasicNode, ArrayList<ForkingEdge>> mNodesFEdges       = new Hashtable<BasicNode, ArrayList<ForkingEdge>>();
    Hashtable<BasicNode, ArrayList<InterruptEdge>> mNodesIEdges       = new Hashtable<BasicNode, ArrayList<InterruptEdge>>();
    Hashtable<BasicNode, AbstractEdge>          mNodesDefaultEdge  = new Hashtable<BasicNode, AbstractEdge>();
    SceneFlowEditor                mSceneFlowEditor;

    public PasteNodesAction(WorkSpacePanel workSpace) {
        mWorkSpace       = workSpace;
        mSceneFlowEditor = mWorkSpace.getSceneFlowEditor();
    }

    protected void pasteNodes() {
        mSceneFlowEditor.setMessageLabelText((mWorkSpace.getClipBoard().size() > 1)
                ? "Pasting selected nodes"
                : "Pasting selected node");

        IDManager im = mWorkSpace.getSceneFlowManager().getIDManager();

        // make a copy
        Set<BasicNode> nodes = new HashSet<BasicNode>();

        for (BasicNode node : mWorkSpace.getClipBoard()) {
            nodes.add(node.getCopy());
        }

        // be sure to give all copied nodes (and edges) new ids
        im.reassignAllIDs(nodes);
        /*if(nodes.size() > 0){//reasing the start node
            BasicNode node = nodes.iterator().next();
            BasicNode parent = node.getParentNode();
            if(parent != null && parent instanceof SuperNode){
                SuperNode superNode =  mWorkSpace.getSceneFlowManager().getCurrentActiveSuperNode();
                HashMap<String, BasicNode> startMap = ((SuperNode)parent).getCopyOfStartNodeMap();
                superNode.setStartNodeMap(startMap);
            }
        }*/

        // Remove edges
        for (BasicNode node : nodes) {
            if (node.hasEdge()) {
                switch (node.getFlavour()) {
                case CNODE :
                    ArrayList<GuargedEdge> ces = node.getCEdgeList();

                    mNodesCEdges.put(node, ces);
                    node.removeAllCEdges();

                    if (node.hasDEdge()) {
                        mNodesDefaultEdge.put(node, node.getDedge());
                        node.removeDEdge();
                    }

                    break;

                case PNODE :
                    ArrayList<RandomEdge> pes = node.getPEdgeList();

                    mNodesPEdges.put(node, pes);
                    node.removeAllPEdges();

                    break;

                case FNODE :
                    ArrayList<ForkingEdge> fes = node.getFEdgeList();

                    mNodesFEdges.put(node, fes);
                    node.removeAllFEdges();

                    break;

                case INODE :
                    ArrayList<InterruptEdge> ies = node.getIEdgeList();

                    mNodesIEdges.put(node, ies);
                    node.removeAllIEdges();

                    break;

                case TNODE :
                    mNodesDefaultEdge.put(node, node.getDedge());
                    node.removeDEdge();

                    break;

                case ENODE :
                    mNodesDefaultEdge.put(node, node.getDedge());
                    node.removeDEdge();

                    break;

                case NONE :

                    // store dedge
                    // mNodesDefaultEdge.put(node, node.getDedge());
                    // remove dedge
                    // node.removeDEdge();
                    break;
                }
            }

            // paste node to the sceneflow
            CreateNodeAction rma = new CreateNodeAction(mWorkSpace, node);

            mCreateNodeActions.add(rma);
            rma.run();
        }

        // now paste each stored edge to sceneflow
        for (BasicNode node : nodes) {

            // cedge
            if (mNodesCEdges.containsKey(node)) {
                ArrayList<GuargedEdge> ces = mNodesCEdges.get(node);

                for (GuargedEdge c : ces) {
                    CreateEdgeAction cea = new CreateEdgeAction(mWorkSpace, mWorkSpace.getNode(node.getId()),
                                               mWorkSpace.getNode(c.getTargetUnid()), c,
                                               de.dfki.vsm.editor.Edge.TYPE.CEDGE);

                    cea.run();
                }
            }

            // pedge
            if (mNodesPEdges.containsKey(node)) {
                ArrayList<RandomEdge> pes = mNodesPEdges.get(node);

                for (RandomEdge p : pes) {
                    CreateEdgeAction cea = new CreateEdgeAction(mWorkSpace, mWorkSpace.getNode(node.getId()),
                                               mWorkSpace.getNode(p.getTargetUnid()), p,
                                               de.dfki.vsm.editor.Edge.TYPE.PEDGE);

                    cea.run();
                }
            }

            // fedge
            if (mNodesFEdges.containsKey(node)) {
                ArrayList<ForkingEdge> fes = mNodesFEdges.get(node);

                for (ForkingEdge f : fes) {
                    CreateEdgeAction cea = new CreateEdgeAction(mWorkSpace, mWorkSpace.getNode(node.getId()),
                                               mWorkSpace.getNode(f.getTargetUnid()), f,
                                               de.dfki.vsm.editor.Edge.TYPE.FEDGE);

                    cea.run();
                }
            }

            // iedge
            if (mNodesIEdges.containsKey(node)) {
                ArrayList<InterruptEdge> ies = mNodesIEdges.get(node);

                for (InterruptEdge i : ies) {
                    CreateEdgeAction cea = new CreateEdgeAction(mWorkSpace, mWorkSpace.getNode(node.getId()),
                                               mWorkSpace.getNode(i.getTargetUnid()), i,
                                               de.dfki.vsm.editor.Edge.TYPE.IEDGE);

                    cea.run();
                }
            }

            // dedge
            if (mNodesDefaultEdge.containsKey(node)) {
                AbstractEdge             e   = mNodesDefaultEdge.get(node);
                CreateEdgeAction cea = null;

                if (TimeoutEdge.class.isInstance(e)) {
                    cea = new CreateEdgeAction(mWorkSpace, mWorkSpace.getNode(node.getId()),
                                               mWorkSpace.getNode(e.getTargetUnid()), e,
                                               de.dfki.vsm.editor.Edge.TYPE.TEDGE);
                } else {
                    cea = new CreateEdgeAction(mWorkSpace, mWorkSpace.getNode(node.getId()),
                                               mWorkSpace.getNode(e.getTargetUnid()), e,
                                               de.dfki.vsm.editor.Edge.TYPE.EEDGE);
                }

                cea.run();
            }
        }
    }

    protected void deleteNodes() {
        for (CreateNodeAction action : mCreateNodeActions) {
            action.delete();
        }
    }

    public void run() {
        pasteNodes();
        UndoAction.getInstance().refreshUndoState();
        RedoAction.getInstance().refreshRedoState();
    }

    private class Edit extends AbstractUndoableEdit {
        @Override
        public void undo() throws CannotUndoException {
            deleteNodes();
        }

        @Override
        public void redo() throws CannotRedoException {
            pasteNodes();
        }

        @Override
        public boolean canUndo() {
            return true;
        }

        @Override
        public boolean canRedo() {
            return true;
        }

        @Override
        public String getUndoPresentationName() {
            return "Undo Pasting Of Nodes ";
        }

        @Override
        public String getRedoPresentationName() {
            return "Redo Pasting Of Nodes ";
        }
    }
}
