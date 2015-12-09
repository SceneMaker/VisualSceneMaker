package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.project.sceneflow.SceneFlowEditor;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.editor.util.IDManager;
import de.dfki.vsm.model.sceneflow.diagram.edges.GuardedEdge;
import de.dfki.vsm.model.sceneflow.diagram.edges.AbstractEdge;
import de.dfki.vsm.model.sceneflow.diagram.edges.ForkingEdge;
import de.dfki.vsm.model.sceneflow.diagram.edges.InterruptEdge;
import de.dfki.vsm.model.sceneflow.diagram.BasicNode;
import de.dfki.vsm.model.sceneflow.diagram.edges.RandomEdge;
import de.dfki.vsm.model.sceneflow.diagram.edges.TimeoutEdge;
import java.util.ArrayList;

//~--- JDK imports ------------------------------------------------------------
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Set;
import java.util.Vector;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;

/**
 * @author Patrick Gebhard
 */
public class PasteNodesAction extends EditorAction {

    Set<CreateNodeAction> mCreateNodeActions = new HashSet<CreateNodeAction>();
    WorkSpacePanel mWorkSpace = null;
    Hashtable<BasicNode, ArrayList<GuardedEdge>> mNodesCEdges = new Hashtable();
    Hashtable<BasicNode, ArrayList<RandomEdge>> mNodesPEdges = new Hashtable();
    Hashtable<BasicNode, ArrayList<ForkingEdge>> mNodesFEdges = new Hashtable();
    Hashtable<BasicNode, ArrayList<InterruptEdge>> mNodesIEdges = new Hashtable();
    Hashtable<BasicNode, AbstractEdge> mNodesDefaultEdge = new Hashtable<BasicNode, AbstractEdge>();
    SceneFlowEditor mSceneFlowEditor;

    public PasteNodesAction(WorkSpacePanel workSpace) {
        mWorkSpace = workSpace;
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

        // Remove edges
        for (BasicNode node : nodes) {
            if (node.hasEdge()) {
                switch (node.getFlavour()) {
                    case CNODE:
                        final ArrayList<GuardedEdge> ces = node.getCEdgeList();

                        mNodesCEdges.put(node, ces);
                        node.removeAllCEdges();

                        if (node.hasDEdge()) {
                            mNodesDefaultEdge.put(node, node.getDedge());
                            node.removeDEdge();
                        }

                        break;

                    case PNODE:
                        final ArrayList<RandomEdge> pes = node.getPEdgeList();

                        mNodesPEdges.put(node, pes);
                        node.removeAllPEdges();

                        break;

                    case FNODE:
                        final ArrayList<ForkingEdge> fes = node.getFEdgeList();

                        mNodesFEdges.put(node, fes);
                        node.removeAllFEdges();

                        break;

                    case INODE:
                        final ArrayList<InterruptEdge> ies = node.getIEdgeList();

                        mNodesIEdges.put(node, ies);
                        node.removeAllIEdges();

                        break;

                    case TNODE:
                        mNodesDefaultEdge.put(node, node.getDedge());
                        node.removeDEdge();

                        break;

                    case ENODE:
                        mNodesDefaultEdge.put(node, node.getDedge());
                        node.removeDEdge();

                        break;

                    case NONE:

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
                final ArrayList<GuardedEdge> ces = mNodesCEdges.get(node);

                for (GuardedEdge c : ces) {
                    CreateEdgeAction cea = new CreateEdgeAction(mWorkSpace, mWorkSpace.getNode(node.getId()),
                            mWorkSpace.getNode(c.getTarget()), c,
                            de.dfki.vsm.editor.Edge.TYPE.CEDGE);

                    cea.run();
                }
            }

            // pedge
            if (mNodesPEdges.containsKey(node)) {
                final ArrayList<RandomEdge> pes = mNodesPEdges.get(node);

                for (RandomEdge p : pes) {
                    CreateEdgeAction cea = new CreateEdgeAction(mWorkSpace, mWorkSpace.getNode(node.getId()),
                            mWorkSpace.getNode(p.getTarget()), p,
                            de.dfki.vsm.editor.Edge.TYPE.PEDGE);

                    cea.run();
                }
            }

            // fedge
            if (mNodesFEdges.containsKey(node)) {
                final ArrayList<ForkingEdge> fes = mNodesFEdges.get(node);

                for (ForkingEdge f : fes) {
                    CreateEdgeAction cea = new CreateEdgeAction(mWorkSpace, mWorkSpace.getNode(node.getId()),
                            mWorkSpace.getNode(f.getTarget()), f,
                            de.dfki.vsm.editor.Edge.TYPE.FEDGE);

                    cea.run();
                }
            }

            // iedge
            if (mNodesIEdges.containsKey(node)) {
                final ArrayList<InterruptEdge> ies = mNodesIEdges.get(node);

                for (InterruptEdge i : ies) {
                    CreateEdgeAction cea = new CreateEdgeAction(mWorkSpace, mWorkSpace.getNode(node.getId()),
                            mWorkSpace.getNode(i.getTarget()), i,
                            de.dfki.vsm.editor.Edge.TYPE.IEDGE);

                    cea.run();
                }
            }

            // dedge
            if (mNodesDefaultEdge.containsKey(node)) {
                AbstractEdge e = mNodesDefaultEdge.get(node);
                CreateEdgeAction cea = null;

                if (TimeoutEdge.class.isInstance(e)) {
                    cea = new CreateEdgeAction(mWorkSpace, mWorkSpace.getNode(node.getId()),
                            mWorkSpace.getNode(e.getTarget()), e,
                            de.dfki.vsm.editor.Edge.TYPE.TEDGE);
                } else {
                    cea = new CreateEdgeAction(mWorkSpace, mWorkSpace.getNode(node.getId()),
                            mWorkSpace.getNode(e.getTarget()), e,
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
