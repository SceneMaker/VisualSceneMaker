package de.dfki.vsm.editor.action;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.project.sceneflow.SceneFlowEditor;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.editor.util.IDManager;
import de.dfki.vsm.model.sceneflow.CEdge;
import de.dfki.vsm.model.sceneflow.Edge;
import de.dfki.vsm.model.sceneflow.FEdge;
import de.dfki.vsm.model.sceneflow.IEdge;
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.model.sceneflow.PEdge;
import de.dfki.vsm.model.sceneflow.TEdge;

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
    Set<CreateNodeAction>          mCreateNodeActions = new HashSet<CreateNodeAction>();
    WorkSpacePanel                      mWorkSpace         = null;
    Hashtable<Node, Vector<CEdge>> mNodesCEdges       = new Hashtable<Node, Vector<CEdge>>();
    Hashtable<Node, Vector<PEdge>> mNodesPEdges       = new Hashtable<Node, Vector<PEdge>>();
    Hashtable<Node, Vector<FEdge>> mNodesFEdges       = new Hashtable<Node, Vector<FEdge>>();
    Hashtable<Node, Vector<IEdge>> mNodesIEdges       = new Hashtable<Node, Vector<IEdge>>();
    Hashtable<Node, Edge>          mNodesDefaultEdge  = new Hashtable<Node, Edge>();
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
        Set<Node> nodes = new HashSet<Node>();

        for (Node node : mWorkSpace.getClipBoard()) {
            nodes.add(node.getCopy());
        }

        // be sure to give all copied nodes (and edges) new ids
        im.reassignAllIDs(nodes);

        // Remove edges
        for (Node node : nodes) {
            if (node.hasEdge()) {
                switch (node.getFlavour()) {
                case CNODE :
                    Vector<CEdge> ces = node.getCEdgeList();

                    mNodesCEdges.put(node, ces);
                    node.removeAllCEdges();

                    if (node.hasDEdge()) {
                        mNodesDefaultEdge.put(node, node.getDedge());
                        node.removeDEdge();
                    }

                    break;

                case PNODE :
                    Vector<PEdge> pes = node.getPEdgeList();

                    mNodesPEdges.put(node, pes);
                    node.removeAllPEdges();

                    break;

                case FNODE :
                    Vector<FEdge> fes = node.getFEdgeList();

                    mNodesFEdges.put(node, fes);
                    node.removeAllFEdges();

                    break;

                case INODE :
                    Vector<IEdge> ies = node.getIEdgeList();

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
        for (Node node : nodes) {

            // cedge
            if (mNodesCEdges.containsKey(node)) {
                Vector<CEdge> ces = mNodesCEdges.get(node);

                for (CEdge c : ces) {
                    CreateEdgeAction cea = new CreateEdgeAction(mWorkSpace, mWorkSpace.getNode(node.getId()),
                                               mWorkSpace.getNode(c.getTarget()), c,
                                               de.dfki.vsm.editor.Edge.TYPE.CEDGE);

                    cea.run();
                }
            }

            // pedge
            if (mNodesPEdges.containsKey(node)) {
                Vector<PEdge> pes = mNodesPEdges.get(node);

                for (PEdge p : pes) {
                    CreateEdgeAction cea = new CreateEdgeAction(mWorkSpace, mWorkSpace.getNode(node.getId()),
                                               mWorkSpace.getNode(p.getTarget()), p,
                                               de.dfki.vsm.editor.Edge.TYPE.PEDGE);

                    cea.run();
                }
            }

            // fedge
            if (mNodesFEdges.containsKey(node)) {
                Vector<FEdge> fes = mNodesFEdges.get(node);

                for (FEdge f : fes) {
                    CreateEdgeAction cea = new CreateEdgeAction(mWorkSpace, mWorkSpace.getNode(node.getId()),
                                               mWorkSpace.getNode(f.getTarget()), f,
                                               de.dfki.vsm.editor.Edge.TYPE.FEDGE);

                    cea.run();
                }
            }

            // iedge
            if (mNodesIEdges.containsKey(node)) {
                Vector<IEdge> ies = mNodesIEdges.get(node);

                for (IEdge i : ies) {
                    CreateEdgeAction cea = new CreateEdgeAction(mWorkSpace, mWorkSpace.getNode(node.getId()),
                                               mWorkSpace.getNode(i.getTarget()), i,
                                               de.dfki.vsm.editor.Edge.TYPE.IEDGE);

                    cea.run();
                }
            }

            // dedge
            if (mNodesDefaultEdge.containsKey(node)) {
                Edge             e   = mNodesDefaultEdge.get(node);
                CreateEdgeAction cea = null;

                if (TEdge.class.isInstance(e)) {
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
