
/*
* SceneflowEditor - IDManager
 */
package de.dfki.vsm.editor.util;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.CEdge;
import de.dfki.vsm.model.sceneflow.FEdge;
import de.dfki.vsm.model.sceneflow.IEdge;
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.model.sceneflow.PEdge;
import de.dfki.vsm.model.sceneflow.SuperNode;

//~--- JDK imports ------------------------------------------------------------

import java.util.Collections;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.Vector;

/**
 * IDManager provides unique ids for nodes and supernodes
 *
 * @author Patrick Gebhard
 */
public class IDManager {
    private List<Integer> mSuperNodeIDs = new LinkedList<Integer>();
    private List<Integer> mNodeIDs      = new LinkedList<Integer>();

    public IDManager() {}

    public IDManager(SuperNode superNode) {
        if (superNode != null) {
            Vector<SuperNode> sns = new Vector<SuperNode>();

            sns.add(superNode);
            getAllIDs(sns);
            Collections.sort(mSuperNodeIDs);
            Collections.sort(mNodeIDs);
        }
    }

    private void getAllIDs(Vector<SuperNode> supernodes) {
        for (SuperNode sn : supernodes) {

            // only scan for supernodes and nodes
            if (!de.dfki.vsm.model.sceneflow.SceneFlow.class.isInstance(sn)) {
                mSuperNodeIDs.add(new Integer(sn.getId().substring(1)));
            }

            // Set<Node> ns = sn.getNodeSet();
            Vector<Node> ns = sn.getNodeList();

            collectNodeIDs(ns);

            // Set<SuperNode> sns = sn.getSuperNodeSet();
            Vector<SuperNode> sns = sn.getSuperNodeList();    // .getSuperNodeSet();

            getAllIDs(sns);
        }
    }

    private void collectNodeIDs(Vector<Node> ns) {
        for (Node n : ns) {
            String id = n.getId();

            if (id.startsWith("N")) {
                mNodeIDs.add(new Integer(n.getId().substring(1)));
            }
        }
    }

    public void setID(de.dfki.vsm.editor.Node n) {
        String  idStr = n.getDataNode().getId().substring(1);
        Integer id    = new Integer(idStr);

        if (SuperNode.class.isInstance(n.getDataNode())) {
            if (!mSuperNodeIDs.contains(id)) {

                // System.out.println("id added for supernode!");
                mSuperNodeIDs.add(id);
                Collections.sort(mSuperNodeIDs);
            }
        } else {
            if (!mNodeIDs.contains(id)) {

                // System.out.println("id added for node!");
                mNodeIDs.add(id);
                Collections.sort(mNodeIDs);
            }
        }
    }

    public String getNextFreeSuperNodeID() {
        int freeID = 1;

        for (int i = 0; i < mSuperNodeIDs.size(); i++) {
            if (freeID == mSuperNodeIDs.get(i)) {
                freeID++;
            } else {
                break;
            }
        }

        mSuperNodeIDs.add(new Integer(freeID));
        Collections.sort(mSuperNodeIDs);

        return "S" + freeID;
    }

    public String getNextFreeNodeID() {
        int freeID = 1;

        for (int i = 0; i < mNodeIDs.size(); i++) {
            if (freeID == mNodeIDs.get(i)) {
                freeID++;
            } else {
                break;
            }
        }

        mNodeIDs.add(new Integer(freeID));
        Collections.sort(mNodeIDs);

        return "N" + freeID;
    }

    public void freeID(de.dfki.vsm.editor.Node n) {
        String  idStr = n.getDataNode().getId().substring(1);
        Integer id    = new Integer(idStr);

        if (SuperNode.class.isInstance(n.getDataNode())) {
            mSuperNodeIDs.remove(id);
            Collections.sort(mSuperNodeIDs);
        } else {
            mNodeIDs.remove(id);
            Collections.sort(mNodeIDs);
        }
    }

    /*
     * resets recursively all ids of a given node or supernode and used edges.
     */
    public void reassignAllIDs(Set<Node> nodes) {

        // DEBUG System.out.println("reassignAllIDs");
        Hashtable<String, String> relationOldNewIDs = new Hashtable<String, String>();
        Vector<Node>              nodesVector       = new Vector<Node>();

        for (Node n : nodes) {
            nodesVector.add(n);
        }

        relationOldNewIDs = reassignNodesID(nodesVector, relationOldNewIDs);
        reassignEdgesID(nodesVector, relationOldNewIDs);
        reassignStartNodeIDs(nodesVector, relationOldNewIDs);

        // TODO reassign alternative Startnode information in Edges
    }

    private Hashtable<String, String> reassignNodesID(Vector<Node> nodes, Hashtable<String, String> lastOldNewIDRef) {
        Hashtable<String, String> currentOldNewIDRef = lastOldNewIDRef;

        for (Node node : nodes) {
            if (SuperNode.class.isInstance(node)) {
                String oldID = node.getId();
                String newID = getNextFreeSuperNodeID();

                // DEBUG
                // System.out.println("Supernode " + node.getName() + " has old id " + oldID + " gets " + newID);
                node.setId(newID);
                currentOldNewIDRef.put(oldID, newID);

                Vector<Node> childNodes = ((SuperNode) node).getNodeAndSuperNodeList();

                // reassign recursively other nodes id
                currentOldNewIDRef = reassignNodesID(childNodes, currentOldNewIDRef);
            } else {
                String oldID = node.getId();
                String newID = getNextFreeNodeID();

                // DEBUG
                // System.out.println("Node " + node.getName() + " has old id " + oldID + " gets " + newID);
                node.setId(newID);
                currentOldNewIDRef.put(oldID, newID);
            }
        }

        return currentOldNewIDRef;
    }

    private void reassignSubSuperNodeStartNodeIDs(SuperNode sn, Hashtable<String, String> relationOldNewIDRef) {
        System.out.println("Checking start node IDs of sub super node " + sn.getId());

        HashMap<String, Node> newSNM = new HashMap<String, Node>();
        HashMap<String, Node> snm    = sn.getStartNodeMap();

        for (String key : snm.keySet()) {

            // DEBUG
            // System.out.println("\treassign old start node id " + key + " to " + relationOldNewIDRef.get(key));
            newSNM.put(relationOldNewIDRef.get(key), snm.get(key));
        }

        sn.setStartNodeMap(newSNM);

        // now go for the sub super nodes
        for (SuperNode sNode : sn.getSuperNodeList()) {

            // reassign start node ids of sub super nodes.
            reassignSubSuperNodeStartNodeIDs(sNode, relationOldNewIDRef);
        }
    }

    private void reassignStartNodeIDs(Vector<Node> nodes, Hashtable<String, String> relationOldNewIDRef) {
        Vector<Node> subNodes = new Vector<Node>();

        for (Node node : nodes) {
            if (node instanceof SuperNode) {

                // System.out.println("Checking start node IDs of super node " + node.getId());
                HashMap<String, Node> newSNM = new HashMap<String, Node>();
                SuperNode             sn     = (SuperNode) node;
                HashMap<String, Node> snm    = sn.getStartNodeMap();

                for (String key : snm.keySet()) {

                    // DEBUG
                    // System.out.println("\treassign old start node id " + key + " to " + relationOldNewIDRef.get(key));
                    newSNM.put(relationOldNewIDRef.get(key), snm.get(key));
                }

                sn.setStartNodeMap(newSNM);

                // now go for the sub super nodes
                for (SuperNode sNode : sn.getSuperNodeList()) {

                    // reassign start node ids of sub super nodes.
                    reassignSubSuperNodeStartNodeIDs(sNode, relationOldNewIDRef);
                }
            }
        }
    }

    private void reassignEdgesID(Vector<Node> nodes, Hashtable<String, String> relationOldNewIDRef) {
        for (Node node : nodes) {
            if (node.hasEdge()) {
                switch (node.getFlavour()) {
                case CNODE :
                    Vector<CEdge> cEdgeList        = node.getCEdgeList();
                    Vector<CEdge> invalidCEdgeList = new Vector<CEdge>();

                    for (CEdge c : cEdgeList) {
                        String newID = relationOldNewIDRef.get(c.getTarget());

                        if (newID != null) {
                            c.setTarget(newID);
                        } else {
                            invalidCEdgeList.add(c);
                        }
                    }

                    if (invalidCEdgeList.size() > 0) {
                        for (CEdge ce : invalidCEdgeList) {
                            cEdgeList.remove(ce);
                        }
                    }

                    // check possible default edges
                    if (node.hasDEdge()) {

                        // DEBUG System.out.println("+ default edge");
                        String teID = relationOldNewIDRef.get(node.getDedge().getTarget());

                        if (teID != null) {
                            node.getDedge().setTarget(teID);
                        } else {

                            // DEBUG System.err.println("unvalid tedge (no target) - removing edge.");
                            node.removeDEdge();
                        }
                    }

                    break;

                case PNODE :

                    // DEBUG System.out.println("pedge(s)");
                    Vector<PEdge> pes           = node.getPEdgeList();
                    Vector<PEdge> unvalidPEdges = new Vector<PEdge>();

                    for (PEdge p : pes) {
                        String newID = relationOldNewIDRef.get(p.getTarget());

                        if (newID != null) {
                            p.setTarget(newID);
                        } else {

                            // DEBUG System.err.println("unvalid pedge (no target) - removing edge.");
                            unvalidPEdges.add(p);
                        }
                    }

                    if (unvalidPEdges.size() > 0) {
                        for (PEdge ce : unvalidPEdges) {
                            pes.remove(ce);
                        }
                    }

                    break;

                case FNODE :

                    // DEBUG System.out.println("fedge(s)");
                    Vector<FEdge> fes           = node.getFEdgeList();
                    Vector<FEdge> unvalidFEdges = new Vector<FEdge>();

                    for (FEdge f : fes) {
                        String newID = relationOldNewIDRef.get(f.getTarget());

                        if (newID != null) {
                            f.setTarget(newID);
                        } else {

                            // DEBUG System.err.println("unvalid fedge (no target) - removing edge.");
                            unvalidFEdges.add(f);
                        }
                    }

                    if (unvalidFEdges.size() > 0) {
                        for (FEdge ce : unvalidFEdges) {
                            fes.remove(ce);
                        }
                    }

                    break;

                case INODE :

                    // DEBUG System.out.println("iedge(s)");
                    Vector<IEdge> ies           = node.getIEdgeList();
                    Vector<IEdge> unvalidIEdges = new Vector<IEdge>();

                    for (IEdge i : ies) {
                        String newID = relationOldNewIDRef.get(i.getTarget());

                        if (newID != null) {
                            i.setTarget(newID);
                        } else {

                            // DEBUG System.err.println("unvalid iedge (no target) - removing edge.");
                            unvalidIEdges.add(i);
                        }
                    }

                    if (unvalidIEdges.size() > 0) {
                        for (IEdge ce : unvalidIEdges) {
                            ies.remove(ce);
                        }
                    }

                    break;

                case TNODE :

                    // DEBUG System.out.println("tedge");
                    String teID = relationOldNewIDRef.get(node.getDedge().getTarget());

                    if (teID != null) {
                        node.getDedge().setTarget(teID);
                    } else {

                        // DEBUG System.err.println("unvalid tedge (no target) - removing edge.");
                        node.removeDEdge();
                    }

                    break;

                case ENODE :
                    String eeID = relationOldNewIDRef.get(node.getDedge().getTarget());

                    if (eeID != null) {
                        node.getDedge().setTarget(eeID);
                    } else {
                        node.removeDEdge();
                    }

                    break;

                case NONE :
                    String neID = relationOldNewIDRef.get(node.getDedge().getTarget());

                    if (neID != null) {
                        node.getDedge().setTarget(neID);
                    } else {
                        node.removeDEdge();
                    }

                    break;
                }
            }

            if (SuperNode.class.isInstance(node)) {
                Vector<Node> childNodes = ((SuperNode) node).getNodeAndSuperNodeList();

                reassignEdgesID(childNodes, relationOldNewIDRef);
            }
        }
    }
}
