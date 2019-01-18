
/*
* SceneflowEditor - IDManager
 */
package de.dfki.vsm.editor.util;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.ForkingEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge;
import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.edge.RandomEdge;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import java.util.ArrayList;

//~--- JDK imports ------------------------------------------------------------

import java.util.Collections;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

/**
 * IDManager provides unique ids for nodes and supernodes
 *
 * @author Patrick Gebhard
 */
public class IDManager {

    //TODO: Improve IDManager with Unique Data Structure
    private List<Integer> mSuperNodeIDs = new LinkedList<>();
    private List<Integer> mNodeIDs      = new LinkedList<>();

    public IDManager() {}

    public IDManager(SuperNode superNode) {
        if (superNode != null) {
            ArrayList<SuperNode> sns = new ArrayList<>();

            sns.add(superNode);
            getAllIDs(sns);
            Collections.sort(mSuperNodeIDs);
            Collections.sort(mNodeIDs);
        }
    }

    private void getAllIDs(ArrayList<SuperNode> supernodes) {
        for (SuperNode sn : supernodes) {

            // only scan for supernodes and nodes
            if (!de.dfki.vsm.model.sceneflow.chart.SceneFlow.class.isInstance(sn)) {
                mSuperNodeIDs.add(new Integer(sn.getId().substring(1)));
            }

            // Set<Node> ns = sn.getNodeSet();
            ArrayList<BasicNode> ns = sn.getNodeList();

            collectNodeIDs(ns);

            // Set<SuperNode> sns = sn.getSuperNodeSet();
            ArrayList<SuperNode> sns = sn.getSuperNodeList();    // .getSuperNodeSet();

            getAllIDs(sns);
        }
    }

    private void collectNodeIDs(ArrayList<BasicNode> ns) {
        for (BasicNode n : ns) {
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

        for (Integer mSuperNodeID : mSuperNodeIDs) {
            if (freeID == mSuperNodeID) {
                freeID++;
            } else {
                break;
            }
        }

        mSuperNodeIDs.add(freeID);
        Collections.sort(mSuperNodeIDs);

        return "S" + freeID;
    }

    public String getNextFreeNodeID() {
        int freeID = 1;

        for (Integer mNodeID : mNodeIDs) {
            if (freeID == mNodeID) {
                freeID++;
            } else {
                break;
            }
        }

        mNodeIDs.add(freeID);
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
    public void reassignAllIDs(Set<BasicNode> nodes) {

        // DEBUG System.out.println("reassignAllIDs");
        Hashtable<String, String> relationOldNewIDs = new Hashtable<>();

        ArrayList<BasicNode> nodesVector = new ArrayList<>(nodes);

        relationOldNewIDs = reassignNodesID(nodesVector, relationOldNewIDs);
        reassignEdgesID(nodesVector, relationOldNewIDs);
        reassignStartNodeIDs(nodesVector, relationOldNewIDs);

        // TODO reassign alternative Startnode information in Edges
    }

    private Hashtable<String, String> reassignNodesID(ArrayList<BasicNode> nodes, Hashtable<String, String> lastOldNewIDRef) {
        Hashtable<String, String> currentOldNewIDRef = lastOldNewIDRef;

        for (BasicNode node : nodes) {
            if (SuperNode.class.isInstance(node)) {
                String oldID = node.getId();
                String newID = getNextFreeSuperNodeID();

                // DEBUG
                // System.out.println("Supernode " + node.getName() + " has old id " + oldID + " gets " + newID);
                node.setId(newID);
                currentOldNewIDRef.put(oldID, newID);

                ArrayList<BasicNode> childNodes = ((SuperNode) node).getNodeAndSuperNodeList();

                // reassign recursively other nodes id
                currentOldNewIDRef = reassignNodesID(childNodes, currentOldNewIDRef);
            } else {
                String oldID = node.getId();
                String newID = getNextFreeNodeID();

                // DEBUG
                // System.out.println("BasicNode " + node.getName() + " has old id " + oldID + " gets " + newID);
                node.setId(newID);
                currentOldNewIDRef.put(oldID, newID);
            }
        }

        return currentOldNewIDRef;
    }

    private void reassignSubSuperNodeStartNodeIDs(SuperNode sn, Hashtable<String, String> relationOldNewIDRef) {
        System.out.println("Checking start node IDs of sub super node " + sn.getId());

        HashMap<String, BasicNode> newSNM = new HashMap<>();
        HashMap<String, BasicNode> snm    = sn.getStartNodeMap();

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

    private void reassignStartNodeIDs(ArrayList<BasicNode> nodes, Hashtable<String, String> relationOldNewIDRef) {
        ArrayList<BasicNode> subNodes = new ArrayList<>();

        for (BasicNode node : nodes) {
            if (node instanceof SuperNode) {

                // System.out.println("Checking start node IDs of super node " + node.getId());
                HashMap<String, BasicNode> newSNM = new HashMap<>();
                SuperNode             sn     = (SuperNode) node;
                HashMap<String, BasicNode> snm    = sn.getStartNodeMap();

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
            /*else {
                HashMap<String, BasicNode> newSNM = new HashMap<String, BasicNode>();
                SuperNode             sn     = (SuperNode) node.getParentNode();
                if(sn != null) {
                    HashMap<String, BasicNode> snm = sn.getStartNodeMap();
                    for (String key : snm.keySet()) {
                        newSNM.put(relationOldNewIDRef.get(key), snm.get(key));
                    }
                    sn.setStartNodeMap(newSNM);
                }
            }*/
        }
    }

    private void reassignEdgesID(ArrayList<BasicNode> nodes, Hashtable<String, String> relationOldNewIDRef) {
        for (BasicNode node : nodes) {
            if (node.hasEdge()) {
                switch (node.getFlavour()) {
                case CNODE :
                    ArrayList<GuargedEdge> cEdgeList        = node.getCEdgeList();
                    ArrayList<GuargedEdge> invalidCEdgeList = new ArrayList<>();

                    for (GuargedEdge c : cEdgeList) {
                        String newID = relationOldNewIDRef.get(c.getTargetUnid());

                        if (newID != null) {
                            c.setTargetUnid(newID);
                        } else {
                            invalidCEdgeList.add(c);
                        }
                    }

                    if (invalidCEdgeList.size() > 0) {
                        for (GuargedEdge ce : invalidCEdgeList) {
                            cEdgeList.remove(ce);
                        }
                    }

                    // check possible default edges
                    if (node.hasDEdge()) {
                        reasignDedge(relationOldNewIDRef, node);
                    }

                    break;

                case PNODE :

                    // DEBUG System.out.println("pedge(s)");
                    ArrayList<RandomEdge> pes           = node.getPEdgeList();
                    ArrayList<RandomEdge> unvalidPEdges = new ArrayList<>();

                    for (RandomEdge p : pes) {
                        String newID = relationOldNewIDRef.get(p.getTargetUnid());

                        if (newID != null) {
                            p.setTargetUnid(newID);
                        } else {

                            // DEBUG System.err.println("unvalid pedge (no target) - removing edge.");
                            unvalidPEdges.add(p);
                        }
                    }

                    if (unvalidPEdges.size() > 0) {
                        for (RandomEdge ce : unvalidPEdges) {
                            pes.remove(ce);
                        }
                    }

                    break;

                case FNODE :

                    // DEBUG System.out.println("fedge(s)");
                    ArrayList<ForkingEdge> fes           = node.getFEdgeList();
                    ArrayList<ForkingEdge> unvalidFEdges = new ArrayList<>();

                    for (ForkingEdge f : fes) {
                        String newID = relationOldNewIDRef.get(f.getTargetUnid());

                        if (newID != null) {
                            f.setTargetUnid(newID);
                        } else {

                            // DEBUG System.err.println("unvalid fedge (no target) - removing edge.");
                            unvalidFEdges.add(f);
                        }
                    }

                    if (unvalidFEdges.size() > 0) {
                        for (ForkingEdge ce : unvalidFEdges) {
                            fes.remove(ce);
                        }
                    }

                    break;

                case INODE :

                    // DEBUG System.out.println("iedge(s)");
                    ArrayList<InterruptEdge> ies           = node.getIEdgeList();
                    ArrayList<InterruptEdge> unvalidIEdges = new ArrayList<>();

                    for (InterruptEdge i : ies) {
                        String newID = relationOldNewIDRef.get(i.getTargetUnid());

                        if (newID != null) {
                            i.setTargetUnid(newID);
                        } else {

                            // DEBUG System.err.println("unvalid iedge (no target) - removing edge.");
                            unvalidIEdges.add(i);
                        }
                    }

                    if (unvalidIEdges.size() > 0) {
                        for (InterruptEdge ce : unvalidIEdges) {
                            ies.remove(ce);
                        }
                    }

                    if(node.hasDEdge()){
                        reasignDedge(relationOldNewIDRef, node);
                    }

                    break;

                case TNODE :
                    // DEBUG System.out.println("tedge");
                    reasignDedge(relationOldNewIDRef, node);
                    break;

                case ENODE :
                    reasignDedge(relationOldNewIDRef, node);
                    break;
                case NONE :
                    reasignDedge(relationOldNewIDRef, node);
                    break;
                }
            }

            if (SuperNode.class.isInstance(node)) {
                ArrayList<BasicNode> childNodes = ((SuperNode) node).getNodeAndSuperNodeList();

                reassignEdgesID(childNodes, relationOldNewIDRef);
            }
        }
    }

    private void reasignDedge(Hashtable<String, String> relationOldNewIDRef, BasicNode node) {
        String eID = relationOldNewIDRef.get(node.getDedge().getTargetUnid());

        if (eID != null) {
            node.getDedge().setTargetUnid(eID);
        } else {
            node.removeDEdge();
        }
    }
}
