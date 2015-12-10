package de.dfki.vsm.editor.util;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.Edge;
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.util.tpl.TPLTuple;

//~--- JDK imports ------------------------------------------------------------

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

/**
 * @author Patrick Gebhard
 * @author Not me
 */
public class AltStartNodeManager {
    public Edge                                                    mEdge;
    public HashMap<TPLTuple<String, Node>, TPLTuple<String, Node>> mAltStartNodeMap;

    public AltStartNodeManager(Edge edge) {
        mEdge            = edge;
        mAltStartNodeMap = mEdge.getCopyOfAltStartNodeMap();
    }

    public void saveAltStartNodeMap() {
        mEdge.setAltStartNodeMap(mAltStartNodeMap);
    }

    public void loadAltStartNodeMap() {
        mAltStartNodeMap = mEdge.getCopyOfAltStartNodeMap();
    }

    public void removeAltStartNode(String id) {
        TPLTuple<String, Node> pair = null;

        for (TPLTuple<String, Node> p : mAltStartNodeMap.keySet()) {
            if (p.getFirst().equals(id)) {
                pair = p;

                break;
            }
        }

        if (pair != null) {
            mAltStartNodeMap.remove(pair);
        }
    }

    public Vector<Node> getSubstitutableStartNodes() {
        Vector<Node> substitutableStartNodeList = new Vector<Node>();

        for (Node node : ((SuperNode) mEdge.getTargetNode()).getStartNodeMap().values()) {
            substitutableStartNodeList.add(node);
        }

        //
        for (TPLTuple<String, Node> startNodePair : mAltStartNodeMap.keySet()) {
            if (!startNodePair.getFirst().equals("none")) {
                substitutableStartNodeList.remove(startNodePair.getSecond());
            }
        }

        return substitutableStartNodeList;
    }

    public Vector<Node> getValidAltStartNodesFor(String id) {
        Vector<Node> validAltStartNodeList = new Vector<Node>();

        // /
        SuperNode targetNode   = (SuperNode) mEdge.getTargetNode();
        Node      selectedNode = targetNode.getChildNodeById(id);

        System.err.println("Selected node=" + selectedNode);

        // /
        if (selectedNode == null) {

            // rwchability map
            HashMap<Node, Vector<Node>> reachableNodeMap = new HashMap<Node, Vector<Node>>();

            for (Node node : ((SuperNode) mEdge.getTargetNode()).getStartNodeMap().values()) {
                reachableNodeMap.put(node, node.getReachableNodeList());
            }

            for (TPLTuple<String, Node> p : mAltStartNodeMap.values()) {
                reachableNodeMap.put(p.getSecond(), p.getSecond().getReachableNodeList());
            }

            // Vector<Node>
            Vector<Node> possililities = ((SuperNode) mEdge.getTargetNode()).getNodeAndSuperNodeList();
            Vector<Node> finals        = new Vector<Node>();

            // /
            for (Node node : possililities) {
                System.err.println("looking if " + node.getId() + " is valid");

                boolean  valid = true;
                Iterator it    = reachableNodeMap.entrySet().iterator();

                while (it.hasNext()) {
                    Map.Entry    pairs = (Map.Entry) it.next();
                    Node         n     = (Node) pairs.getKey();
                    Vector<Node> v     = (Vector<Node>) pairs.getValue();

                    if (v.contains(node)) {
                        valid = false;
                    }
                }

                if (valid) {
                    finals.add(node);
                }
            }

            /////
            for (Node n : finals) {
                validAltStartNodeList.add(n);
            }

            /////
            for (Node n : finals) {
                Vector<Node> reverse = n.getReachableNodeList();

                ////
                Iterator it = mAltStartNodeMap.entrySet().iterator();

                while (it.hasNext()) {
                    Map.Entry              pairs = (Map.Entry) it.next();
                    TPLTuple<String, Node> p1    = (TPLTuple<String, Node>) pairs.getKey();
                    TPLTuple<String, Node> p2    = (TPLTuple<String, Node>) pairs.getValue();

                    if (reverse.contains(p2.getSecond())) {

                        // / remove p2 from finals
                        validAltStartNodeList.remove(n);
                    }
                }

                ////
            }

            // return finals;
        } else {
            Vector<Node> reachableNodeList = selectedNode.getReachableNodeList();

            for (Node node : reachableNodeList) {
                if (!node.getId().equals(id)) {
                    validAltStartNodeList.add(node);
                }
            }
        }

        ////
        return validAltStartNodeList;
    }

    public void createAltStartNode(String s, String a) {
        String                 x     = (s.equals("none")
                                        ? ""
                                        : s);
        Node                   n1    = ((SuperNode) mEdge.getTargetNode()).getChildNodeById(x);
        Node                   n2    = ((SuperNode) mEdge.getTargetNode()).getChildNodeById(a);
        TPLTuple<String, Node> pair1 = new TPLTuple<String, Node>(x, n1);
        TPLTuple<String, Node> pair2 = new TPLTuple<String, Node>(a, n2);

        mAltStartNodeMap.put(pair1, pair2);
    }
}
