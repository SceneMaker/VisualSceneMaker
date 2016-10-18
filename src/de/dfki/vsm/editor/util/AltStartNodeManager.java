package de.dfki.vsm.editor.util;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.AbstractEdge;
import de.dfki.vsm.model.sceneflow.BasicNode;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.util.tpl.TPLTuple;
import java.util.ArrayList;

//~--- JDK imports ------------------------------------------------------------

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * @author Patrick Gebhard
 * @author Gregor Mehlmann
 */
public class AltStartNodeManager {
    public AbstractEdge                                                    mEdge;
    public HashMap<TPLTuple<String, BasicNode>, TPLTuple<String, BasicNode>> mAltStartNodeMap;

    public AltStartNodeManager(AbstractEdge edge) {
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
        TPLTuple<String, BasicNode> pair = null;

        for (TPLTuple<String, BasicNode> p : mAltStartNodeMap.keySet()) {
            if (p.getFirst().equals(id)) {
                pair = p;

                break;
            }
        }

        if (pair != null) {
            mAltStartNodeMap.remove(pair);
        }
    }

    public ArrayList<BasicNode> getSubstitutableStartNodes() {
        ArrayList<BasicNode> substitutableStartNodeList = new ArrayList<BasicNode>();

        for (BasicNode node : ((SuperNode) mEdge.getTargetNode()).getStartNodeMap().values()) {
            substitutableStartNodeList.add(node);
        }

        //
        for (TPLTuple<String, BasicNode> startNodePair : mAltStartNodeMap.keySet()) {
            if (!startNodePair.getFirst().equals("none")) {
                substitutableStartNodeList.remove(startNodePair.getSecond());
            }
        }

        return substitutableStartNodeList;
    }

    public ArrayList<BasicNode> getValidAltStartNodesFor(String id) {
        ArrayList<BasicNode> validAltStartNodeList = new ArrayList<BasicNode>();

        // /
        SuperNode targetNode   = (SuperNode) mEdge.getTargetNode();
        BasicNode      selectedNode = targetNode.getChildNodeById(id);

        System.err.println("Selected node=" + selectedNode);

        // /
        if (selectedNode == null) {

            // rwchability map
            HashMap<BasicNode, ArrayList<BasicNode>> reachableNodeMap = new HashMap<BasicNode, ArrayList<BasicNode>>();

            for (BasicNode node : ((SuperNode) mEdge.getTargetNode()).getStartNodeMap().values()) {
                reachableNodeMap.put(node, node.getReachableNodeList());
            }

            for (TPLTuple<String, BasicNode> p : mAltStartNodeMap.values()) {
                reachableNodeMap.put(p.getSecond(), p.getSecond().getReachableNodeList());
            }

            // ArrayList<Node>
            ArrayList<BasicNode> possililities = ((SuperNode) mEdge.getTargetNode()).getNodeAndSuperNodeList();
            ArrayList<BasicNode> finals        = new ArrayList<BasicNode>();

            // /
            for (BasicNode node : possililities) {
                System.err.println("looking if " + node.getId() + " is valid");

                boolean  valid = true;
                Iterator it    = reachableNodeMap.entrySet().iterator();

                while (it.hasNext()) {
                    Map.Entry    pairs = (Map.Entry) it.next();
                    BasicNode         n     = (BasicNode) pairs.getKey();
                    ArrayList<BasicNode> v     = (ArrayList<BasicNode>) pairs.getValue();

                    if (v.contains(node)) {
                        valid = false;
                    }
                }

                if (valid) {
                    finals.add(node);
                }
            }

            /////
            for (BasicNode n : finals) {
                validAltStartNodeList.add(n);
            }

            /////
            for (BasicNode n : finals) {
                ArrayList<BasicNode> reverse = n.getReachableNodeList();

                ////
                Iterator it = mAltStartNodeMap.entrySet().iterator();

                while (it.hasNext()) {
                    Map.Entry              pairs = (Map.Entry) it.next();
                    TPLTuple<String, BasicNode> p1    = (TPLTuple<String, BasicNode>) pairs.getKey();
                    TPLTuple<String, BasicNode> p2    = (TPLTuple<String, BasicNode>) pairs.getValue();

                    if (reverse.contains(p2.getSecond())) {

                        // / remove p2 from finals
                        validAltStartNodeList.remove(n);
                    }
                }

                ////
            }

            // return finals;
        } else {
            ArrayList<BasicNode> reachableNodeList = selectedNode.getReachableNodeList();

            for (BasicNode node : reachableNodeList) {
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
        BasicNode                   n1    = ((SuperNode) mEdge.getTargetNode()).getChildNodeById(x);
        BasicNode                   n2    = ((SuperNode) mEdge.getTargetNode()).getChildNodeById(a);
        TPLTuple<String, BasicNode> pair1 = new TPLTuple<String, BasicNode>(x, n1);
        TPLTuple<String, BasicNode> pair2 = new TPLTuple<String, BasicNode>(a, n2);

        mAltStartNodeMap.put(pair1, pair2);
    }
}
