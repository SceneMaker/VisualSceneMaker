package de.dfki.vsm.editor.util;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.edge.AbstractEdge;
import de.dfki.vsm.util.tpl.Tuple;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

//~--- JDK imports ------------------------------------------------------------

/**
 * @author Patrick Gebhard
 * @author Gregor Mehlmann
 */
public class AltStartNodeManager {
    public AbstractEdge                                                    mEdge;
    public Map<Tuple<String, BasicNode>, Tuple<String, BasicNode>> mAltStartNodeMap;

    public AltStartNodeManager(AbstractEdge edge) {
        mEdge            = edge;
        mAltStartNodeMap = mEdge.getCopyOfAltStartNodeMap();
    }

    public void saveAltStartNodeMap() {
        mEdge.setAltMap(mAltStartNodeMap);
    }

    public void loadAltStartNodeMap() {
        mAltStartNodeMap = mEdge.getCopyOfAltStartNodeMap();
    }

    public void removeAltStartNode(String id) {
        Tuple<String, BasicNode> pair = null;

        for (Tuple<String, BasicNode> p : mAltStartNodeMap.keySet()) {
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

        ArrayList<BasicNode> substitutableStartNodeList = new ArrayList<>(((SuperNode) mEdge.getTargetNode()).getStartNodeMap().values());

        //
        for (Tuple<String, BasicNode> startNodePair : mAltStartNodeMap.keySet()) {
            if (!startNodePair.getFirst().equals("none")) {
                substitutableStartNodeList.remove(startNodePair.getSecond());
            }
        }

        return substitutableStartNodeList;
    }

    public ArrayList<BasicNode> getValidAltStartNodesFor(String id) {
        ArrayList<BasicNode> validAltStartNodeList = new ArrayList<>();

        // /
        SuperNode targetNode   = (SuperNode) mEdge.getTargetNode();
        BasicNode      selectedNode = targetNode.getChildNodeById(id);

        System.err.println("Selected node=" + selectedNode);

        // /
        if (selectedNode == null) {

            // rwchability map
            HashMap<BasicNode, ArrayList<BasicNode>> reachableNodeMap = new HashMap<>();

            for (BasicNode node : ((SuperNode) mEdge.getTargetNode()).getStartNodeMap().values()) {
                reachableNodeMap.put(node, node.getReachableNodeList());
            }

            for (Tuple<String, BasicNode> p : mAltStartNodeMap.values()) {
                reachableNodeMap.put(p.getSecond(), p.getSecond().getReachableNodeList());
            }

            // ArrayList<Node>
            ArrayList<BasicNode> possililities = ((SuperNode) mEdge.getTargetNode()).getNodeAndSuperNodeList();
            ArrayList<BasicNode> finals        = new ArrayList<>();

            // /
            for (BasicNode node : possililities) {
                System.err.println("looking if " + node.getId() + " is valid");

                boolean  valid = true;

                for (Map.Entry<BasicNode, ArrayList<BasicNode>> basicNodeArrayListEntry : reachableNodeMap.entrySet()) {
                    Map.Entry pairs = basicNodeArrayListEntry;
                    BasicNode n = (BasicNode) pairs.getKey();
                    ArrayList<BasicNode> v = (ArrayList<BasicNode>) pairs.getValue();

                    if (v.contains(node)) {
                        valid = false;
                    }
                }

                if (valid) {
                    finals.add(node);
                }
            }

            /////
            validAltStartNodeList.addAll(finals);

            /////
            for (BasicNode n : finals) {
                ArrayList<BasicNode> reverse = n.getReachableNodeList();

                ////

                for (Map.Entry<Tuple<String, BasicNode>, Tuple<String, BasicNode>> tplTupleTPLTupleEntry : mAltStartNodeMap.entrySet()) {
                    Map.Entry pairs = tplTupleTPLTupleEntry;
                    Tuple<String, BasicNode> p1 = (Tuple<String, BasicNode>) pairs.getKey();
                    Tuple<String, BasicNode> p2 = (Tuple<String, BasicNode>) pairs.getValue();

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
        Tuple<String, BasicNode> pair1 = new Tuple<>(x, n1);
        Tuple<String, BasicNode> pair2 = new Tuple<>(a, n2);

        mAltStartNodeMap.put(pair1, pair2);
    }
}
