package de.dfki.vsm.model.sceneflow;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.model.sceneflow.graphics.edge.Graphics;
import de.dfki.vsm.util.tpl.TPLTuple;

//~--- JDK imports ------------------------------------------------------------

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

/**
 * @author Not me
 */
public abstract class Edge extends Syntax {
    protected String                                                  mTarget          = new String();
    protected String                                                  mSource          = new String();
    protected Node                                                    mTargetNode      = null;
    protected Node                                                    mSourceNode      = null;
    protected Graphics                                                mGraphics        = null;
    protected Vector<Command>                                         mCmdList         = new Vector<Command>();
    protected HashMap<TPLTuple<String, Node>, TPLTuple<String, Node>> mAltStartNodeMap = new HashMap<TPLTuple<String,
                                                                                             Node>, TPLTuple<String,
                                                                                                 Node>>();

    public enum Type {
        CEdge, EEdge, IEdge, PEdge, TEdge, FEdge
    }

    public Edge() {}

    public Edge(String target, String source, Node targetNode, Node sourceNode, Graphics graphics,
                Vector<Command> cmdList, HashMap<TPLTuple<String, Node>, TPLTuple<String, Node>> altStartNodeMap) {
        mTarget          = target;
        mSource          = source;
        mTargetNode      = targetNode;
        mSourceNode      = sourceNode;
        mGraphics        = graphics;
        mCmdList         = cmdList;
        mAltStartNodeMap = altStartNodeMap;
    }

    public String getTarget() {
        return mTarget;
    }

    public void setTarget(String value) {
        mTarget = value;
    }

    public String getSource() {
        return mSource;
    }

    public void setSource(String value) {
        mSource = value;
    }

    public Node getTargetNode() {
        return mTargetNode;
    }

    public void setTargetNode(Node value) {
        mTargetNode = value;
    }

    public Node getSourceNode() {
        return mSourceNode;
    }

    public void setSourceNode(Node value) {
        mSourceNode = value;
    }

    public Graphics getGraphics() {
        return mGraphics;
    }

    public void setGraphics(Graphics value) {
        mGraphics = value;
    }

    public Vector<Command> getCmdList() {
        return mCmdList;
    }

    public void setCmdList(Vector<Command> value) {
        mCmdList = value;
    }

    public Vector<Command> getCopyOfCmdList() {
        Vector<Command> copy = new Vector<Command>();

        for (Command cmd : mCmdList) {
            copy.add(cmd.getCopy());
        }

        return copy;
    }

    public Vector<Node> getAltStartNodeList() {
        Vector<Node> altStartNodeList = new Vector<Node>();

        for (TPLTuple<String, Node> pair : mAltStartNodeMap.values()) {
            altStartNodeList.add(pair.getSecond());
        }

        return altStartNodeList;
    }

    public HashMap<TPLTuple<String, Node>, TPLTuple<String, Node>> getAltStartNodeMap() {
        return mAltStartNodeMap;
    }

    public void setAltStartNodeMap(HashMap<TPLTuple<String, Node>, TPLTuple<String, Node>> value) {
        mAltStartNodeMap = value;
    }

    // TODO: This is not yet a deep copy
    public HashMap<TPLTuple<String, Node>, TPLTuple<String, Node>> getCopyOfAltStartNodeMap() {
        HashMap<TPLTuple<String, Node>, TPLTuple<String, Node>> copy = new HashMap<TPLTuple<String, Node>,
                                                                           TPLTuple<String, Node>>();
        Iterator it = mAltStartNodeMap.entrySet().iterator();

        while (it.hasNext()) {
            Map.Entry              pairs             = (Map.Entry) it.next();
            TPLTuple<String, Node> startNodePair     = (TPLTuple<String, Node>) pairs.getKey();
            TPLTuple<String, Node> altStartNodePair  = (TPLTuple<String, Node>) pairs.getValue();
            TPLTuple<String, Node> startNodePairCopy = new TPLTuple<String, Node>(startNodePair.getFirst(),
                                                           startNodePair.getSecond());
            TPLTuple<String, Node> altStartNodePairCopy = new TPLTuple<String, Node>(altStartNodePair.getFirst(),
                                                              altStartNodePair.getSecond());

            copy.put(startNodePairCopy, altStartNodePairCopy);
        }

        return copy;
    }

    // TODO: do this over the list of strings
    public String getAltStartNodesAsString() {
        String   result = "";
        Iterator it     = mAltStartNodeMap.entrySet().iterator();

        while (it.hasNext()) {
            Map.Entry              pairs = (Map.Entry) it.next();
            TPLTuple<String, Node> start = (TPLTuple<String, Node>) pairs.getKey();
            TPLTuple<String, Node> alt   = (TPLTuple<String, Node>) pairs.getValue();

            result += start.getFirst() + "/" + alt.getFirst() + ";";
        }

        return result;
    }

    public abstract Type getEdgeType();

    public abstract Edge getCopy();
}
