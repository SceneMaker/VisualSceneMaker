package de.dfki.vsm.model.sceneflow;

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.model.sceneflow.graphics.edge.EdgeGraphics;
import de.dfki.vsm.util.tpl.TPLTuple;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.ArrayList;

/**
 * @author Gregor Mehlmann
 */
public abstract class AbstractEdge implements ModelObject {

    protected String mTarget = new String();
    protected String mSource = new String();
    protected BasicNode mTargetNode = null;
    protected BasicNode mSourceNode = null;
    protected EdgeGraphics mGraphics = null;
    protected ArrayList<Command> mCmdList = new ArrayList<Command>();
    protected HashMap<TPLTuple<String, BasicNode>, TPLTuple<String, BasicNode>> mAltStartNodeMap = new HashMap<TPLTuple<String, BasicNode>, TPLTuple<String, BasicNode>>();

    public enum Type {

        CEdge, EEdge, IEdge, PEdge, TEdge, FEdge
    }

    public AbstractEdge() {
    }

    public AbstractEdge(String target, String source, BasicNode targetNode, BasicNode sourceNode, EdgeGraphics graphics,
            ArrayList<Command> cmdList, HashMap<TPLTuple<String, BasicNode>, TPLTuple<String, BasicNode>> altStartNodeMap) {
        mTarget = target;
        mSource = source;
        mTargetNode = targetNode;
        mSourceNode = sourceNode;
        mGraphics = graphics;
        mCmdList = cmdList;
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

    public BasicNode getTargetNode() {
        return mTargetNode;
    }

    public void setTargetNode(BasicNode value) {
        mTargetNode = value;
    }

    public BasicNode getSourceNode() {
        return mSourceNode;
    }

    public void setSourceNode(BasicNode value) {
        mSourceNode = value;
    }

    public EdgeGraphics getGraphics() {
        return mGraphics;
    }

    public void setGraphics(EdgeGraphics value) {
        mGraphics = value;
    }

    public ArrayList<Command> getCmdList() {
        return mCmdList;
    }

    public void setCmdList(ArrayList<Command> value) {
        mCmdList = value;
    }

    public ArrayList<Command> getCopyOfCmdList() {
        ArrayList<Command> copy = new ArrayList<Command>();

        for (Command cmd : mCmdList) {
            copy.add(cmd.getCopy());
        }

        return copy;
    }

    public ArrayList<BasicNode> getAltStartNodeList() {
        ArrayList<BasicNode> altStartNodeList = new ArrayList<BasicNode>();

        for (TPLTuple<String, BasicNode> pair : mAltStartNodeMap.values()) {
            altStartNodeList.add(pair.getSecond());
        }

        return altStartNodeList;
    }

    public HashMap<TPLTuple<String, BasicNode>, TPLTuple<String, BasicNode>> getAltStartNodeMap() {
        return mAltStartNodeMap;
    }

    public void setAltStartNodeMap(HashMap<TPLTuple<String, BasicNode>, TPLTuple<String, BasicNode>> value) {
        mAltStartNodeMap = value;
    }

    // TODO: This is not yet a deep copy
    public HashMap<TPLTuple<String, BasicNode>, TPLTuple<String, BasicNode>> getCopyOfAltStartNodeMap() {
        HashMap<TPLTuple<String, BasicNode>, TPLTuple<String, BasicNode>> copy = new HashMap<TPLTuple<String, BasicNode>, TPLTuple<String, BasicNode>>();
        Iterator it = mAltStartNodeMap.entrySet().iterator();

        while (it.hasNext()) {
            Map.Entry pairs = (Map.Entry) it.next();
            TPLTuple<String, BasicNode> startNodePair = (TPLTuple<String, BasicNode>) pairs.getKey();
            TPLTuple<String, BasicNode> altStartNodePair = (TPLTuple<String, BasicNode>) pairs.getValue();
            TPLTuple<String, BasicNode> startNodePairCopy = new TPLTuple<String, BasicNode>(startNodePair.getFirst(),
                    startNodePair.getSecond());
            TPLTuple<String, BasicNode> altStartNodePairCopy = new TPLTuple<String, BasicNode>(altStartNodePair.getFirst(),
                    altStartNodePair.getSecond());

            copy.put(startNodePairCopy, altStartNodePairCopy);
        }

        return copy;
    }

    // TODO: do this over the list of strings
    public String getAltStartNodesAsString() {
        String result = "";
        Iterator it = mAltStartNodeMap.entrySet().iterator();

        while (it.hasNext()) {
            Map.Entry pairs = (Map.Entry) it.next();
            TPLTuple<String, BasicNode> start = (TPLTuple<String, BasicNode>) pairs.getKey();
            TPLTuple<String, BasicNode> alt = (TPLTuple<String, BasicNode>) pairs.getValue();

            result += start.getFirst() + "/" + alt.getFirst() + ";";
        }

        return result;
    }

    public abstract Type getEdgeType();

    public abstract AbstractEdge getCopy();
}
