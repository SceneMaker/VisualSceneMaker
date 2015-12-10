package de.dfki.vsm.model.sceneflow.diagram.edges;

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.model.sceneflow.command.AbstractCommand;
import de.dfki.vsm.model.sceneflow.diagram.BasicNode;
import de.dfki.vsm.model.sceneflow.diagram.graphics.edge.EdgeGraphics;
import java.util.ArrayList;

/**
 * @author Gregor Mehlmann
 */
public abstract class AbstractEdge /*extends SyntaxObject */ implements ModelObject{

    // Target and source node names
    protected String mTarget = null;
    protected String mSource = null;
    // The target and source nodes
    protected BasicNode mTargetNode = null;
    protected BasicNode mSourceNode = null;
    // The graphics data of the edge
    protected EdgeGraphics mGraphics = null;
    // The list of edge commands
    protected ArrayList<AbstractCommand> mCommandList = new ArrayList();
    //
    //protected HashMap<TPLTuple<String, Node>, TPLTuple<String, Node>> mAltStartNodeMap = new HashMap();

    public enum Type {

        CEdge, EEdge, IEdge, PEdge, TEdge, FEdge
    }

    public AbstractEdge() {
    }

    public AbstractEdge(
            final String target,
            final String source,
            final BasicNode targetNode,
            final BasicNode sourceNode,
            final EdgeGraphics graphics,
            final ArrayList cmdList) {
        mTarget = target;
        mSource = source;
        mTargetNode = targetNode;
        mSourceNode = sourceNode;
        mGraphics = graphics;
        mCommandList = cmdList;
//        mAltStartNodeMap = altMap;
    }

    public final String getTarget() {
        return mTarget;
    }

    public final void setTarget(final String value) {
        mTarget = value;
    }

    public final String getSource() {
        return mSource;
    }

    public final void setSource(final String value) {
        mSource = value;
    }

    public final BasicNode getTargetNode() {
        return mTargetNode;
    }

    public final void setTargetNode(final BasicNode value) {
        mTargetNode = value;
    }

    public final BasicNode getSourceNode() {
        return mSourceNode;
    }

    public final void setSourceNode(final BasicNode value) {
        mSourceNode = value;
    }

    public final EdgeGraphics getGraphics() {
        return mGraphics;
    }

    public final void setGraphics(final EdgeGraphics value) {
        mGraphics = value;
    }

    public final ArrayList<AbstractCommand> getCmdList() {
        return mCommandList;
    }

    public final void setCmdList(final ArrayList value) {
        mCommandList = value;
    }

    public final ArrayList<AbstractCommand> getCopyOfCmdList() {
        final ArrayList<AbstractCommand> copy = new ArrayList();
        for (final AbstractCommand command : mCommandList) {
            copy.add(command.getCopy());
        }
        return copy;
    }

//    public final ArrayList<Node> getAltStartNodeList() {
//        final ArrayList<Node> altStartNodeList = new ArrayList();
//        for (final TPLTuple<String, Node> pair : mAltStartNodeMap.values()) {
//            altStartNodeList.add(pair.getSecond());
//        }
//        return altStartNodeList;
//    }
//
//    public final HashMap<TPLTuple<String, Node>, TPLTuple<String, Node>> getAltStartNodeMap() {
//        return mAltStartNodeMap;
//    }
//
//    public final void setAltStartNodeMap(final HashMap value) {
//        mAltStartNodeMap = value;
//    }
//
//    // TODO: This is not yet a deep copy
//    public final HashMap<TPLTuple<String, Node>, TPLTuple<String, Node>> getCopyOfAltStartNodeMap() {
//        HashMap<TPLTuple<String, Node>, TPLTuple<String, Node>> copy = new HashMap<TPLTuple<String, Node>, TPLTuple<String, Node>>();
//        Iterator it = mAltStartNodeMap.entrySet().iterator();
//
//        while (it.hasNext()) {
//            Map.Entry pairs = (Map.Entry) it.next();
//            TPLTuple<String, Node> startNodePair = (TPLTuple<String, Node>) pairs.getKey();
//            TPLTuple<String, Node> altStartNodePair = (TPLTuple<String, Node>) pairs.getValue();
//            TPLTuple<String, Node> startNodePairCopy = new TPLTuple<String, Node>(startNodePair.getFirst(),
//                    startNodePair.getSecond());
//            TPLTuple<String, Node> altStartNodePairCopy = new TPLTuple<String, Node>(altStartNodePair.getFirst(),
//                    altStartNodePair.getSecond());
//
//            copy.put(startNodePairCopy, altStartNodePairCopy);
//        }
//
//        return copy;
//    }
//    // TODO: do this over the list of strings
//    public final String getAltStartNodesAsString() {
//        String result = "";
//        Iterator it = mAltStartNodeMap.entrySet().iterator();
//
//        while (it.hasNext()) {
//            Map.Entry pairs = (Map.Entry) it.next();
//            TPLTuple<String, Node> start = (TPLTuple<String, Node>) pairs.getKey();
//            TPLTuple<String, Node> alt = (TPLTuple<String, Node>) pairs.getValue();
//
//            result += start.getFirst() + "/" + alt.getFirst() + ";";
//        }
//
//        return result;
//    }
    public abstract Type getEdgeType();

    @Override
    public abstract AbstractEdge getCopy();
}
