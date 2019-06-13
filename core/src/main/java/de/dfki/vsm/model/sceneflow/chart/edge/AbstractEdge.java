package de.dfki.vsm.model.sceneflow.chart.edge;

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgeGraphics;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.util.tpl.Tuple;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Gregor Mehlmann
 */
public abstract class AbstractEdge implements ModelObject {

    protected String mTargetUnid = "";
    protected String mSourceUnid = "";
    protected BasicNode mTargetNode = null;
    protected BasicNode mSourceNode = null;
    protected EdgeGraphics mGraphics = null;
    protected List<Command> mCmdList = new ArrayList<>();
    protected Map<Tuple<String, BasicNode>, Tuple<String, BasicNode>> mAltMap = new HashMap<>();

    // The edge type
    public enum EdgeType {

        GuardedEdge,
        EpsilonEdge,
        InterruptEdge,
        RandomEdge,
        TimeoutEdge,
        ForkingEdge
    }

    public AbstractEdge() {
    }

    public AbstractEdge(
            final String targetUnid,
            final String sourceUnid,
            final BasicNode targetNode,
            final BasicNode sourceNode,
            final EdgeGraphics graphics,
            final List cmdList,
            final Map altMap) {
        mTargetUnid = targetUnid;
        mSourceUnid = sourceUnid;
        mTargetNode = targetNode;
        mSourceNode = sourceNode;
        mGraphics = graphics;
        mCmdList = cmdList;
        mAltMap = altMap;
    }

    public final String getTargetUnid() {
        return mTargetUnid;
    }

    public final void setTargetUnid(final String value) {
        mTargetUnid = value;
    }

    public final String getSourceUnid() {
        return mSourceUnid;
    }

    public final void setSourceUnid(final String value) {
        mSourceUnid = value;
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

    public final List<Command> getCmdList() {
        return mCmdList;
    }

    public final void setCmdList(final List<Command> value) {
        mCmdList = value;
    }

    public final List<Command> getCopyOfCmdList() {
        final ArrayList<Command> copy = new ArrayList<>();
        for (Command cmd : mCmdList) {
            copy.add(cmd.getCopy());
        }
        return copy;
    }

    /*
    public final ArrayList<BasicNode> getAltList() {
        final ArrayList<BasicNode> altList = new ArrayList();
        for (Tuple<String, BasicNode> pair : mAltMap.values()) {
            altList.add(pair.getSecond());
        }
        return altList;
    }
     */
    public final Map<Tuple<String, BasicNode>, Tuple<String, BasicNode>> getAltMap() {
        return mAltMap;
    }

    public final void setAltMap(final Map value) {
        mAltMap = value;
    }

    public Map<Tuple<String, BasicNode>, Tuple<String, BasicNode>> getCopyOfAltStartNodeMap() {
        Map<Tuple<String, BasicNode>, Tuple<String, BasicNode>> copy = new HashMap<>();

        mAltMap.forEach((startNodePair, altStartNodePair) -> {
            var startNodePairCopy = new Tuple<>(
                    startNodePair.getFirst(),
                    startNodePair.getSecond().getCopy()
            );
            var altStartNodePairCopy = new Tuple<>(
                    altStartNodePair.getFirst(),
                    altStartNodePair.getSecond().getCopy()
            );

            copy.put(startNodePairCopy, altStartNodePairCopy);
        });
        return copy;
    }

    // TODO: do this over the list of strings
    public String getAltStartNodesAsString() {
        StringBuilder result = new StringBuilder();

        for (Map.Entry<Tuple<String, BasicNode>, Tuple<String, BasicNode>> pairs : mAltMap.entrySet()) {
            var start = pairs.getKey();
            var alt = pairs.getValue();

            result.append(start.getFirst())
                    .append("/")
                    .append(alt.getFirst())
                    .append(";");
        }

        return result.toString();
    }

    public abstract EdgeType getEdgeType();

    @Override
    public abstract AbstractEdge getCopy();
}
