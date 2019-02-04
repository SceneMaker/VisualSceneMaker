package de.dfki.vsm.model.sceneflow.chart.edge;

import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgeGraphics;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.tpl.TPLTuple;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.util.ArrayList;
import org.w3c.dom.Element;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * @author Gregor Mehlmann
 */
public class GuargedEdge extends AbstractEdge {

    protected Expression mCondition = null;

    public GuargedEdge() {
    }

    public GuargedEdge(String target, String source, BasicNode targetNode, BasicNode sourceNode, EdgeGraphics graphics,
            ArrayList<Command> cmdList, HashMap<TPLTuple<String, BasicNode>, TPLTuple<String, BasicNode>> altStartNodeMap,
            Expression condition) {
        super(target, source, targetNode, sourceNode, graphics, cmdList, altStartNodeMap);
        mCondition = condition;
    }

    public Expression getCondition() {
        return mCondition;
    }

    public void setCondition(Expression value) {
        mCondition = value;
    }

    @Override
    public EdgeType getEdgeType() {
        return EdgeType.GuardedEdge;
    }

    public String getAbstractSyntax() {
        return null;
    }

    public String getConcreteSyntax() {
        return null;
    }

    public String getFormattedSyntax() {
        return null;
    }

    // TODO:
    public GuargedEdge getCopy() {
        return new GuargedEdge(mTargetUnid, mSourceUnid, mTargetNode, mSourceNode, mGraphics.getCopy(), getCopyOfCmdList(),
                getCopyOfAltStartNodeMap(), mCondition.getCopy());
    }

    public void writeXML(de.dfki.vsm.util.ios.IOSIndentWriter out) throws XMLWriteError {
        StringBuilder start = new StringBuilder();

        for (Map.Entry<TPLTuple<String, BasicNode>, TPLTuple<String, BasicNode>> tplTupleTPLTupleEntry : mAltMap.entrySet()) {
            Map.Entry pairs = tplTupleTPLTupleEntry;
            TPLTuple<String, BasicNode> startNodeData = (TPLTuple<String, BasicNode>) pairs.getKey();
            TPLTuple<String, BasicNode> altStartNodeData = (TPLTuple<String, BasicNode>) pairs.getValue();

            start.append(startNodeData.getFirst()).append("/").append(altStartNodeData.getFirst()).append(";");
        }

        out.println("<CEdge target=\"" + mTargetUnid + "\" start=\"" + start + "\">").push();

        if (mGraphics != null) {
            mGraphics.writeXML(out);
        }

        if (mCondition != null) {
            mCondition.writeXML(out);
        }

        if (!mCmdList.isEmpty()) {
            out.println("<Commands>").push();

            for (Command command : mCmdList) {
                command.writeXML(out);
            }

            out.pop().println("</Commands>");
        }

        out.pop().println("</CEdge>");
    }

    public void parseXML(Element element) throws XMLParseError {
        mTargetUnid = element.getAttribute("target");

        String[] altStartNodes = element.getAttribute("start").split(";");

        for (String idPair : altStartNodes) {
            if (!idPair.isEmpty()) {
                String[] ids = idPair.split("/");
                String startId = ids[0];
                String altStartId = ids[1];
                TPLTuple<String, BasicNode> startPair = new TPLTuple<>(startId, null);
                TPLTuple<String, BasicNode> altStartPair = new TPLTuple<>(altStartId, null);

                mAltMap.put(startPair, altStartPair);
            }
        }

        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                java.lang.String tag = element.getTagName();

                if (tag.equals("Graphics")) {
                    mGraphics = new EdgeGraphics();
                    mGraphics.parseXML(element);
                } else if (tag.equals("Commands")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        public void run(Element element) throws XMLParseError {
                            mCmdList.add(Command.parse(element));
                        }
                    });
                } else {
                    mCondition = Expression.parse(element);
                }
            }
        });
    }
}
