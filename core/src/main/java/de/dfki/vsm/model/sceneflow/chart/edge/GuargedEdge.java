package de.dfki.vsm.model.sceneflow.chart.edge;

import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgeGraphics;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.tpl.Tuple;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;

import java.util.List;
import java.util.Map;

/**
 * @author Gregor Mehlmann
 */
public class GuargedEdge extends AbstractEdge {

    protected Expression mCondition = null;

    public GuargedEdge() {
    }

    public GuargedEdge(String target, String source, BasicNode targetNode, BasicNode sourceNode, EdgeGraphics graphics,
                       List<Command> cmdList, Map<Tuple<String, BasicNode>, Tuple<String, BasicNode>> altStartNodeMap,
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

        for (Map.Entry<Tuple<String, BasicNode>, Tuple<String, BasicNode>> tplTupleTPLTupleEntry : mAltMap.entrySet()) {
            Map.Entry pairs = tplTupleTPLTupleEntry;
            Tuple<String, BasicNode> startNodeData = (Tuple<String, BasicNode>) pairs.getKey();
            Tuple<String, BasicNode> altStartNodeData = (Tuple<String, BasicNode>) pairs.getValue();

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
                Tuple<String, BasicNode> startPair = new Tuple<>(startId, null);
                Tuple<String, BasicNode> altStartPair = new Tuple<>(altStartId, null);

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
