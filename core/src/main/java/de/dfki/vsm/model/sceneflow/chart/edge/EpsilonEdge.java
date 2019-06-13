package de.dfki.vsm.model.sceneflow.chart.edge;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgeGraphics;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.tpl.Tuple;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;

import java.util.List;
import java.util.Map;

//~--- JDK imports ------------------------------------------------------------

/**
 * @author Gregor Mehlmann
 */
public class EpsilonEdge extends AbstractEdge {
    public EpsilonEdge() {}

    public EpsilonEdge(String target, String source, BasicNode targetNode, BasicNode sourceNode, EdgeGraphics graphics,
                       List<Command> cmdList, Map<Tuple<String, BasicNode>, Tuple<String, BasicNode>> altStartNodeMap) {
        super(target, source, targetNode, sourceNode, graphics, cmdList, altStartNodeMap);
    }

    public EdgeType getEdgeType() {
        return EdgeType.EpsilonEdge;
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
    public EpsilonEdge getCopy() {
        return new EpsilonEdge(mTargetUnid, mSourceUnid, mTargetNode, mSourceNode, mGraphics.getCopy(), getCopyOfCmdList(),
                         getCopyOfAltStartNodeMap());
    }

    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        StringBuilder start = new StringBuilder();

        for (Map.Entry<Tuple<String, BasicNode>, Tuple<String, BasicNode>> tplTupleTPLTupleEntry : mAltMap.entrySet()) {
            Map.Entry pairs = tplTupleTPLTupleEntry;
            Tuple<String, BasicNode> startNodeData = (Tuple<String, BasicNode>) pairs.getKey();
            Tuple<String, BasicNode> altStartNodeData = (Tuple<String, BasicNode>) pairs.getValue();

            start.append(startNodeData.getFirst()).append("/").append(altStartNodeData.getFirst()).append(";");
        }

        out.println("<EEdge target=\"" + mTargetUnid + "\" start =\"" + start + "\">");

        if (mGraphics != null) {
            mGraphics.writeXML(out);
        }

        if (!mCmdList.isEmpty()) {
            out.println("<Commands>").push();

            for (Command command : mCmdList) {
                command.writeXML(out);
            }

            out.pop().println("</Commands>");
        }

        out.println("</EEdge>");
    }

    public void parseXML(Element element) throws XMLParseError {
        mTargetUnid = element.getAttribute("target");

        String[] altStartNodes = element.getAttribute("start").split(";");

        for (String idPair : altStartNodes) {
            if (!idPair.isEmpty()) {
                String[]               ids          = idPair.split("/");
                String                 startId      = ids[0];
                String                 altStartId   = ids[1];
                Tuple<String, BasicNode> startPair = new Tuple<>(startId, null);
                Tuple<String, BasicNode> altStartPair = new Tuple<>(altStartId, null);

                mAltMap.put(startPair, altStartPair);
            }
        }

        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                String tag = element.getTagName();

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
                    throw new XMLParseError(null,
                                            "Cannot parse an element with tag \"" + tag
                                            + "\" into a child of an EEdge!");
                }
            }
        });
    }
}
