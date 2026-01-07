package de.dfki.vsm.model.sceneflow.chart.edge;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgeGraphics;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.tpl.Tuple;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

//~--- JDK imports ------------------------------------------------------------

/**
 * @author Gregor Mehlmann
 */
public class TimeoutEdge extends AbstractEdge {
    protected long       mTimeout    = Long.MIN_VALUE;
    protected Expression mExpression = null;

    public TimeoutEdge() {}

    // PG: Allow expression for mor flexibility. Consistency check through GUI
    public TimeoutEdge(String target, String source, BasicNode targetNode, BasicNode sourceNode, EdgeGraphics graphics,
                       ArrayList<Command> cmdList, HashMap<Tuple<String, BasicNode>, Tuple<String, BasicNode>> altStartNodeMap,
                       Expression expression) {
        super(target, source, targetNode, sourceNode, graphics, cmdList, altStartNodeMap);
        mExpression = expression;
    }

    public TimeoutEdge(String target, String source, BasicNode targetNode, BasicNode sourceNode, EdgeGraphics graphics,
                       List<Command> cmdList, Map<Tuple<String, BasicNode>, Tuple<String, BasicNode>> altStartNodeMap,
                       long timeout) {
        super(target, source, targetNode, sourceNode, graphics, cmdList, altStartNodeMap);
        mTimeout = timeout;
    }

    public long getTimeout() {
        return mTimeout;
    }

    public void setTimeout(long value) throws NumberFormatException {
        if(value >= 0){
            mTimeout = value;
        }else{
            throw new NumberFormatException("Invalid Time Out Egde Value");
        }
       // mTimeout = value;
    }

    public void setExpression(Expression value) {
        mExpression = value;
    }

    public Expression getExpression() {
        return mExpression;
    }

    public EdgeType getEdgeType() {
        return EdgeType.TimeoutEdge;
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
    public TimeoutEdge getCopy() {
        TimeoutEdge copy = new TimeoutEdge(mTargetUnid, mSourceUnid, mTargetNode, mSourceNode, mGraphics.getCopy(), getCopyOfCmdList(),
                getCopyOfAltStartNodeMap(), mTimeout);
        if (mExpression != null) {
            copy.setExpression(mExpression.getCopy());
        }
        return copy;
    }

    public void writeXML(de.dfki.vsm.util.ios.IOSIndentWriter out) throws XMLWriteError {
        StringBuilder start = new StringBuilder();

        for (Map.Entry<Tuple<String, BasicNode>, Tuple<String, BasicNode>> tplTupleTPLTupleEntry : mAltMap.entrySet()) {
            Map.Entry pairs = tplTupleTPLTupleEntry;
            Tuple<String, BasicNode> startNodeData = (Tuple<String, BasicNode>) pairs.getKey();
            Tuple<String, BasicNode> altStartNodeData = (Tuple<String, BasicNode>) pairs.getValue();

            start.append(startNodeData.getFirst()).append("/").append(altStartNodeData.getFirst()).append(";");
        }

        out.println("<TEdge target=\"" + mTargetUnid + "\" start=\"" + start + "\" timeout=\"" + mTimeout + "\">").push();

        if (mGraphics != null) {
            mGraphics.writeXML(out);
        }

        if (mExpression != null) {
            mExpression.writeXML(out);
        }

        if (!mCmdList.isEmpty()) {
            out.println("<Commands>").push();

            for (Command command : mCmdList) {
                command.writeXML(out);
            }

            out.pop().println("</Commands>");
        }

        out.pop().println("</TEdge>");
    }

    public void parseXML(Element element) throws XMLParseError {
        mTargetUnid  = element.getAttribute("target");
        String timeoutAttr = element.getAttribute("timeout");
        if (timeoutAttr != null && !timeoutAttr.isBlank()) {
            try {
                mTimeout = java.lang.Long.parseLong(timeoutAttr);
            } catch (NumberFormatException ignored) {
                mTimeout = Long.MIN_VALUE;
            }
        } else {
            mTimeout = Long.MIN_VALUE;
        }

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
                    mExpression = Expression.parse(element);
                }
            }
        });
    }
}
