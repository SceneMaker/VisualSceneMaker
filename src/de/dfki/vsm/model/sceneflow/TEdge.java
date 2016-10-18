package de.dfki.vsm.model.sceneflow;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.model.sceneflow.graphics.edge.EdgeGraphics;
import de.dfki.vsm.util.tpl.TPLTuple;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.util.ArrayList;

import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * @author Gregor Mehlmann
 */
public class TEdge extends AbstractEdge {
    protected long       mTimeout    = Long.MIN_VALUE;
    protected Expression mExpression = null;

    public TEdge() {}

    // PG: Allow expression for mor flexibility. Consistency check through GUI
    public TEdge(String target, String source, BasicNode targetNode, BasicNode sourceNode, EdgeGraphics graphics,
                 ArrayList<Command> cmdList, HashMap<TPLTuple<String, BasicNode>, TPLTuple<String, BasicNode>> altStartNodeMap,
                 Expression expression) {
        super(target, source, targetNode, sourceNode, graphics, cmdList, altStartNodeMap);
        mExpression = expression;
    }

    public TEdge(String target, String source, BasicNode targetNode, BasicNode sourceNode, EdgeGraphics graphics,
                 ArrayList<Command> cmdList, HashMap<TPLTuple<String, BasicNode>, TPLTuple<String, BasicNode>> altStartNodeMap,
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

    public Type getEdgeType() {
        return Type.TEdge;
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
    public TEdge getCopy() {
        return new TEdge(mTarget, mSource, mTargetNode, mSourceNode, mGraphics.getCopy(), getCopyOfCmdList(),
                         getCopyOfAltStartNodeMap(), mTimeout);
    }

    public void writeXML(de.dfki.vsm.util.ios.IOSIndentWriter out) throws XMLWriteError {
        String   start = "";
        Iterator it    = mAltStartNodeMap.entrySet().iterator();

        while (it.hasNext()) {
            Map.Entry              pairs            = (Map.Entry) it.next();
            TPLTuple<String, BasicNode> startNodeData    = (TPLTuple<String, BasicNode>) pairs.getKey();
            TPLTuple<String, BasicNode> altStartNodeData = (TPLTuple<String, BasicNode>) pairs.getValue();

            start += startNodeData.getFirst() + "/" + altStartNodeData.getFirst() + ";";
        }

        out.println("<TEdge target=\"" + mTarget + "\" start=\"" + start + "\" timeout=\"" + mTimeout + "\">");

        if (mGraphics != null) {
            mGraphics.writeXML(out);
        }

        if (!mCmdList.isEmpty()) {
            out.println("<Commands>").push();

            for (int i = 0; i < mCmdList.size(); i++) {
                mCmdList.get(i).writeXML(out);
            }

            out.pop().println("</Commands>");
        }

        out.println("</TEdge>");
    }

    public void parseXML(Element element) throws XMLParseError {
        mTarget  = element.getAttribute("target");
        mTimeout = java.lang.Long.valueOf(element.getAttribute("timeout"));

        String[] altStartNodes = element.getAttribute("start").split(";");

        for (String idPair : altStartNodes) {
            if (!idPair.isEmpty()) {
                String[]               ids          = idPair.split("/");
                String                 startId      = ids[0];
                String                 altStartId   = ids[1];
                TPLTuple<String, BasicNode> startPair    = new TPLTuple<String, BasicNode>(startId, null);
                TPLTuple<String, BasicNode> altStartPair = new TPLTuple<String, BasicNode>(altStartId, null);

                mAltStartNodeMap.put(startPair, altStartPair);
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
                    throw new XMLParseError(null,
                                            "Cannot parse an element with tag \"" + tag
                                            + "\" into a child of a TEdge!");
                }
            }
        });
    }
}
